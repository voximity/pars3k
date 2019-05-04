module Pars3k
	NON_EXPRESSION_TYPES = ["Assign", "TypeNode", "Splat", "Union", "UninitializedVar", "TypeDeclaration", "Generic", "ClassDef", "Def", "VisibilityModifier", "MultiAssign"]

	macro parse_monad(body)
		{% if NON_EXPRESSION_TYPES.includes? body[body.size - 1].class_name %}
			{{body[body.size - 1].raise "expected last operation in monad to be an expression, got a '#{body[body.size - 1].class_name}'"}}
		{% end %}
		{{body[0].args[0]}}.sequence do |{{body[0].receiver}}|
		{% for i in 1...body.size - 1 %}
			{% if body[i].class_name == "Assign" %}
				{{body[i].target}} = {{body[i].value}}
			{% else %}
				{% if body[i].class_name == "Call" && body[i].name == "<=" %}
					{{body[i].args[0]}}.sequence do |{{body[i].receiver}}|
				{% else %}
					{{body[i].raise "expected operation '<=' or '=', got '#{body[i].name}'"}}
				{% end %}
			{% end %}
		{% end %}
		{{body[body.size - 1]}}
		{% for i in 1...body.size - 1 %}
			{% if body[i].class_name == "Call" %}
				end
			{% end %}
		{% end %}
		end
	end

	# A struct containing information about the current Parser's context.
	# Used to chain Parsers together and retain input position.
	struct ParseContext
		getter parsing
		getter position

		def initialize(@parsing : String, @position : Int32 = 0)
		end

		def next(offset = 1)
			ParseContext.new(@parsing, @position + offset)
		end

		def set_position(pos)
			@position = pos
		end
	end

	# A struct containing information about a parse error.
	struct ParseError
		getter context
		getter message

		def initialize(@message : String, @context : ParseContext)
		end

		def to_s
			@message
		end
	end

	# ParseResult(T) is a result of a parsed Parser with return type T.
	# If the parse errored, then `ParseResult(T)#errored` will be true.
	# Otherwise, you can get a value of type `(T | ParseError)` with `ParseResult(T).value`.
	# If you are absolutely positive the parse did NOT error (e.g. `!ParseResult(T).errored`),
	# then you can acquire the value of type `T` with `ParseResult(T).definite_value`.
	struct ParseResult(T)
		def self.error(e : ParseError)
			inst = ParseResult(T).allocate
			inst.initialize_as_error e
			inst
		end
		def self.error(message : String, context : ParseContext)
			ParseResult(T).error ParseError.new message, context
		end

		@errored = uninitialized Bool
		@error = uninitialized ParseError
		@context = uninitialized ParseContext

		getter errored
		getter context

		def initialize(@value : T, @context)
			@errored = false
		end

		def initialize_as_error(e : ParseError)
			@errored = true
			@error = e
			@context = e.context
		end

		def set_context_position(pos)
			@context.set_position pos
		end

		def error
			errored ? @error : Nil
		end

		def value
			errored ? @error : @value
		end

		def definite_value
			@value
		end

		def definite_error
			@error
		end
	end

	# Parser(T) is a parser with return type T.
	class Parser(T)
		def self.const(value : T)
			Parser(T).new { |ctx| ParseResult(T).new value, ctx }
		end

		getter block

		def initialize(&block : ParseContext -> ParseResult(T))
			@block = block
		end

		# Parses the input string `input` given the parser's logic provided
		# by its block at definition.
		def parse(input : String) : (T | ParseError)
			context = ParseContext.new input
			result = @block.call context
			result.value
		end

		# Transforms the result of the parser such that, when the parser is
		# parsed, the output value becomes a different value.
		# For example, if you took a Parser(Char) and wanted to transform
		# it to a Parser(String) by `Char.to_s`, then you could use
		# `Parser(Char).transform { |char| char.to_s }`. It is similar to
		# a map method on arrays from other languages.
		def transform(&new_block : T -> B) : Parser(B) forall B
			Parser(B).new do |context|
				result = @block.call context
				if result.errored
					ParseResult(B).error result.definite_error
				else
					ParseResult(B).new new_block.call(result.definite_value), result.context
				end
			end
		end

		# Sequences the current parser with another parser.
		# Expects a block that receives the result of the current parser
		# and returns a new parser of any type, presumably dynamically created.
		def sequence(&new_block : T -> Parser(B)) : Parser(B) forall B
			Parser(B).new do |context|
				result = @block.call context
				if result.errored
					ParseResult(B).error result.definite_error
				else
					next_parser = new_block.call result.definite_value
					next_parser.block.call result.context
				end
			end
		end

		# Sequences the current parser with another parser given they are the same type.
		def +(other : Parser(B)) : Parser(B) forall B
			sequence { |_| other }
		end

		# Sequences the current parser with another parser, and disregards the other parser's result,
		# but ensures the two succeed.
		def <<(other : Parser(B)) : Parser(T) forall B
			Parser(T).new do |context|
				result = @block.call context
				if result.errored
					result
				else
					new_result = other.block.call result.context
					if new_result.errored
						ParseResult(T).error new_result.definite_error
					else
						result.set_context_position new_result.context.position
						result
					end
				end
			end
		end

		# Sequences the current parser with another parser, and disregards the original parser's result,
		# but ensures the two succeed.
		def >>(other : Parser(B)) : Parser(B) forall B
			Parser(B).new do |context|
				result = @block.call context
				if result.errored
					ParseResult(B).error result.definite_error
				else
					new_result = other.block.call result.context
					new_result
				end
			end
		end

		# Given `A | B`, creates a new parser that succeeds when
		# A succeeds or B succeeds. Checks A first, doesn't check B
		# if A succeeds.
		def |(other : Parser(T)) : Parser(T)
			Parser(T).new do |context|
				result = @block.call context
				if result.errored
					other.block.call context
				else
					result
				end
			end
		end
	end


	# PREDEFINED PARSERS

	# Creates a parser that parses a specific character.
	def parse_char(char : Char) : Parser(Char)
		Parser(Char).new do |context|
			if context.position >= context.parsing.size
				ParseResult(Char).error "expected '#{char}', input ended", context
			elsif context.parsing[context.position] == char
				ParseResult(Char).new char, context.next
			else
				ParseResult(Char).error "expected '#{char}', got '#{context.parsing[context.position]}'", context
			end
		end
	end

	# Creates a parser that parses a specific string.
	def parse_string(string : String) : Parser(String)
		if string.size == 0
			Parser(String).const ""
		elsif string.size == 1
			(parse_char string[0]).transform { |c| c.to_s }
		else
			parser = parse_char string[0]
			string[1...string.size].chars.each do |char|
				parser += parse_char char
			end
			parser.transform { |_| string }
		end
	end

	# Creates a parser that parses a specific set of characters, given the character is in the string.
	def parse_one_char_of(string : String) : Parser(Char)
		parser = parse_char string[0]
		(1...string.size).each do |index|
			parser |= parse_char string[index]
		end
		parser
	end

	# Creates a parser that allows repetition of a specific parser consistently, until it is no longer parsed successfully.
	def parse_many_of(parser : Parser(T)) : Parser(Array(T)) forall T
		Parser(Array(T)).new do |ctx|
			result = parser.block.call ctx
			results = [] of T
			context = ctx
			count = 1
			while !result.errored
				context = result.context
				results << result.definite_value
				result = parser.block.call context
				count += 1
			end
			ParseResult(Array(T)).new results, context
		end
	end

	# Like `parse_many_of`, but requires at least one parse result to succeed.
	def parse_one_or_more_of(parser : Parser(T)) : Parser(Array(T)) forall T
		Parser(Array(T)).new do |context|
			result = parser.block.call context
			if result.errored
				ParseResult(Array(T)).error result.definite_error
			else
				chars = [result.definite_value]
				new_parser = parse_many_of parser
				new_result = new_parser.block.call result.context
				new_result.definite_value.each do |char|
					chars << char
				end
				ParseResult(Array(T)).new chars, new_result.context
			end
		end
	end

	# Creates a parser that parses a delimited list of items parsed by `parser`, and delimited by parser `delimiter`.
	# Useful for when you want to extract a list of parsable items by, say, commas.
	def parse_delimited_list(parser : Parser(A), delimiter : Parser(B)) : Parser(Array(A)) forall A, B
		Parser(Array(A)).new do |ctx|
			result = parser.block.call ctx
			if result.errored
				next ParseResult(Array(A)).error result.definite_error
			end
			results = [result.definite_value]
			context = ctx
			count = 1
			delimiter_result = delimiter.block.call result.context
			while !delimiter_result.errored
				result = parser.block.call delimiter_result.context
				if result.errored
					break
				end
				context = result.context
				results << result.definite_value
				delimiter_result = delimiter.block.call context
			end
			ParseResult(Array(A)).new results, context
		end
	end

	# Helpful default parsers

	def concatenate(chars : Array(Chars))
		r = ""
		chars.each { |c| r += c }
		r
	end

	def parse_alphabet_lower
		parse_one_char_of "abcdefghijklmnopqrstuvwxyz"
	end

	def parse_alphabet_upper
		parse_one_char_of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	end

	def parse_alphabet
		parse_alphabet_lower | parse_alphabet_upper
	end

	def parse_word
		(parse_one_or_more_of parse_alphabet).transform { |c| concatenate c }
	end
end

