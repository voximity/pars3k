module Pars3k
	extend self
	NON_EXPRESSION_TYPES = ["Assign", "TypeNode", "Splat", "Union", "UninitializedVar", "TypeDeclaration", "Generic", "ClassDef", "Def", "VisibilityModifier", "MultiAssign"]

	macro do_parse(body)
		{% if NON_EXPRESSION_TYPES.includes? body[body.size - 1].class_name %}
			{{body[body.size - 1].raise "expected last operation in monad to be an expression, got a '#{body[body.size - 1].class_name}'"}}
		{% end %}
		({{body[0].args[0]}}).sequence do |{{body[0].receiver}}|
		{% for i in 1...body.size - 1 %}
			{% if body[i].class_name == "Assign" %}
				{{body[i].target}} = {{body[i].value}}
			{% else %}
				{% if body[i].class_name == "Call" && body[i].name == "<=" %}
					({{body[i].args[0]}}).sequence do |{{body[i].receiver}}|
				{% elsif NON_EXPRESSION_TYPES.includes? body[i].class_name %}
					{{body[i].raise "expected operation '<=' or '=', got '#{body[i].name}'"}}
				{% else %}
					{{body[i]}}
				{% end %}
			{% end %}
		{% end %}
		{{body[body.size - 1]}}
		{% for i in 1...body.size - 1 %}
			{% if body[i].class_name == "Call" && body[i].name == "<=" %}
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

		def debug(text : String)
			puts parsing
			puts "-" * position + "|"
			puts "#{text.upcase}: #{position}"
		end
	end

	# A struct containing information about a parse error.
	struct ParseError
		getter context
		getter message

		def initialize(@message : String, @context : ParseContext)
		end

		def to_s
			"(#{@context.parsing}:#{@context.position}) #{@message}"
		end

		def to_s(io)
			io << to_s
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

	class Parse
		# Creates a parser that always returns the same value of T.
		def self.constant(value : T) : Parser(T) forall T
			Parser(T).const value
		end

		# Creates a parser that parses a specific string.
		def self.char(char : Char)
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
		def self.string(string : String) : Parser(String)
			if string.size == 0
				constant ""
			elsif string.size == 1
				(char string[0]).transform { |c| c.to_s }
			else
				parser = char string[0]
				string[1...string.size].chars.each do |char|
					parser += char char
				end
				parser.transform { |_| string }
			end
		end

		# Creates a parser that parses a specific set of characters, given the character is in the string.
		def self.one_char_of(string : String) : Parser(Char)
			parser = char string[0]
			(1...string.size).each do |index|
				parser |= char string[index]
			end
			parser
		end

		# Creates a parser that parses everything but a specific set of characters
		def self.no_char_of(string : String) : Parser(Char)
			Parser(Char).new do |context|
				if context.position >= context.parsing.size
					ParseResult(Char).error "expected none of '#{string}', input ended", context
				elsif string.includes? context.parsing[context.position]
					ParseResult(Char).error "expected none of '#{string}', got #{context.parsing[context.position]}", context
				else
					ParseResult(Char).new context.parsing[context.position], context.next
				end
			end
		end

		# Creates a parser that allows repetition of a specific parser consistently, until it is no longer parsed successfully.
		def self.many_of(parser : Parser(T)) : Parser(Array(T)) forall T
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

		# Like `many_of`, but requires at least one parse result to succeed.
		def self.one_or_more_of(parser : Parser(T)) : Parser(Array(T)) forall T
			Parser(Array(T)).new do |context|
				result = parser.block.call context
				if result.errored
					ParseResult(Array(T)).error result.definite_error
				else
					chars = [result.definite_value]
					new_parser = many_of parser
					new_result = new_parser.block.call result.context
					new_result.definite_value.each do |char|
						chars << char
					end
					ParseResult(Array(T)).new chars, new_result.context
				end
			end
		end

		def self.some_of(parser : Parser(T), range : Range) : Parser(Array(T)) forall T
			max = range.end - (range.excludes_end? ? 1 : 0)
			Parser(Array(T)).new do |ctx|
				count = 0
				result = parser.block.call ctx
				if result.errored && !range.includes? count
					next ParseResult(Array(T)).error result.definite_error
				end
				results = [] of T
				context = ctx
				count += 1
				while !result.errored
					context = result.context
					results << result.definite_value
					result = parser.block.call context
					count += 1
					if count > max
						count = max
						break
					end
				end
				unless range.includes? count
					next ParseResult(Array(T)).error "expected #{range} parses, got #{count} parses", context
				end
				ParseResult(Array(T)).new results, result.context
			end
		end

		def self.some_of(parser : Parser(T), count : Int32) : Parser(Array(T)) forall T
			some_of(parser, count..count)
		end

		def self.one_of(parser : Parser(T)) : Parser(Array(T)) forall T
			some_of parser, ..1
		end

		def self.one_of?(parser : Parser(T)) : Parser(T?) forall T
			Parser(T?).new do |context|
				result = parser.block.call context
				if result.errored
					ParseResult(T?).new(nil, result.context)
				else
					ParseResult(T?).new(result.definite_value.as T, result.context)
				end
			end
		end

		def self.if_not_nil?(parser : Parser(T), value : B) : Parser(T?) forall T, B
			if value.nil?
				Parser(T?).new { |context| ParseResult(T?).new(nil, context) }
			else
				Parser(T?).new do |context|
					result = parser.block.call context
					if result.errored
						ParseResult(T?).error result.definite_error
					else
						ParseResult(T?).new result.definite_value.as T, result.context
					end
				end
			end
		end

		# Creates a parser that parses a delimited list of items parsed by `parser`, and delimited by parser `delimiter`.
		# Useful for when you want to extract a list of parsable items by, say, commas.
		def self.delimited_list(parser : Parser(A), delimiter : Parser(B)) : Parser(Array(A)) forall A, B
			Parser(Array(A)).new do |ctx|
				result = parser.block.call ctx
				if result.errored
					next ParseResult(Array(A)).error result.definite_error
				end
				results = [result.definite_value] of A
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
				ParseResult(Array(A)).new results, delimiter_result.context
			end
		end

		def self.join(parser : Parser(Array(Char))) : Parser(String)
			parser.transform &.reduce "" { |v, c| v + c }
		end

		def self.alphabet_lower
			one_char_of "abcdefghijklmnopqrstuvwxyz"
		end

		def self.alphabet_upper
			one_char_of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		end

		def self.alphabet
			alphabet_lower | alphabet_upper
		end

		def self.word
			(one_or_more_of alphabet).transform { |c| concatenate c }
		end

		def self.digit
			one_char_of "0123456789"
		end

		def self.int
			(one_or_more_of digit).transform { |c| (concatenate c).to_i }
		end

		def self.float
			do_parse({
				whole <= (join one_or_more_of digit),
				_ <= (one_of? char '.'),
				decimal <= (join many_of digit),
				constant "#{whole}#{decimal.size == 0 ? ".0" : "." + decimal}".to_f
			})
		end
	end

	# Parser(T) is a parser with return type T.
	class Parser(T)
		# Parser.const(T) returns a parser that always succeeds with value of type T.
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

		# Given `A / B`, creates a new parser that succeeds when
		# A succeeds or B succeeds. Checks A first, doesn't check B
		# if A succeeds. Ignores type differences, gives union type.
		def /(other : Parser(B)) : Parser(T | B) forall B
			Parser(T | B).new do |context|
				result = @block.call context
				if result.errored
					new_result = other.block.call result.context
					if new_result.errored
						ParseResult(T | B).error new_result.definite_error
					else
						ParseResult(T | B).new new_result.definite_value, new_result.context
					end
				else
					ParseResult(T | B).new result.definite_value, result.context
				end
			end
		end
	end

	def concatenate(chars : Array(Char))
		chars.reduce "" { |v, c| v + c }
	end
end

