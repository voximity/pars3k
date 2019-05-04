module Pars3k

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

	struct ParseError
		getter context
		getter message

		def initialize(@message : String, @context : ParseContext)
		end

		def to_s
			@message
		end
	end

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

	class Parser(T)
		def self.const(value : T)
			Parser(T).new { |ctx| ParseResult(T).new value, ctx }
		end

		getter block

		def initialize(&block : ParseContext -> ParseResult(T))
			@block = block
		end

		def parse(input : String) : (T | ParseError)
			context = ParseContext.new input
			result = @block.call context
			result.value
		end

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

		def +(other : Parser(B)) : Parser(B) forall B
			sequence { |_| other }
		end

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

	def parse_one_char_of(string : String) : Parser(Char)
		parser = parse_char string[0]
		(1...string.size).each do |index|
			parser |= parse_char string[index]
		end
		parser
	end

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
end

