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
		# ```cr
		# p = Parse.constant "abc"
		# puts p.parse "abc" #=> abc
		# puts p.parse "123" #=> abc
		# puts p.parse "" #=> abc
		# ```
		#
		# `Parse.constant(value : T)` creates a `Parser(T)` that always succeeds with `value`.
		# This method is used to create final parse results using the `do_parse` macro.
		def self.constant(value : T) : Parser(T) forall T
			Parser(T).const value
		end

		# ```cr
		# p = Parse.char 'a'
		# puts p.parse "abc" #=> a
		# puts p.parse "bca" #=> expected 'a', got 'b'
		# puts p.parse "cab" #=> expected 'a', got 'c'
		# ```
		#
		# `Parse.char(c : Char)` creates a `Parser(Char)` that looks at the current parse position and expects `c`.
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

		# ```cr
		# p = Parse.string "cat"
		# puts p.parse "cat" #=> cat
		# puts p.parse "dog" #=> expected 'c', got 'd'
		# puts p.parse "" #=> expected 'c', got end of string
		# ```
		#
		# `Parse.string(s : String)` creates a `Parser(String)` that looks at the current parse position expects
		# the array of characters in the string `s` (`s.chars`) to be consecutively present.
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

		# ```cr
		# p = Parse.one_char_of "abc"
		# puts p.parse "apple" #=> a
		# puts p.parse "banana" #=> b
		# puts p.parse "carrot" #=> c
		# puts p.parse "dragonfruit" #=> expected 'c', got 'd'
		# ```
		#
		# `Parse.one_char_of(s : String)` creates a `Parser(Char)` that looks at the current parse position and
		# expects the current character to be present in the string `s`.
		def self.one_char_of(string : String) : Parser(Char)
			parser = char string[0]
			(1...string.size).each do |index|
				parser |= char string[index]
			end
			parser
		end

		# ```cr
		# p = Parse.no_char_of "abc"
		# puts p.parse "apple" #=> expected no character of 'abc', got 'a'
		# puts p.parse "banana" #=> expected no character of 'abc', got 'b'
		# puts p.parse "carrot" #=> expected no character of 'abc', got 'c'
		# puts p.parse "dragonfruit" #=> d
		# ```
		#
		# `Parse.no_char_of(s : String)` functions identically to `Parse.one_char_of`, but reverses the expected
		# input. If the current character is present in `s`, then the parse fails.
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

		# ```cr
		# char_a = Parse.char 'a'
		# many_space = Parse.many_of char_a
		# puts p.parse "abc" #=> ['a']
		# puts p.parse "aabbcc" #=> ['a', 'a']
		# puts p.parse "aaaaaah" #=> ['a', 'a', 'a', 'a', 'a', 'a']
		# puts p.parse "pars3k" #=> []
		# ```
		#
		# `Parse.many_of(p : Parser(T))` creates a `Parser(Array(T))` that continuously parses the parser `p`
		# until it fails. It succeeds with an array of the successive values.
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

		# ```cr
		# char_a = Parse.one_char_of "act"
		# valid = Parse.one_or_more_of char_a
		# puts valid.parse "cat" #=> ['c', 'a', 't']
		# puts valid.parse "act" #=> ['a', 'c', 't']
		# puts valid.parse "t" #=> ['t']
		# puts valid.parse "nope" #=> expected 't', got 'n'
		# ```
		#
		# `Parse.one_or_more_of(p : Parser(T))` creates a `Parser(Array(T))` that works like `Parse.many_of`,
		# but expects at least one parse to succeed. Returns with the error of the first failure if it does
		# not succeed.
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

		# ```cr
		# char_a = Parse.char 'a'
		# set = Parse.some_of char_a, 2..4
		# puts set.parse "" #=> expected 'a', got end of string
		# puts set.parse "a" #=> expected 'a', got end of string
		# puts set.parse "aa" #=> ['a', 'a']
		# puts set.parse "aaa" #=> ['a', 'a', 'a']
		# puts set.parse "aaaa" #=> ['a', 'a', 'a', 'a']
		# puts set.parse "aaaaa" #=> ['a', 'a', 'a', 'a']
		# ```
		#
		# `Parse.some_of(p : Parser(T), r : Range)` creates a `Parser(Array(T))` that works like `Parse.many_of`,
		# but fails if the number of successful parses is below the lower bound of the range `r`, and stops parsing
		# if the number of successful parses goes over the limit. In this case, `char_a` is parsed between 2 and 4 times.
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

		# Runs `Parse.some_of(p, count..count)`.
		def self.some_of(parser : Parser(T), count : Int32) : Parser(Array(T)) forall T
			some_of(parser, count..count)
		end

		# ```cr
		# char_a = Parse.char 'a'
		# set = Parse.one_of char_a
		# puts set.parse "" #=> []
		# puts set.parse "a" #=> ['a']
		# puts set.parse "aa" #=> ['a']
		# ```
		#
		# `Parse.one_of(p : Parser(T))` is a shortcut to `Parse.some_of(p, 0..1)`.
		def self.one_of(parser : Parser(T)) : Parser(Array(T)) forall T
			some_of parser, ..1
		end

		# ```cr
		# set = Parse.one_of? Parse.char 'a'
		# puts set.parse "" #=> nil
		# puts set.parse "a" #=> a
		# puts set.parse "aa" #=> a
		# ```
		#
		# `Parse.one_of?(p : Parser(T))` creates a `Parser(T | Nil)` that will return nil if no parse is found.
		# Otherwise, it returns the value of `T`. To use the result effectively, check the return type with
		# `result.nil?`.
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

		# ```cr
		# parser = (Parse.one_of? Parse.char 'a').sequence do |a|
		#   (Parse.if_not_nil? Parse.char('b'), char).sequence do |b|
		#     Parse.constant({a, b})
		#   end
		# end
		# puts parser.parse "ab" #=> {'a', 'b'}
		# puts parser.parse "ac" #=> expected 'b', got 'c'
		# puts parser.parse "b" #=> {nil, nil}
		# ```
		#
		# `Parse.if_not_nil?(p : Parser(T), value : B?)` creates a `Parser(T | Nil)` that will always return nil
		# if the initial `value` is nil. This is to be used with `do_parse` or `Parser#sequence`. It can be used to
		# ignore parsers if one before it yielded a nil value.
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

		# ```cr
		# whitespace = Parse.many_of Parse.char ' '
		# comma = whitespace >> Parse.char(',') >> whitespace
		# word = Parse.join Parse.one_or_more_of Parse.one_char_of "abcdefghijklmnopqrstuvwxyz01234567890"
		# list = Parse.delimited_list word, comma
		# puts list.parse "" #=> []
		# puts list.parse "hello, world" #=> ["hello", "world"]
		# puts list.parse "par ,  s3k" #=> ["par", "s3k"]
		# puts list.parse "!!!, hello" #=> []
		# ```
		#
		# `Parse.delimited_list(p : Parser(T), d : Parser(B))` creates a `Parser(Array(T))` that will continue to parse
		# with `p` delimited by `d` until an error with either occurs.
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

		# ```cr
		# alphabet = Parse.one_char_of "abcdefghijklmnopqrstuvwxyz"
		# word = Parse.many_of alphabet
		# word_joined = Parse.join word
		# puts word.parse "hello" #=> ['h', 'e', 'l', 'l', 'o']
		# puts word_joined.parse "hello" #=> hello
		# ```
		#
		# `Parse.join(p : Parser(Char))` transforms the parser `p` by adding all of the characters of a result
		# into a string.
		def self.join(parser : Parser(Array(Char))) : Parser(String)
			parser.transform &.reduce "" { |v, c| v + c }
		end

		# ```cr
		# Parse.one_char_of "abcdefghijklmnopqrstuvwxyz"
		# ```
		#
		# Parses a character of the lowercase alphabet.
		def self.alphabet_lower
			one_char_of "abcdefghijklmnopqrstuvwxyz"
		end

		# ```cr
		# Parse.one_char_of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		# ```
		#
		# Parses a character of the uppercase alphabet.
		def self.alphabet_upper
			one_char_of "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
		end

		# ```cr
		# Parse.alphabet_lower | Parse.alphabet_upper
		# ```
		#
		# Parses a character in the alphabet regardless of case.
		def self.alphabet
			alphabet_lower | alphabet_upper
		end

		# ```cr
		# Parse.join Parse.one_or_more_of Parse.alphabet
		# ```
		#
		# Parses a full word of at least one character.
		def self.word
			(one_or_more_of alphabet).transform { |c| concatenate c }
		end

		# ```cr
		# Parse.one_char_of "0123456789"
		# ```
		#
		# Parses a digit as a character.
		def self.digit
			one_char_of "0123456789"
		end

		# ```cr
		# (Parse.join Parse.one_or_more_of Parse.digit).transform &.to_i
		# ```
		#
		# Parses an integer as an actual `Int`.
		def self.int
			(one_or_more_of digit).transform { |c| (concatenate c).to_i }
		end

		# ```cr
		# do_parse({
		#   whole <= (Parse.join Parse.one_or_more_of Parse.digit),
		#   _ <= (Parse.one_of? char '.'),
		#   decimal <= (Parse.join Parse.many_of Parse.digit),
		#   Parse.constant "#{whole}#{decimal.size == 0 ? ".0" : "." + decimal}".to_f
		# })
		# ```
		#
		# Parses a float as an actual `Float`.
		def self.float
			do_parse({
				whole <= (join one_or_more_of digit),
				_ <= (one_of? char '.'),
				decimal <= (join many_of digit),
				constant "#{whole}#{decimal.size == 0 ? ".0" : "." + decimal}".to_f
			})
		end

		private def self.concatenate(chars : Array(Char))
			chars.reduce "" { |v, c| v + c }
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
end

