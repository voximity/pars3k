# This is just a test file. Nothing interesting is actually here!

require "./src/p3k.cr"
include Pars3k

def concatenate(array : Array(Char)) : String
	result = ""
	array.each do |char|
		result += char
	end
	result
end

whitespace = parse_many_of parse_char ' '
comma = (parse_char ',') << whitespace

key_value = parse_monad({
	key <= parse_word,
	colon <= whitespace >> (parse_char ':') << whitespace,
	value <= parse_word,
	Parser.const({key, value})
})

puts parse_delimited_list(key_value, comma).parse("hello: world, greetings: man,yeet     :haha")