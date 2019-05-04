require "./src/p3k.cr"
include Pars3k

def concatenate(array : Array(Char)) : String
	result = ""
	array.each do |char|
		result += char
	end
	result
end

alphabet = parse_one_char_of "abcdefghijklmnopqrstuvwxyz"
word = (parse_one_or_more_of alphabet).transform { |chars| concatenate chars }
whitespace = parse_many_of parse_char ' '
comma = (parse_char ',') << whitespace

puts (parse_delimited_list word, comma).parse "hello, world,       how,are,   you"