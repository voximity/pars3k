# This is just a test file. Nothing interesting is actually here!

require "./src/p3k.cr"
include Pars3k

#whitespace = Parse.many_of Parse.char ' '
#comma = (Parse.char ',') << (whitespace)
#colon = (whitespace) >> (Parse.char ':') << (whitespace)

#key_value = parse_monad({
#	key <= Parse.word,
#	colon <= colon,
#	value <= Parse.word,
#	Parser.const({key, value})
#})

#puts Parse.delimited_list(key_value, comma).parse("hello: world, greetings: man,yeet     :haha")

digit = (Parse.one_char_of "0123456789").transform { |char| char.to_i }

puts digit.parse "1" #=> 1
puts (digit.parse "1").class #=> 