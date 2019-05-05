require "../src/pars3k.cr"
include Pars3k
aaa = (Parse.one_of? Parse.word)
thing = (Parse.one_of? Parse.word).sequence do |w|
	(Parse.if_not_nil?(Parse.int, w)).sequence do |num|
		Parse.constant({w, num})
	end
end
puts thing.parse("abc123")
puts thing.parse("123abc")