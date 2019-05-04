require "../src/p3k.cr"
include Pars3k

result = Parse.float.parse "1"
unless result.is_a? ParseError
	puts result + 1
end