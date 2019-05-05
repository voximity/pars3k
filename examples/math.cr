require "../src/pars3k.cr"
include Pars3k

inputs = ["1+1", "1 + 1", "2 * 2", "6 - 3", "1+1+1"]

def whitespace
	Parse.many_of Parse.one_char_of " \n\t"
end

def operator
	Parse.one_char_of "+-/*"
end

def number
	Parse.int
end

def evaluate(a, op, b)
	if op.nil? || b.nil?
		return a
	end
	case op
	when '+'
		a+b
	when '-'
		a-b
	when '*'
		a*b
	when '/'
		a//b
	else
		a
	end
end

def expression : Parser(Int32)
	do_parse({
		num_a <= number,
		_ <= whitespace,
		op <= Parse.one_of?(operator),
		_ <= whitespace,
		num_b <= Parse.if_not_nil?(expression, op),
		eval = evaluate(num_a, op, num_b),
		Parse.constant(eval)
	})
end

inputs.each do |input|
	result = expression.parse input
	puts input
	puts result
	puts ""
end