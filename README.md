# pars3k

pars3k (parsec/parse 3000) is a library for Crystal adding support for combinator parsers.
The structure is heavily inspired by Parsec from Haskell. It is also inspired by `crystal-parsec`.

## Explanation

A combinator parser is a parsing system that allows for the creation of small "parsers,"
an object that takes a String in, and has some small computational logic applied to extract
a deeper meaning, like parsing through JSON and converting to a usable `Hash`.

The idea of a combinator parser is the concept of literally combining parsers. Small parsers
can be combined with logic (like OR, AND, etc.) to create larger, more meaningful parsers.
Ultimately, you should end up with one big parser.

### Application

As mentioned before, this style of parser can be used for creating interpreted programming languages,
decoding markup languages, reading from files of different formats, etc. It is generally accepted as
a more flexible and reliable parsing style than, say, regular expressions. The following category will
contain some use cases using pars3k.

## Usage

```cr
require "p3k"
include Pars3k
```

*Note: **HIGHLY** recommend you `include Pars3k`*

All parsers are of type `Parser(T)`, where `T` is the ultimate return type of the Parser.
One example of a primitive Parser is a `Parser(Char)`, which can be created using `Parse.char(Char)`.
The method gives a `Parser(Char)` that, when parsed, looks for a specific character as specified.

### Primitive parsers

```cr
char_a = Parse.char('a')

puts char_a.parse "abc" #=> a
```

This example creates a `Parser(Char)` from `Parse.char`, and parses the string `"abc"` on it. The
character parser looks at the beginning of the string, and looks for the first character. If the
first character matches the character supplied to the parser originally (`Parse.char 'a'`), then
the parse will succeed and the parse result will return the character that was matched.

```cr
puts char_a.parse "bca" #=> expected 'a', got 'b'
```

This example uses the same `char_a` parser, but parses string `"bca"` on it. Because it doesn't start
with the original requirement `'a'`, the parse fails and returns a `ParseError`. A `ParseError` is a
struct that contains a message about the parse failure, which can be retrieved using `ParseError#message`.
As such, the return type of `Parser(T)#parse` is a union type of `(T | ParseError)`, as it can return either.

```cr
str_cat = Parse.string "cat"

puts str_cat.parse "cat" #=> cat
puts str_cat.parse "cats are cool" #=> cat
puts str_cat.parse "dog" #=> expected 'cat', got 'd'
```

This example creates a new primitive parser, the `Parser(String)` created by `Parse.string(String)`. It expects
an exact copy of the string provided. In this example, the text `"cat"` is used.

### Amalgam parsers

```cr
char_a = Parse.char 'a'
char_b = Parse.char 'b'
parse_ab = char_a | char_b

puts parse_ab.parse "abc" #=> a
puts parse_ab.parse "bca" #=> b
puts parse_ab.parse "cab" #=> expected 'b', got 'c'
```

This example creates three parsers:

- a `Parser(Char)` that expects a character of `'a'`,
- a `Parser(Char)` that expects a character of `'b'`, and
- a `Parser(Char)` created using the `|` operator that will try the left parser first, then the right, and use
  the successful parser.

As seen the `|` operator allows you to create amalgam parsers by using OR logic. It first tries the parser
on the left, then the right. If both fail, it will throw the `ParseError` given by the rightmost parser.

This process is tedious for large masses of characters, such as if you wanted to accept all letters of the
alphabet. For this sake, there exists a `Parser(Char)` that can be created with `Parse.one_char_of(String)`,
which accepts a String as a list of characters, and allows a character to be parsed that is in that list.

```cr
parse_alphabet = Parse.one_char_of "abcdefghijklmnopqrstuvwxyz"

puts parse_alphabet.parse "abc" #=> a
puts parse_alphabet.parse "bca" #=> b
puts parse_alphabet.parse "xyz" #=> x
puts parse_alphabet.parse "yzx" #=> y
puts parse_alphabet.parse "123" #=> expected 'z', got '1'
```

This example creates a parser that accepts a char from the provided list. As seen, alphabetical characters
parse successfully, but numerical characters do not, as they were not in the original string of the
alphabet.

Additionally, this `parse_alphabet` definition is available from `Parse.alphabet`, which also accepts uppercase.
`Parse.alphabet_lower` only accepts lowercase.

#### Repetitive parsers

We can create parsers that repeat until they no longer parse using `Parse.many_of(Parser(T))`. This parser
takes in any kind of `Parser(T)`, and outputs a usable parser of type `Parser(Array(T))`. It will match a string
continuously until it no longer can, and group all parsed values into an array.

Extra note: `Parse.one_or_more_of(Parser(T))` also exists, acting similarly, but erroring if at least one parse
does not parse successfully.

```cr
word = Parse.many_of Parse.alphabet

puts word.parse "hello world" #=> ['h', 'e', 'l', 'l', 'o']
puts word.parse "abc" #=> ['a', 'b', 'c']
puts word.parse "123" #=> []
```

There is a clear issue with the result of the parser: it returns a list of the characters. If we want to convert
this into a usable `String`, we have to transform the parser.

### Transforming parsers

Existing parsers can be "transformed" to create new parsers with new logic. Transforming parsers is very useful.
To transform a parser, use the `Parser(T)#transform(T -> B)` method, which accepts a block that receives the
resulting value of a parse as a parameter, and outputs a transformed/mapped value.

For example, if you created a parser that accepted numbers:

```cr
digit = Parse.one_char_of "0123456789"
```

Upon parsing it, it would yield characters on success:

```cr
puts (digit.parse "1").class #=> Char
```

we find that the result is a `Char`, not any form of a `Number`! To solve this, we can transform the parser:

```cr
digit = (Parse.one_char_of "0123456789").transform { |char| char.to_i }

puts digit.parse "1" #=> 1
puts (digit.parse "1").class #=> Int32
```

Success! Now the parsed value from our parser is the correct type, `Int32`.

Back to the issue we found in the word parser from the previous section, we can transform the `Array(Char)` to
a `String`.

```cr
word = (Parse.many_of Parse.alphabet).transform &.reduce("") { |value, char| value + char}

puts word.parse "hello world" #=> hello
puts word.parse "abc" #=> abc
puts word.parse "" #=>
```

This transformation takes `chars`, an `Array(Char)`, and transforms it by using its reduce method, which iterates
through all of the characters in the array and adds them to a blank string.

Additionally, this identical `word` parser is available as `Parse.word` (`Parser(String)`).

### Parser operators

The `|` (OR) operator has already been discussed. There are a couple more operators, like:

- `A >> B` creates a new parser that ensures both A and B parse sequentially, but results with the value of B.
- `A << B` creates a new parser that ensures both A and B parse sequentially, but results with the value of A.

```cr
letter = Parse.alphabet
digit = Parse.one_char_of "0123456789"
parser_take_digit = letter >> digit
parser_take_letter = letter << digit

puts parser_take_digit.parse "a1" #=> 1
puts parser_take_digit.parse "b2" #=> 2

puts parser_take_letter.parse "a1" #=> a
puts parser_take_letter.parse "b2" #=> b
```

In this example, two parsers are created, `letter` and `digit`, that are just amalgam parsers that allow any
alphabetical character or any digit character, respectively. Then, two new parsers are created using the `>>` and
`<<` operators. The first parses both sequentially but results with the result of `digit`, and the second does the
same but results with the value of `letter`. Upon parsing these, the two parsers must work sequentially, but
only returns with the parser's result the operator is pointing toward.

### Parsing lists

`Parse` has a special parser that can parse a list of parsable items by parser `A`, delimited by parser `B`.
Using this, we can create a parser that parses through a list of words (using `Parser.word`), delimited by
an amalgam parser that uses commas. It is called from `Parse.delimited_list(Parser(A), Parser(B)) : Parser(Array(A))`.

```cr
word = Parser.word
optional_whitespace = Parser.many_of Parser.char ' '
comma = (Parser.char ',') << optional_whitespace

list_parser = Parse.delimited_list word, comma

puts list_parser.parse "hello, world" #=> ["hello", "world"]
puts list_parser.parse "how,are,    you" #=> ["how", "are", "you"]
puts list_parser.parse "123, 456" #=> []
puts list_parser.parse "hello world, how are you" #=> ["hello"]
```

### Complex sequential parsers

In the event you need to create complex sequential parsers, you can use `Parser(T)#sequence`. The `sequence`
method is a method that takes in a block that receives the output of `Parser(T)` as a value, and must return
a new `Parser` of any type, or `Parser(B)`. We can recreate the `parser_take_digit` and `parser_take_letter`
parsers using this functionality:

```cr
letter = Parse.alphabet
digit = Parse.one_char_of "0123456789"

parser_take_digit = letter.sequence do |char_result|
  digit.sequence do |digit_result|
    Parse.constant digit_result
  end
end
```

The original two parsers are `sequence`d in each other, and ultimately, a `Parse.constant` parser is returned.
A `Parse.constant` parser is a parser that takes in any value of type `T`. When parsed, it ALWAYS returns the
value of type `T`. In this case, we create it with the `Char` result from `digit`.

```cr
parser_letter_digit = letter.sequence do |char_result|
  digit.sequence do |digit_result|
    Parse.constant({char_result, digit_result}) # a constant parser with a `Tuple(Char, Char)`
  end
end
```

This parser will parse strings like `a1`, `b2`, `c3`, etc., but return both of the retrieved values as a `Tuple`.

```cr
result = parser_letter_digit.parse "a1"

puts result[0] #=> a
puts result[1] #=> 1
```

This form of parser sequencing can quickly become tedious. As a result, the library has a special macro
inspired by Haskell's `do` statement. It allows you to chain parsers like above, but in a much more linear
and organized manner. Here is the most recent sequential parser `parser_letter_digit` using `do_parse`:

```cr
parser_letter_digit = do_parse({
  char_result <= letter,
  digit_result <= digit,
  Parse.constant({char_result, digit_result})
})
```

The body of the `do_parse` macro is a list of actions separated by commas. The last element of this list
MUST be an expression that is ultimately returned through the new parser.

For each of the other elements in the list, they must be either parser results or local variables.

- Parser results look like `result_variable_name <= parser,`. In this case, the result from `parser` is
  stored as `result_variable_name`.
- Local variables are simply `variable_name = value,`. In this case, `variable_name` is set to `value`.

Utilizing our knowledge of pars3k, we can create parsers like this:

```cr
word = Parse.word

whitespace = Parse.many_of Parse.char ' '
equals = whitespace >> (Parse.char '=') << whitespace

key_value_pair = do_parse({
  key <= word,
  eq <= equals,
  value <= word,
  Parse.constant({key, value})
})

comma = (Parse.char ',') << whitespace

key_value_list = Parse.delimited_list key_value_pair, comma

puts key_value_list.parse "hello = world" #=> [{"hello", "world"}]
puts key_value_list.parse "how = are, you= sir" #=> [{"how", "are"}, {"you", "sir"}]
puts key_value_list.parse "all=     sorts,of   =supported, white = spaces" #=> [{"all", "sorts"}, {"of", "supported"}, {"white", "spaces"}]
```

### Custom parsers

Although not exactly necessary, custom parsers can be created using custom logic.

```cr
def char_parser(char)
  Parser(Char).new do |context|
    if context.position >= context.parsing.size
      ParseResult(Char).error "expected '#{char}', got end of string", context
    elsif context.parsing[context.position] == char
      ParseResult(Char).new char, context.next
    else
      ParseResult(Char).error "expected '#{char}', got '#{context.parsing[context.position]}", context
    end
  end
end
```

- `context` is a `ParseContext`
- `context.parsing` is the string being parsed
- `context.position` is the position in the string being analyzed
- `ParseResult(T)` is a parse result of type `T`, same as the parser expected
- `ParseResult(T).new result, new_context_after_shift` should be used to yield parsed values
- `ParseResult(T).error message, original_context` should be used to throw parse errors

This defines `char_parser(Char)`, which creates a parser that expects a character as specified. This implementation
is the same as the internal implementation `Parse.char(Char)`. See the source code for more applications of
Parsers derived from blocks.