import std/unittest
import ../token/token

type
  Lexer* = object
    input*: string
    position*: int32
    next_position*: int32
    curr_char*: char

# forward declarations
proc read_char*(l: var Lexer)
proc read_identifier(l: var Lexer): string
proc read_digit(l: var Lexer): string
proc skip_whitespace(l: var Lexer)
proc is_letter(c: char): bool
proc is_digit(c: char): bool

# procedures
# lexer procs
proc lexer*(i: string): Lexer =
  result = Lexer(input: i, position: 0, next_position: 0, curr_char: '\0')
  result.read_char()

proc lexer*(i: string; p, np: int32; cc: char): Lexer =
  result = Lexer(input: i, position: p, next_position: np, curr_char: cc)
  result.read_char()

proc read_char*(l: var Lexer) =
  if l.next_position >= len(l.input):
    l.curr_char = '\0'
  else:
    l.curr_char = l.input[l.next_position]
  l.position = l.next_position
  l.next_position += 1

proc peek_char*(l: var Lexer): char =
  result = l.input[l.next_position]
  if l.next_position >= len(l.input):
    result = '\0'

proc next_token*(l: var Lexer): token.Token =
  l.skip_whitespace() # remove unnecessary characters

  case l.curr_char
  of '=':
    if l.peek_char() == '=':
      l.read_char()
      result = token.token(token.EQ, "==")
    else:
      result = token.token(token.ASSIGN, $l.curr_char)
  of '!':
    if l.peek_char() == '=':
      l.read_char()
      result = token.token(token.NEQ, "!=")
    else:
      result = token.token(token.BANG, $l.curr_char)
  of ';':
    result = token.token(token.SEMICOLON, $l.curr_char)
  of '(':
    result = token.token(token.LPAREN, $l.curr_char)
  of ')':
    result = token.token(token.RPAREN, $l.curr_char)
  of ',':
    result = token.token(token.COMMA, $l.curr_char)
  of '+':
    result = token.token(token.PLUS, $l.curr_char)
  of '-':
    result = token.token(token.MINUS, $l.curr_char)
  of '/':
    result = token.token(token.SLASH, $l.curr_char)
  of '*':
    result = token.token(token.ASTERISK, $l.curr_char)
  of '<':
    result = token.token(token.LT, $l.curr_char)
  of '>':
    result = token.token(token.GT, $l.curr_char)
  of '{':
    result = token.token(token.LBRACE, $l.curr_char)
  of '}':
    result = token.token(token.RBRACE, $l.curr_char)
  of '\0':
    result.token_type = token.EOF
    result.literal = ""
  else:
    if l.curr_char.is_letter():
      result.literal = l.read_identifier()
      result.token_type = token.lookup_ident(result.literal)
      return
    elif l.curr_char.is_digit():
      result.literal = l.read_digit()
      result.token_type = token.INT
      return
    else:
      result = token.token(token.ILLEGAL, $l.curr_char)

  l.read_char()

proc read_identifier(l: var Lexer): string =
  let position = l.position
  while l.curr_char.is_letter():
    l.read_char()
  l.input.substr(position, l.position-1)

proc read_digit(l: var Lexer): string = 
  let position = l.position
  while l.curr_char.is_digit():
    l.read_char()
  l.input.substr(position, l.position-1)

proc skip_whitespace(l: var Lexer) =
  var cc = l.curr_char
  while cc == ' ' or cc == '\t' or cc == '\n' or cc == '\r':
    l.read_char()
    cc = l.curr_char

# helper procs
proc is_letter(c: char): bool =
  let cc = cast[uint8](c)
  cast[uint8]('a') <= cc and cast[uint8]('z') >= cc or
  cast[uint8]('A') <= cc and cast[uint8]('Z') >= cc or
  cast[uint8]('_') == cc

proc is_digit(c: char): bool =
  let cc = cast[uint8](c)
  cast[uint8]('0') <= cc and cast[uint8]('9') >= cc

suite "test lexer":
  setup:
    type TestToken = object
      expected_type: TokenType
      expected_literal: string

    proc test_token(et, el: string): TestToken =
      TestToken(expected_type: et, expected_literal: el)

  test "test next token single char":
    let input = """
      =+(){},;
    """
    let tests = @[
      test_token(token.ASSIGN, "="),
      test_token(token.PLUS, "+"),
      test_token(token.LPAREN, "("),
      test_token(token.RPAREN, ")"),
      test_token(token.LBRACE, "{"),
      test_token(token.RBRACE, "}"),
      test_token(token.COMMA, ","),
      test_token(token.SEMICOLON, ";")
    ]
    var lexer = lexer(input)

    for test in tests:
      let tok = lexer.next_token()
      check:
        tok.token_type == test.expected_type
        tok.literal == test.expected_literal

  test "test next token multi char":
    let input = """
      let five = 5;
      let ten = 10;
      
      let add = fn(x, y) {
        x + y;
      };
      
      let result = add(five, ten);
    """
    let tests = @[
      test_token(token.LET, "let"),
      test_token(token.IDENT, "five"),
      test_token(token.ASSIGN, "="),
      test_token(token.INT, "5"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.LET, "let"),
      test_token(token.IDENT, "ten"),
      test_token(token.ASSIGN, "="),
      test_token(token.INT, "10"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.LET, "let"),
      test_token(token.IDENT, "add"),
      test_token(token.ASSIGN, "="),
      test_token(token.FUNCTION, "fn"),
      test_token(token.LPAREN, "("),
      test_token(token.IDENT, "x"),
      test_token(token.COMMA, ","),
      test_token(token.IDENT, "y"),
      test_token(token.RPAREN, ")"),
      test_token(token.LBRACE, "{"),
      test_token(token.IDENT, "x"),
      test_token(token.PLUS, "+"),
      test_token(token.IDENT, "y"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.RBRACE, "}"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.LET, "let"),
      test_token(token.IDENT, "result"),
      test_token(token.ASSIGN, "="),
      test_token(token.IDENT, "add"),
      test_token(token.LPAREN, "("),
      test_token(token.IDENT, "five"),
      test_token(token.COMMA, ","),
      test_token(token.IDENT, "ten"),
      test_token(token.RPAREN, ")"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.EOF, "")
    ]
    var lexer = lexer(input)

    for test in tests:
      let tok = lexer.next_token()
      check:
        tok.token_type == test.expected_type
        tok.literal == test.expected_literal

  test "test next token two char comparisons":
    let input = """
      !-/*5;
      5 < 10 > 5;
      5 != 56;
      10 == 10;
    """
    let tests = @[
      test_token(token.BANG, "!"),
      test_token(token.MINUS, "-"),
      test_token(token.SLASH, "/"),
      test_token(token.ASTERISK, "*"),
      test_token(token.INT, "5"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.INT, "5"),
      test_token(token.LT, "<"),
      test_token(token.INT, "10"),
      test_token(token.GT, ">"),
      test_token(token.INT, "5"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.INT, "5"),
      test_token(token.NEQ, "!="),
      test_token(token.INT, "56"),
      test_token(token.SEMICOLON, ";"),
      test_token(token.INT, "10"),
      test_token(token.EQ, "=="),
      test_token(token.INT, "10"),
      test_token(token.SEMICOLON, ";")
    ]
    var lexer = lexer(input)

    for test in tests:
      let tok = lexer.next_token()
      check:
        tok.token_type == test.expected_type
        tok.literal == test.expected_literal

  test "test next token if and return statements":
    let input = """
      if (5 < 10) {
        return true;
      } else {
        return false;
      }
    """
    let tests = @[test_token(token.IF, "if"),
                  test_token(token.LPAREN, "("),
                  test_token(token.INT, "5"),
                  test_token(token.LT, "<"),
                  test_token(token.INT, "10"),
                  test_token(token.RPAREN, ")"),
                  test_token(token.LBRACE, "{"),
                  test_token(token.RETURN, "return"),
                  test_token(token.TRUE, "true"),
                  test_token(token.SEMICOLON, ";"),
                  test_token(token.RBRACE, "}"),
                  test_token(token.ELSE, "else"),
                  test_token(token.LBRACE, "{"),
                  test_token(token.RETURN, "return"),
                  test_token(token.FALSE, "false"),
                  test_token(token.SEMICOLON, ";"),
                  test_token(token.RBRACE, "}"),]
    var lexer = lexer(input)

    for test in tests:
      let tok = lexer.next_token()
      check:
        tok.token_type == test.expected_type
        tok.literal == test.expected_literal