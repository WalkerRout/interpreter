
import token

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

proc next_char*(l: var Lexer): char =
  result = l.input[l.next_position]
  if l.next_position >= len(l.input):
    result = '\0'

proc next_token*(l: var Lexer): token.Token =
  l.skip_whitespace() # remove unnecessary characters

  case l.curr_char
  of '=':
    if l.next_char() == '=':
      l.read_char()
      result = token.token(token.EQ, "==")
    else:
      result = token.token(token.ASSIGN, $l.curr_char)
  of '!':
    if l.next_char() == '=':
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

