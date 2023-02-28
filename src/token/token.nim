import std/unittest
import std/tables

type
  TokenType* = string
  Token* = object
    token_type*: TokenType
    literal*: string

const
  ILLEGAL* = "ILLEGAL"
  EOF* = "EOF"
  IDENT* = "IDENT"
  INT* = "INT"

  ASSIGN* = "="
  PLUS* = "+"
  MINUS* = "-"
  BANG* = "!"
  ASTERISK* = "*"
  SLASH* = "/"
  LT* = "<"
  GT* = ">"

  EQ* = "=="
  NEQ* = "!="

  COMMA* = ","
  SEMICOLON* = ";"
  LPAREN* = "("
  RPAREN* = ")"
  LBRACE* = "{"
  RBRACE* = "}"

  FUNCTION* = "FUNCTION"
  LET* = "LET"
  TRUE* = "TRUE"
  FALSE* = "FALSE"
  IF* = "IF"
  ELSE* = "ELSE"
  RETURN* = "RETURN"

  keywords = {
    "fn": FUNCTION,
    "let": LET,
    "true": TRUE,
    "false": FALSE,
    "if": IF,
    "else": ELSE,
    "return": RETURN
  }.toTable

  DEFAULT_TOKEN* = Token(token_type: ILLEGAL, literal: "DEFAULT_TOKEN")

proc token*(tt, l: string): Token =
  Token(token_type: tt, literal: l)

proc lookup_ident*(i: string): TokenType =
  try: result = keywords[i]
  except: result = IDENT

suite "test token":
  setup:
    let tok = token(IDENT, "IDENT")

  test "test create token":
    check:
      tok.token_type == IDENT
      tok.literal == "IDENT"

  test "test lookup ident":
    check:
      lookup_ident("fn") == FUNCTION
      lookup_ident("let") == LET
      lookup_ident("this_is_a_custom_identifier") == IDENT