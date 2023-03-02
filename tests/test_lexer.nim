import std/unittest

import ../src/lexer

import ../src/token

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