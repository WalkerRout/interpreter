import std/unittest
import std/tables
#import std/strutils
import std/typetraits

import ../src/parser

import ../src/util
import ../src/token
import ../src/lexer

suite "test parser":
  setup:
    type
      TestIdent = object
        expected_identifier: string

      TestLet[T] = object
        input: string
        expected_identifier: string
        expected_value: T

      TestReturn[T] = object
        input: string
        expected_value: T

      TestBoolean = object
        input: string
        expected_state: bool

      TestPrefix[T] = object
        input: string
        operator: string
        value: T

      TestInfix[T] = object
        input: string
        operator: string
        left_value: T
        right_value: T

      TestPrecedence = object
        input: string
        expected: string

      TestFn = object
        input: string
        expected_params: seq[string]

    proc test_identn(ei: string): TestIdent =
      TestIdent(expected_identifier: ei)

    proc test_letn[T](i: string, ei: string, ev: T): TestLet[T] =
      TestLet[T](input: i, expected_identifier: ei, expected_value: ev)

    proc test_returnn[T](i: string, ev: T): TestReturn[T] =
      TestReturn[T](input: i, expected_value: ev)

    proc test_booleann(i: string, es: bool): TestBoolean =
      TestBoolean(input: i, expected_state: es)

    proc test_prefixn[T](i, o: string, v: T): TestPrefix[T] =
      TestPrefix[T](input: i, operator: o, value: v)

    proc test_infixn[T](i, o: string, lv, rv: T): TestInfix[T] =
      TestInfix[T](input: i, operator: o, left_value: lv, right_value: rv)

    proc test_precedencen(i, e: string): TestPrecedence =
      TestPrecedence(input: i, expected: e)

    proc test_fnn(i: string, ep: seq[string]): TestFn =
      TestFn(input: i, expected_params: ep)

    # tests for literals, not expression statements
    proc test_integer_literal(e: Node, value: int64): bool =
      let ile = dynamic_cast[IntegerLiteralExpression](e)
      if ile == nil: return false
      result = ile.value == value
      result = result and ile.token_literal() == $value

    proc test_identifier(e: Node, value: string): bool =
      let ie = dynamic_cast[IdentifierExpression](e)
      if ie == nil: return false
      result = ie.value == value
      result = result and ie.token_literal() == value

    proc test_boolean(e: Node, value: bool): bool =
      let be = dynamic_cast[BooleanExpression](e)
      if be == nil: return false
      result = be.value == value
      result = result and be.token_literal() == $value

    proc test_literal(e: Node, value: auto): bool =
      const type_name = value.type.name
      when type_name == "int64" or type_name == "int":
        test_integer_literal(e, value)
      elif type_name == "string":
        test_identifier(e, value)
      elif type_name == "bool":
        test_boolean(e, value)
      else:
        echo "No literal test for type " & type_name
        false
    # end of tests for literals

    proc test_prefix(e: Node, operator: string, left: auto): bool =
      let pe = dynamic_cast[PrefixExpression](e)
      if pe == nil: return false
      result = pe.operator == operator
      result = result and test_literal(pe.value, left)

    proc test_infix(e: Node, operator: string, left, right: auto): bool =
      let ie = dynamic_cast[InfixExpression](e)
      if ie == nil: return false
      result = test_literal(ie.left_value, left)
      result = result and ie.operator == operator
      result = result and test_literal(ie.right_value, right)
    # end tests for literals

    proc test_let_statement(s: Node, test: TestLet): bool =
      let ls = dynamic_cast[LetStatement](s)
      if ls == nil: return false
      result = test_literal(ls.name, test.expected_identifier)
      result = result and test_literal(ls.value, test.expected_value)

    proc test_return_statement(s: Node, test: TestReturn): bool =
      let rs = dynamic_cast[ReturnStatement](s)
      if rs == nil: return false
      result = rs.token_literal == "return"

  test "test let statement parsing":
    let tests_int = @[
      test_letn("let x = 5;", "x", 5),
      test_letn("let y = 10;", "y", 10),
      test_letn("let foobar = 20040605;", "foobar", 20040605),
    ]
    let tests_string = @[
      test_letn("let barfoo = x;", "barfoo", "x")
    ]
    for i, test in tests_int.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_let_statement(program.statements[0], test)

    for i, test in tests_string.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_let_statement(program.statements[0], test)


  test "test error logging":
    let input = """
      let x 5;
      let = 10;
      let 20040605;
    """
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    #check_parser_errors(parser) #commment/uncomment to start/stop the test
    check len(program.statements) == 4 # there are 4 expression statements (-> x, 5, 10, 20040605)

  test "test return statement parsing":
    let tests_int = @[
      test_returnn("return 5;", 5),
      test_returnn("return 10;", 10)
    ]
    let tests_string = @[
      test_returnn("return x;", "x")
    ]
    for i, test in tests_int.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_return_statement(program.statements[0], test)

    for i, test in tests_string.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_return_statement(program.statements[0], test)

  test "test string parsing":
    let s = let_statement(
      token.token(token.LET, "let"),
      identifier_expression(
        token.token(token.IDENT, "variable_a"),
        "variable_a"
      ),
      identifier_expression(
        token.token(token.IDENT, "variable_b"),
        "variable_b"
      )
    ) # end of let_statement
    let p = program(@[Node(s)])
    check p.string() == "let variable_a = variable_b;\n"

  test "test identifier expression parsing":
    let input = """
      foobar;
    """
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    check:
      len(program.statements) == 1
      test_identifier(program.statements[0].ExpressionStatement.expression, "foobar")

  test "test integer literal expression parsing":
    let input = """
      5;
    """
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    check:
      len(program.statements) == 1
      test_integer_literal(program.statements[0].ExpressionStatement.expression, 5)

  test "test prefix expression parsing":
    let tests_int = @[
      test_prefixn("!5;", "!", 5),
      test_prefixn("-15;", "-", 15)
    ]
    let tests_bool = @[
      test_prefixn("!true;", "!", true),
      test_prefixn("!false;", "!", false)
    ]
    for test in tests_int:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_prefix(program.statements[0].ExpressionStatement.expression, test.operator, test.value.int64)

    for test in tests_bool:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_prefix(program.statements[0].ExpressionStatement.expression, test.operator, test.value.bool)

  test "test infix expression parsing":
    let tests_int = @[
      test_infixn("5 + 5;",  "+",  5, 5),
      test_infixn("5 - 5;",  "-",  5, 5),
      test_infixn("5 * 5;",  "*",  5, 5),
      test_infixn("5 / 5;",  "/",  5, 5),
      test_infixn("5 < 5;",  "<",  5, 5),
      test_infixn("5 > 5;",  ">",  5, 5),
      test_infixn("5 == 5;", "==", 5, 5),
      test_infixn("5 != 5;", "!=", 5, 5),
    ]
    let tests_bool = @[
      test_infixn("true == true;", "==", true, true),
      test_infixn("true != false;", "!=", true, false),
      test_infixn("false == false;", "==", false, false),
    ]
    for test in tests_int:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_infix(program.statements[0].ExpressionStatement.expression, test.operator, test.left_value.int64, test.right_value.int64)

    for test in tests_bool:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_infix(program.statements[0].ExpressionStatement.expression, test.operator, test.left_value.bool, test.right_value.bool)

  test "test infix expression operator precedence parsing":
    let tests = @[
      test_precedencen("-a * b;", "((-a) * b)"),
      test_precedencen("!-a;", "(!(-a))"),
      test_precedencen("a + b + c;", "(a + (b + c))"),
      test_precedencen("a + b - c;", "(a + (b - c))"),
      test_precedencen("a * b * c;", "((a * b) * c)"),
      test_precedencen("a * b / c;", "((a * b) / c)"),
      test_precedencen("a + b / c;", "(a + (b / c))",),
      test_precedencen("a + b * c + d / e - f;", "(a + ((b * c) + ((d / e) - f)))"),
      test_precedencen("3 + 4; -5 * 5;", "(3 + 4)\n((-5) * 5)"),
      test_precedencen("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4))"),
      test_precedencen("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4))"),
      test_precedencen("3 + 4 * 5 == 3 * 1 + 4 * 5;", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
      test_precedencen("true;", "true"),
      test_precedencen("false;", "false"),
      test_precedencen("3 > 5 == false", "((3 > 5) == false)"),
      test_precedencen("3 < 5 == true", "((3 < 5) == true)")
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        program.string() == test.expected & "\n"

  test "test boolean expression parsing":
    let tests = @[
      test_booleann("true;", true),
      test_booleann("false;", false)
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        test_literal(program.statements[0].ExpressionStatement.expression, test.expected_state)

  test "test forced precedence expression parsing":
    let tests = @[
      test_precedencen("1 + (2 + 3) + 4", "(1 + ((2 + 3) + 4))"),
      test_precedencen("(5 + 5) * 2", "((5 + 5) * 2)"),
      test_precedencen("2 / (5 + 5)", "(2 / (5 + 5))"),
      test_precedencen("-(5 + 5)", "(-(5 + 5))"),
      test_precedencen("!(true == true)", "(!(true == true))"),
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        program.string() == test.expected & "\n"

  test "test if expression parsing":
    let test = "if (x < y) { x };"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let ie = dynamic_cast[IfExpression](program.statements[0].ExpressionStatement.expression)
    check:
      ie != nil
      len(program.statements) == 1
      test_infix(ie.condition, "<", "x", "y")
      len(ie.consequence.statements) == 1
      test_literal(ie.consequence.statements[0].ExpressionStatement.expression, "x")
      ie.alternative == nil

  test "test if else expression parsing":
    let test = "if (x < y) { x } else { y };"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let ie = dynamic_cast[IfExpression](program.statements[0].ExpressionStatement.expression)
    check:
      ie != nil
      len(program.statements) == 1
      test_infix(ie.condition, "<", "x", "y")
      len(ie.consequence.statements) == 1
      test_literal(ie.consequence.statements[0].ExpressionStatement.expression, "x")
      test_literal(ie.alternative.statements[0].ExpressionStatement.expression, "y")

  test "test fn expression parsing":
    let test = "fn (x, y) { x + y; };"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let fe = dynamic_cast[FnExpression](program.statements[0].ExpressionStatement.expression)
    check:
      fe != nil
      len(program.statements) == 1
      test_literal(fe.parameters[0], "x")
      test_literal(fe.parameters[1], "y")
      len(fe.body.statements) == 1
      test_infix(fe.body.statements[0].ExpressionStatement.expression, "+", "x", "y")

  test "test fn expression edge case parsing":
    let tests = @[
      test_fnn("fn () {};", @[]),
      test_fnn("fn (x) {};", @["x"]),
      test_fnn("fn (x, y, z) {};", @["x", "y", "z"])
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      let fe = dynamic_cast[FnExpression](program.statements[0].ExpressionStatement.expression)
      check:
        fe != nil
        len(program.statements) == 1
        len(fe.parameters) == len(test.expected_params)

      for i, ep in test.expected_params.pairs:
        check test_literal(fe.parameters[i], ep)

  test "test fn call expression parsing":
    let test = "add(1, 2 * 3, 4 + 5);"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let fce = dynamic_cast[FnCallExpression](program.statements[0].ExpressionStatement.expression)
    check:
      fce != nil
      len(program.statements) == 1
      test_literal(fce.function, "add")
      len(fce.arguments) == 3
      test_literal(fce.arguments[0], 1)
      test_infix(fce.arguments[1], "*", 2, 3)
      test_infix(fce.arguments[2], "+", 4, 5)


  test "test fn call expression precedence parsing":
    let tests = @[
      test_precedencen("a + add(b * c) + d;", "(a + (add((b * c)) + d))"),
      test_precedencen("add(a, b + c, 1 * (2 + 3) / 5, add(5, 1 + 2));", "add(a, (b + c), ((1 * (2 + 3)) / 5), add(5, (1 + 2)))"),
      test_precedencen("add(a + b + c * d / f + g);", "add((a + (b + (((c * d) / f) + g))))")
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        program.string() == test.expected & "\n"