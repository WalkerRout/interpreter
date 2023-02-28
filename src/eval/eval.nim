import std/unittest

import ../util/util
import ../obj/obj
import ../token/token
import ../lexer/lexer
import ../parser/parser

# do not need to create new object each time object is evaluated
let
  NULL* = obj.null_object()
  TRUE* = obj.boolean_object(true)
  FALSE* = obj.boolean_object(false)

# forward declarations
proc eval_statements(statements: seq[Node]): obj.Object

# procs
proc eval*(n: parser.Node): obj.Object =
  if n of parser.Program:
    let node = n.Program
    result = eval_statements(node.statements)

  elif n of parser.ExpressionStatement:
    let node = n.ExpressionStatement
    result = eval(node.expression)

  elif n of parser.IntegerLiteralExpression:
    let node = n.IntegerLiteralExpression
    result = obj.integer_object(node.value)

  elif n of parser.BooleanExpression:
    let node = n.BooleanExpression
    result = if node.value: TRUE else: FALSE

  else:
    result = nil

proc eval_statements(statements: seq[Node]): obj.Object =
  for statement in statements:
    result = eval(statement)

suite "test eval":
  setup:
    type
      TestIntegerObject = object
        input: string
        expected: int64

      TestBooleanObject = object
        input: string
        expected: bool

      TestNullObject = object
        input: string

    proc test_integer_objectn(i: string, e: int64): TestIntegerObject =
      TestIntegerObject(input: i, expected: e)

    proc test_boolean_objectn(i: string, e: bool): TestBooleanObject =
      TestBooleanObject(input: i, expected: e)

    proc test_null_objectn(i: string): TestBooleanObject =
      TestBooleanObject(input: i)

    proc test_eval(input: string): obj.Object =
      let lexer = lexer.lexer(input)
      var parser = parser.parser(lexer)
      let program = parser.parse_program()

      eval(program.Node)

    proc test_integer_object(o: obj.Object, expected: int64): bool =
      let io = dynamic_cast[obj.IntegerObject](o)
      result = io != nil
      result = result and io.value == expected

    proc test_boolean_object(o: obj.Object, expected: bool): bool =
      let bo = dynamic_cast[obj.BooleanObject](o)
      result = bo != nil
      result = result and bo.value == expected

  test "test integer object evaluation":
    let tests = @[
      test_integer_objectn("5;", 5),
      test_integer_objectn("120;", 120)
    ]
    for test in tests:
      let evaluated_obj = test_eval(test.input)
      check test_integer_object(evaluated_obj, test.expected)

  test "test boolean object evaluation":
    let tests = @[
      test_boolean_objectn("true;", true),
      test_boolean_objectn("false;", false)
    ]
    for test in tests:
      let evaluated_obj = test_eval(test.input)
      check test_boolean_object(evaluated_obj, test.expected)
