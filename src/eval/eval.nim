import std/unittest

import ../util/util
import ../obj/obj
import ../token/token
import ../lexer/lexer
import ../parser/parser

# forward declarations
proc eval_program(statements: seq[Node]): obj.Object
proc eval_block(statements: seq[Node]): obj.Object
proc eval_prefix_operator(operator: string, right: obj.Object): obj.Object
proc eval_bang_operator(right: obj.Object): obj.Object
proc eval_negative_operator(right: obj.Object): obj.Object
proc eval_infix_operator(operator: string, left, right: obj.Object): obj.Object
proc eval_integer_object_infix_operator(operator: string, left, right: obj.Object): obj.Object
proc eval_if(n: parser.IfExpression): obj.Object
proc truthy(o: obj.Object): bool

# procs
proc eval*(n: parser.Node): obj.Object =
  if n of parser.Program:
    let node = n.Program
    eval_program(node.statements)

  elif n of parser.ExpressionStatement:
    let node = n.ExpressionStatement
    eval(node.expression)

  elif n of parser.IntegerLiteralExpression:
    let node = n.IntegerLiteralExpression
    obj.integer_object(node.value)

  elif n of parser.BooleanExpression:
    let node = n.BooleanExpression
    if node.value: obj.TRUE else: obj.FALSE

  elif n of parser.PrefixExpression:
    let node = n.PrefixExpression
    let right = eval(node.value)
    eval_prefix_operator(node.operator, right)

  elif n of parser.InfixExpression:
    let node = n.InfixExpression
    let left = eval(node.left_value)
    let right = eval(node.right_value)
    eval_infix_operator(node.operator, left, right)

  elif n of parser.IfExpression:
    eval_if(n.IfExpression)

  elif n of parser.BlockStatement:
    let node = n.BlockStatement
    eval_block(node.statements)

  elif n of parser.ReturnStatement:
    let right = eval(n.ReturnStatement.value)
    obj.return_object(right)

  else:
    obj.NULL

proc eval_program(statements: seq[Node]): obj.Object =
  for statement in statements:
    result = eval(statement)
    if result of obj.ReturnObject:
      return result.ReturnObject.value

proc eval_block(statements: seq[Node]): obj.Object =
  for statement in statements:
    result = eval(statement)
    if result != nil and result of obj.ReturnObject:
      return result

proc eval_prefix_operator(operator: string, right: obj.Object): obj.Object =
  case operator
  of "!":
    eval_bang_operator(right)
  of "-":
    eval_negative_operator(right)
  else:
    obj.NULL

proc eval_bang_operator(right: obj.Object): obj.Object =
  if right of obj.BooleanObject:
    if right.BooleanObject == obj.TRUE: obj.FALSE else: obj.TRUE
  elif right of obj.IntegerObject:
    if right.IntegerObject.value >= 1: obj.FALSE else: obj.TRUE
  elif right of obj.NullObject:
    obj.TRUE
  else:
    obj.FALSE

proc eval_negative_operator(right: obj.Object): obj.Object =
  if right.object_type != obj.INTEGER_OBJ:
    return obj.NULL

  let value = right.IntegerObject.value
  integer_object(-value)

proc eval_infix_operator(operator: string, left, right: obj.Object): obj.Object =
  if left.object_type == obj.INTEGER_OBJ and right.object_type == obj.INTEGER_OBJ:
    eval_integer_object_infix_operator(operator, left, right)
  elif operator == "==":
    # intentional address comparison: the values obj.TRUE and obj.FALSE and both references
    obj.boolean_object(left == right)
  elif operator == "!=":
    obj.boolean_object(left != right)
  else:
    obj.NULL

proc eval_integer_object_infix_operator(operator: string, left, right: obj.Object): obj.Object =
  let left_value = left.IntegerObject.value
  let right_value = right.IntegerObject.value

  case operator
  of "+":
    integer_object(left_value + right_value)
  of "-":
    integer_object(left_value - right_value)
  of "*":
    integer_object(left_value * right_value)
  of "/":
    let v = left_value.int / right_value.int
    integer_object(v.int64)
  of "<":
    boolean_object(left_value < right_value)
  of ">":
    boolean_object(left_value > right_value)
  of "==":
    boolean_object(left_value == right_value)
  of "!=":
    boolean_object(left_value != right_value)
  else:
    obj.NULL

proc eval_if(n: parser.IfExpression): obj.Object =
  let condition = eval(n.condition)
  if truthy(condition):
    eval(n.consequence)
  elif n.alternative != nil:
    eval(n.alternative)
  else:
    obj.NULL

proc truthy(o: obj.Object): bool =
  if o of obj.NullObject:
    false
  elif o of obj.BooleanObject:
    o.BooleanObject.value
  elif o of obj.IntegerObject:
    if o.IntegerObject.value >= 1: true else: false
  else:
    true # truthy

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

      TestIfObject[T] = object
        input: string
        expected: T

      TestBangOperator = object
        input: string
        expected: bool

    proc test_integer_objectn(i: string, e: int64): TestIntegerObject =
      TestIntegerObject(input: i, expected: e)

    proc test_boolean_objectn(i: string, e: bool): TestBooleanObject =
      TestBooleanObject(input: i, expected: e)

    proc test_null_objectn(i: string): TestNullObject =
      TestNullObject(input: i)

    proc test_if_objectn[T](i: string, e: T): TestIfObject[T] =
      TestIfObject[T](input: i, expected: e)

    proc test_bang_operatorn(i: string, e: bool): TestBangOperator =
      TestBangOperator(input: i, expected: e)

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

    proc test_null_object(o: obj.Object): bool =
      let no = dynamic_cast[obj.NullObject](o)
      result = no != nil
      result = result and no == obj.NULL

  test "test integer object evaluation":
    let tests = @[
      test_integer_objectn("5;", 5),
      test_integer_objectn("120;", 120),
      test_integer_objectn("-5;", -5),
      test_integer_objectn("-120;", -120),
      test_integer_objectn("5 + 5 + 5 + 5 - 10", 10),
      test_integer_objectn("2 * 2 * 2 * 2 * 2", 32),
      test_integer_objectn("-50 + 100 + -50", 0),
      test_integer_objectn("5 * 2 + 10", 20),
      test_integer_objectn("5 + 2 * 10", 25),
      test_integer_objectn("20 + 2 * -10", 0),
      test_integer_objectn("50 / 2 * 2 + 10", 60),
      test_integer_objectn("2 * (5 + 10)", 30),
      test_integer_objectn("3 * 3 * 3 + 10", 37),
      test_integer_objectn("3 * (3 * 3) + 10", 37),
      test_integer_objectn("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
    ]
    for test in tests:
      let evaluated_obj = test_eval(test.input)
      check test_integer_object(evaluated_obj, test.expected)

  test "test boolean object evaluation":
    let tests = @[
      test_boolean_objectn("true;", true),
      test_boolean_objectn("false;", false),
      test_boolean_objectn("1 < 2", true),
      test_boolean_objectn("1 > 2", false),
      test_boolean_objectn("1 < 1", false),
      test_boolean_objectn("1 > 1", false),
      test_boolean_objectn("1 == 1", true),
      test_boolean_objectn("1 != 1", false),
      test_boolean_objectn("1 == 2", false),
      test_boolean_objectn("1 != 2", true),
      test_boolean_objectn("true == true", true),
      test_boolean_objectn("false == false", true),
      test_boolean_objectn("true == false", false),
      test_boolean_objectn("true != false", true),
      test_boolean_objectn("false != true", true),
      test_boolean_objectn("(1 < 2) == true", true),
      test_boolean_objectn("(1 < 2) == false", false),
      test_boolean_objectn("(1 > 2) == true", false),
      test_boolean_objectn("(1 > 2) == false", true)
    ]
    for test in tests:
      let evaluated_obj = test_eval(test.input)
      check test_boolean_object(evaluated_obj, test.expected)

  test "test if object evaluation":
    let tests_int = @[
      test_if_objectn("if (true) { 10 }", 10),
      test_if_objectn("if (1) { 10 }", 10),
      test_if_objectn("if (1 < 2) { 10 }", 10),
      test_if_objectn("if (1 > 2) { 10 } else { 20 }", 20),
      test_if_objectn("if (1 < 2) { 10 } else { 20 }", 10)
    ]
    let tests_bool = @[
      test_if_objectn("if (true) { 10 == 10; }", true),
      test_if_objectn("if (true) { 10 == 20; }", false),
    ]
    let tests_nil = @[
      test_if_objectn[obj.Object]("if (false) { 10 }", nil),
      test_if_objectn[obj.Object]("if (1 > 2) { 10 }", nil)
    ]
    for test in tests_int:
      let evaluated_obj = test_eval(test.input)
      check test_integer_object(evaluated_obj, test.expected)

    for test in tests_bool:
      let evaluated_obj = test_eval(test.input)
      check test_boolean_object(evaluated_obj, test.expected)

    for test in tests_nil:
      let evaluated_obj = test_eval(test.input)
      check test_null_object(evaluated_obj)

  test "test bang operator evaluation":
    let tests = @[
      test_bang_operatorn("!true;", false),
      test_bang_operatorn("!false;", true),
      test_bang_operatorn("!5;", false),
      test_bang_operatorn("!-5;", true),
      test_bang_operatorn("!!true;", true),
      test_bang_operatorn("!!false;", false),
      test_bang_operatorn("!!5;", true),
      test_bang_operatorn("!!-5;", false)
    ]
    for test in tests:
      let evaluated_obj = test_eval(test.input)
      check test_boolean_object(evaluated_obj, test.expected)

  test "test if object evaluation":
    let tests_int = @[
      test_integer_objectn("return 10;", 10),
      test_integer_objectn("return 10; 9;", 10),
      test_integer_objectn("return 2 * 5; 9;", 10),
      test_integer_objectn("9; return 2 * 5; 9;", 10),
      test_integer_objectn("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10)
    ]
    let tests_bool = @[
      test_boolean_objectn("return 20 > 21;", false),
      test_boolean_objectn("return false != true;", true),
    ]
    let tests_nil = @[
      test_null_objectn("return;"),
      test_null_objectn("8; return;")
    ]
    for test in tests_int:
      let evaluated_obj = test_eval(test.input)
      check test_integer_object(evaluated_obj, test.expected)

    for test in tests_bool:
      let evaluated_obj = test_eval(test.input)
      check test_boolean_object(evaluated_obj, test.expected)

    for test in tests_nil:
      let evaluated_obj = test_eval(test.input)
      check test_null_object(evaluated_obj)