import std/unittest

import ../util/util
import ../obj/obj
import ../token/token
import ../lexer/lexer
import ../parser/parser

# forward declarations
proc eval_program(statements: seq[Node], env: var obj.Environment): obj.Object
proc eval_block(statements: seq[Node], env: var obj.Environment): obj.Object
proc eval_prefix_operator(operator: string, right: obj.Object): obj.Object
proc eval_bang_operator(right: obj.Object): obj.Object
proc eval_negative_operator(right: obj.Object): obj.Object
proc eval_identifier(ie: parser.IdentifierExpression, env: var obj.Environment): obj.Object
proc eval_infix_operator(operator: string, left, right: obj.Object): obj.Object
proc eval_integer_object_infix_operator(operator: string, left, right: obj.Object): obj.Object
proc eval_expressions(expressions: seq[Node], env: var Environment): seq[obj.Object]
proc eval_if(ie: parser.IfExpression, env: var obj.Environment): obj.Object
proc apply_function(fo: obj.FunctionObject, args: seq[obj.Object]): obj.Object
proc extend_function_env(fo: obj.FunctionObject, args: seq[obj.Object]): Environment
proc unwrap_return(o: obj.Object): obj.Object
proc truthy(o: obj.Object): bool
proc is_error(o: obj.Object): bool

# procs
proc eval*(n: parser.Node, env: var obj.Environment): obj.Object =
  if n of parser.Program:
    let node = n.Program
    eval_program(node.statements, env)

  elif n of parser.ExpressionStatement:
    let node = n.ExpressionStatement
    eval(node.expression, env)

  elif n of parser.IntegerLiteralExpression:
    let node = n.IntegerLiteralExpression
    obj.integer_object(node.value)

  elif n of parser.BooleanExpression:
    let node = n.BooleanExpression
    if node.value: obj.TRUE else: obj.FALSE

  elif n of parser.IdentifierExpression:
    let node = n.IdentifierExpression
    eval_identifier(node, env)

  elif n of parser.PrefixExpression:
    let node = n.PrefixExpression
    let right = eval(node.value, env)
    if is_error(right): return right
    eval_prefix_operator(node.operator, right)

  elif n of parser.InfixExpression:
    let node = n.InfixExpression
    let left = eval(node.left_value, env)
    if is_error(left): return left
    let right = eval(node.right_value, env)
    if is_error(right): return right
    eval_infix_operator(node.operator, left, right)

  elif n of parser.IfExpression:
    eval_if(n.IfExpression, env)

  elif n of parser.FnExpression:
    let node = n.FnExpression
    obj.function_object(node.parameters, node.body, env)

  elif n of parser.FnCallExpression:
    let node = n.FnCallExpression
    let function = eval(node.function, env)
    if is_error(function): return function

    let args = eval_expressions(node.arguments, env)
    if len(args) == 1 and is_error(args[0]):
      return args[0]

    apply_function(function.FunctionObject, args)

  elif n of parser.BlockStatement:
    let node = n.BlockStatement
    eval_block(node.statements, env)

  elif n of parser.ReturnStatement:
    let right = eval(n.ReturnStatement.value, env)
    if is_error(right): return right
    obj.return_object(right)

  elif n of parser.LetStatement:
    let node = n.LetStatement
    let val = eval(node.value, env)
    if is_error(val): return val
    env.set(node.name.value, val)

  else:
    obj.NULL

proc eval_program(statements: seq[Node], env: var obj.Environment): obj.Object =
  for statement in statements:
    result = eval(statement, env)
    if result of obj.ReturnObject:
      return result.ReturnObject.value
    elif result of obj.ErrorObject:
      return result

proc eval_block(statements: seq[Node], env: var obj.Environment): obj.Object =
  for statement in statements:
    result = eval(statement, env)
    if result != nil and result of obj.ReturnObject or result of obj.ErrorObject:
      return result

proc eval_prefix_operator(operator: string, right: obj.Object): obj.Object =
  case operator
  of "!":
    eval_bang_operator(right)
  of "-":
    eval_negative_operator(right)
  else:
    obj.error_object "unknown operator: ", operator, right.object_type

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
    return obj.error_object("unknown operator: -", right.object_type)

  let value = right.IntegerObject.value
  integer_object(-value)

proc eval_identifier(ie: parser.IdentifierExpression, env: var obj.Environment): obj.Object =
  result = env.get(ie.value)
  if result == nil:
    result = error_object("identifier not found: ", ie.value)

proc eval_infix_operator(operator: string, left, right: obj.Object): obj.Object =
  if left.object_type == obj.INTEGER_OBJ and right.object_type == obj.INTEGER_OBJ:
    eval_integer_object_infix_operator(operator, left, right)
  elif operator == "==":
    # intentional address comparison: the values obj.TRUE and obj.FALSE and both references
    obj.boolean_object(left == right)
  elif operator == "!=":
    obj.boolean_object(left != right)
  else:
    if left.object_type != right.object_type:
      obj.error_object "conflicting types: ", left.object_type, " ", operator, " ", right.object_type
    else:
      obj.error_object "unknown operator: ", left.object_type, " ", operator, " ", right.object_type

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
    obj.error_object "unknown operator: ", left.object_type, " ", operator, " ", right.object_type

proc eval_expressions(expressions: seq[Node], env: var Environment): seq[obj.Object] =
  for expression in expressions:
    let eval = eval(expression, env)
    if is_error(eval): return @[eval]
    result.add(eval)

proc apply_function(fo: obj.FunctionObject, args: seq[obj.Object]): obj.Object =
  var extenv = extend_function_env(fo, args)
  unwrap_return(eval(fo.body, extenv))

proc extend_function_env(fo: obj.FunctionObject, args: seq[obj.Object]): Environment =
  result = obj.environment(fo.env)
  for i, param in fo.parameters:
    discard result.set(param.IdentifierExpression.value, args[i])

proc unwrap_return(o: obj.Object): obj.Object =
  let ro = dynamic_cast[ReturnObject](o)
  if ro != nil: ro.value else: o

proc eval_if(ie: parser.IfExpression, env: var obj.Environment): obj.Object =
  let condition = eval(ie.condition, env)
  if is_error(condition): return condition

  if truthy(condition):
    eval(ie.consequence, env)
  elif ie.alternative != nil:
    eval(ie.alternative, env)
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

proc is_error(o: obj.Object): bool =
  result = false
  if o != nil:
    result = o.object_type == obj.ERROR_OBJ

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

      TestErrorObject = object
        input: string
        msg: string

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

    proc test_error_objectn(i, m: string): TestErrorObject =
      TestErrorObject(input: i, msg: m)

    proc test_bang_operatorn(i: string, e: bool): TestBangOperator =
      TestBangOperator(input: i, expected: e)

    proc test_eval(input: string): obj.Object =
      let lexer = lexer.lexer(input)
      var parser = parser.parser(lexer)
      let program = parser.parse_program()
      var env = obj.environment()
      eval(program.Node, env)

    proc test_integer_object(o: obj.Object, expected: int64): bool =
      let io = dynamic_cast[obj.IntegerObject](o)
      if io == nil: return false
      result = io.value == expected

    proc test_boolean_object(o: obj.Object, expected: bool): bool =
      let bo = dynamic_cast[obj.BooleanObject](o)
      if bo == nil: return false
      result = bo.value == expected

    proc test_null_object(o: obj.Object): bool =
      let no = dynamic_cast[obj.NullObject](o)
      if no == nil: return false
      result = no == obj.NULL

    proc test_error_object(o: obj.Object, expected: string): bool =
      let eo = dynamic_cast[ErrorObject](o)
      if eo == nil: return false
      result = eo.msg == expected

    proc test_function_object(o: obj.Object, p: seq[string], eb: string): bool =
      let fo = dynamic_cast[FunctionObject](o)
      if fo == nil: return false
      result = len(fo.parameters) == len(p)
      for i, param in fo.parameters.pairs:
        result = result and param.IdentifierExpression.value == p[i]
      result = result and fo.body.string() == eb

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

  test "test error handling":
    let tests = @[
      test_error_objectn("foobar;", "identifier not found: foobar"),
      test_error_objectn("5 + true;", "conflicting types: INTEGER + BOOLEAN"),
      test_error_objectn("5 + true; 5;", "conflicting types: INTEGER + BOOLEAN"),
      test_error_objectn("-true", "unknown operator: -BOOLEAN"),
      test_error_objectn("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
      test_error_objectn("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
      test_error_objectn("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
      test_error_objectn("if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", "unknown operator: BOOLEAN + BOOLEAN")
    ]
    for test in tests:
      let evaluated_obj = test_eval(test.input)
      check test_error_object(evaluated_obj, test.msg)

  test "test let object evaluation":
    let tests_int = @[
      test_integer_objectn("let a = 5; a;", 5),
      test_integer_objectn("let a = 5 * 5; a;", 25),
      test_integer_objectn("let a = 5; let b = a; b;", 5),
      test_integer_objectn("let a = 5; let b = a; let c = a + b + 5; c;", 15)
    ]
    let tests_bool = @[
      test_boolean_objectn("let a = true; a;", true),
      test_boolean_objectn("let a = false; a;", false),
      test_boolean_objectn("let a = 5 > 2; let b = true; a == b;", true),
      test_boolean_objectn("let a = 2 != 2; let b = false; a != b;", false)
    ]
    for test in tests_int:
      let evaluated_obj = test_eval(test.input)
      check test_integer_object(evaluated_obj, test.expected)

    for test in tests_bool:
      let evaluated_obj = test_eval(test.input)
      check test_boolean_object(evaluated_obj, test.expected)

  test "test function object evaluation":
    let test = "fn(x) { x + 2; };"
    let evaluated_obj = test_eval(test)
    check test_function_object(evaluated_obj, @["x"], "(x + 2)\n")

  test "test function call object evaluation":
    let tests_int = @[
      test_integer_objectn("let identity = fn(x) { x; }; identity(5);", 5),
      test_integer_objectn("let identity = fn(x) { return x; }; identity(5);", 5),
      test_integer_objectn("let double = fn(x) { x * 2; }; double(5);", 10),
      test_integer_objectn("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
      test_integer_objectn("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
      test_integer_objectn("fn(x) { x; }(5)", 5)
    ]
    let tests_bool = @[
      test_boolean_objectn("let identity = fn(x) { x; }; identity(false);", false),
      test_boolean_objectn("let identity = fn(x) { return x; }; identity(true);", true),
      test_boolean_objectn("let negate = fn(x) { !x; }; negate(true);", false),
      test_boolean_objectn("let eq = fn(x, y) { x == y; }; eq(5, 5);", true),
      test_boolean_objectn("let eq = fn(x, y) { x == y; }; eq(true, eq(2, 3));", false),
      test_boolean_objectn("fn(x) { x; }(false)", false)
    ]
    for test in tests_int:
      let evaluated_obj = test_eval(test.input)
      check test_integer_object(evaluated_obj, test.expected)

    for test in tests_bool:
      let evaluated_obj = test_eval(test.input)
      check test_boolean_object(evaluated_obj, test.expected)