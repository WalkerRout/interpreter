
import util
import obj
import token
import lexer
import parser

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