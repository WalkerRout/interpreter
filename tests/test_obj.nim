import std/unittest
import std/tables
import std/strutils

import ../src/obj

import ../src/parser

suite "test object":
  test "test integer object":
    let io = integer_object(4)
    check:
      io.object_type == INTEGER_OBJ
      io.value == 4
      io.inspect() == "4"

  test "test boolean object":
    let bo = boolean_object(true)
    check:
      bo.object_type == BOOLEAN_OBJ
      bo.value == true
      bo.inspect() == "true"

  test "test null object":
    let no = null_object()
    check:
      no.object_type == NULL_OBJ

  test "test error object":
    let eo = error_object("error msg")
    check:
      eo.object_type == ERROR_OBJ
      eo.msg == "error msg"
      eo.inspect() == "error msg"

  test "test function object":
    let params: seq[Node] = @[]
    let env = Environment()
    let fo = function_object(params, nil, env)
    check:
      fo.object_type == FUNCTION_OBJ
      fo.parameters == params
      fo.body == nil
      fo.env == env