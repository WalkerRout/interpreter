import std/tables
import std/strutils

import parser

type
  ObjectType* = string
  Object* = ref object of RootObj
    object_type*: ObjectType

  Environment* = ref object
    store*: tables.Table[system.string, Object]
    outer*: Environment

  IntegerObject* = ref object of Object
    value*: int64

  BooleanObject* = ref object of Object
    value*: bool

  ReturnObject* = ref object of Object
    value*: Object

  NullObject* = ref object of Object

  ErrorObject* = ref object of Object
    msg*: string

  FunctionObject* = ref object of Object
    parameters*: seq[parser.Node] # invariant: nodes are identifier expressions
    body*: parser.BlockStatement
    env*: Environment

const
  INTEGER_OBJ* = "INTEGER"
  BOOLEAN_OBJ* = "BOOLEAN"
  RETURN_OBJ* = "OBJECT"
  NULL_OBJ* = "NULL"
  ERROR_OBJ* = "ERROR"
  FUNCTION_OBJ* = "FUNCTION"

# do not need to create new object each time object is evaluated
let
  NULL* = NullObject(object_type: NULL_OBJ)
  TRUE* = BooleanObject(object_type: BOOLEAN_OBJ, value: true)
  FALSE* = BooleanObject(object_type: BOOLEAN_OBJ, value: false)

# object procs
method inspect*(o: Object): string {.base.} =
  raise newException(Exception, "PURE VIRTUAL CALL")

# environment procs
proc environment*(): Environment =
  Environment(store: tables.initTable[system.string, Object](), outer: nil)

proc environment*(o: Environment): Environment =
  Environment(store: tables.initTable[system.string, Object](), outer: o)

proc environment*(s: tables.Table[system.string, Object]): Environment =
  Environment(store: s, outer: nil)

proc environment*(s: tables.Table[system.string, Object], o: Environment): Environment =
  Environment(store: s, outer: o)

proc get*(e: Environment, key: string): Object =
  result = e.store.getOrDefault(key, nil)
  if result == nil and e.outer != nil:
    result = e.outer.get(key)

proc set*(e: var Environment, key: string, value: Object): Object =
  e.store[key] = value
  return value

# integer object procs
proc integer_object*(v: int64): IntegerObject =
  IntegerObject(object_type: INTEGER_OBJ, value: v)

method inspect*(io: IntegerObject): string =
  $io.value

# boolean object procs
proc boolean_object*(v: bool): BooleanObject =
  if v: TRUE else: FALSE

method inspect*(bo: BooleanObject): string =
  $bo.value

# boolean object procs
proc return_object*(v: Object): ReturnObject =
  ReturnObject(object_type: RETURN_OBJ, value: v)

method inspect*(ro: ReturnObject): string =
  ro.value.inspect()

# null object procs
proc null_object*(): NullObject =
  NULL

method inspect*(bo: NullObject): string =
  "nil"

# error object procs
proc error_object*(args: varargs[string]): ErrorObject =
  var m: string
  for arg in args:
    m &= arg
  ErrorObject(object_type: ERROR_OBJ, msg: m)

method inspect*(eo: ErrorObject): string =
  eo.msg

# function object procs
proc function_object*(p: seq[parser.Node], b: parser.BlockStatement, e: obj.Environment): FunctionObject =
  FunctionObject(object_type: FUNCTION_OBJ, parameters: p, body: b, env: e)

method inspect*(fo: FunctionObject): string =
  var params: string
  for param in fo.parameters:
    params.add(param.string())
    params.add(", ")
  params.delete(len(params)-2..<len(params))

  "fn (" & params & "):\n" & fo.body.string() & "\n"

