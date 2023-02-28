import std/unittest

type
  ObjectType* = string
  Object* = ref object of RootObj
    object_type*: ObjectType

  IntegerObject* = ref object of Object
    value*: int64

  BooleanObject* = ref object of Object
    value*: bool

  ReturnObject* = ref object of Object
    value*: Object

  NullObject* = ref object of Object

  ErrorObject* = ref object of Object
    msg*: string

const
  INTEGER_OBJ* = "INTEGER"
  BOOLEAN_OBJ* = "BOOLEAN"
  RETURN_OBJ* = "OBJECT"
  NULL_OBJ* = "NULL"
  ERROR_OBJ* = "ERROR"

# do not need to create new object each time object is evaluated
let
  NULL* = NullObject(object_type: NULL_OBJ)
  TRUE* = BooleanObject(object_type: BOOLEAN_OBJ, value: true)
  FALSE* = BooleanObject(object_type: BOOLEAN_OBJ, value: false)

# object procs
method inspect*(o: Object): string {.base.} =
  raise newException(Exception, "PURE VIRTUAL CALL")

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
proc error_object*(m: string): ErrorObject =
  ErrorObject(object_type: ERROR_OBJ, msg: m)

method inspect*(eo: ErrorObject): string =
  eo.msg

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