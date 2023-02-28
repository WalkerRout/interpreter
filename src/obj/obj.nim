import std/unittest

type
  ObjectType* = string
  Object* = ref object of RootObj
    object_type*: ObjectType

  IntegerObject* = ref object of Object
    value*: int64

  BooleanObject* = ref object of Object
    value*: bool

  NullObject* = ref object of Object

const
  INTEGER_OBJ* = "INTEGER"
  BOOLEAN_OBJ* = "BOOLEAN"
  NULL_OBJ* = "NULL"

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
  BooleanObject(object_type: BOOLEAN_OBJ, value: v)

method inspect*(bo: BooleanObject): string =
  $bo.value

# null object procs
proc null_object*(): NullObject =
  NullObject(object_type: NULL_OBJ)

method inspect*(bo: NullObject): string =
  "<nil>"

suite "test object":
  test "test integer object":
    let io = integer_object(4)
    check:
      io.object_type == INTEGER_OBJ
      io.value == 4

  test "test boolean object":
    let bo = boolean_object(true)
    check:
      bo.object_type == BOOLEAN_OBJ
      bo.value == true

  test "test null object":
    let no = null_object()
    check:
      no.object_type == NULL_OBJ