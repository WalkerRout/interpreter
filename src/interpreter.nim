import repl/repl
import token/token
import lexer/lexer
import parser/parser
import obj/obj
import eval/eval

proc main() =
  echo "Interpreter running...\n"
  repl.repl()

when isMainModule:
  main()

#[
# TODO
> proc boolean_object*(v: bool): BooleanObject =
    BooleanObject(object_type: BOOLEAN_OBJ, value: v)

  can be converted into

  proc boolean_object*(v: bool): BooleanObject =
    new result
    result.object_type = BOOLEAN_OBJ
    result.value = v

  for more concise constructors
]#