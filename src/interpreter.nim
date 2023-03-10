import std/os

import repl
import token
import lexer
import parser
import obj
import eval
import util

proc evaluate(path: string): string =
  result = "Unable to evaluate program!"
  var file: string
  try:
    file = readFile(path)
  except: 
    return "File does not exist! Please try again"

  let lexer = lexer.lexer(file)
  var parser = parser.parser(lexer)
  var env = obj.environment()

  let obj = util.benchmark "program evaluation":
    let program = parser.parse_program()
    parser.check_parser_errors()
    eval.eval(program, env)

  if obj != nil:
    result = obj.inspect()

proc main() =
  if paramCount() >= 1:
    let arg = paramStr(1)
    if arg == "test":
      echo "Testing Interpreter!"
    else:
      echo evaluate(arg)
  else:
    echo "Interpreter v0.1 REPL running...\n"
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
