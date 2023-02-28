import ../lexer/lexer
import ../parser/parser
import ../eval/eval
import ../obj/obj

proc repl*() =
  echo "\t- Interpreter v0.1 -"
  while true:
    stdout.write "-> "
    let input = stdin.readLine()
    if input == ".exit": break

    let lexer = lexer.lexer(input)
    var parser = parser.parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()

    let obj = eval.eval(program)
    if obj != nil:
      echo obj.inspect() & "\n"
    else:
      echo program.string()