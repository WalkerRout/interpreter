import ../lexer/lexer
import ../parser/parser

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
    echo program.string()