import ../lexer/lexer
import ../parser/parser
import ../eval/eval
import ../obj/obj

type Status {.pure.} = enum
  stBreak
  stContinue
  stIgnore

proc check_command(input: string): Status
proc clear_terminal()

proc repl*() =
  echo "\t- Interpreter v0.1 -"
  var env = obj.environment()

  while true:
    stdout.write "-> "

    let input = stdin.readLine()
    let status = check_command(input)
    case status
    of stBreak:
      break
    of stContinue:
      continue
    of stIgnore:
      discard

    let lexer = lexer.lexer(input)
    var parser = parser.parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()

    let obj = eval.eval(program, env)
    if obj != nil:
      echo obj.inspect()

proc check_command(input: string): Status =
  case input
  of ".exit":
    echo "Interpreter shutting down..."
    stBreak
  of ".clear":
    clear_terminal()
    stContinue
  of ".evaluate":
    let file_name = stdin.readLine()
    var file: string
    try:
      file = readFile(file_name)
    except: 
      echo "File does not exist! Please try again"
      return stContinue

    let lexer = lexer.lexer(file)
    var parser = parser.parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    
    var env = obj.environment()
    let obj = eval.eval(program, env)
    if obj != nil:
      echo obj.inspect()

    stContinue
  else:
    stIgnore

proc clear_terminal() =
  stdout.write "\x1B[2J\x1B[1;1H"

proc ctrlc*() {.noconv.} =
  stdout.write "Interpreter shutting down..."
  quit(0)
setControlCHook(ctrlc)