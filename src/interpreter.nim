import repl/repl
import token/token
import lexer/lexer
import parser/parser

proc main() =
  echo "Interpreter running...\n"
  #repl.repl()

when isMainModule:
  main()