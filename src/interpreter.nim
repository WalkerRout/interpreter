import repl/repl
import token/token
import lexer/lexer
import parser/parser
import obj/obj
import eval/eval

proc main() =
  echo "Interpreter running...\n"
  #repl.repl()

when isMainModule:
  main()
