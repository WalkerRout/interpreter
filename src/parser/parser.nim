import std/unittest
import std/tables
import std/strutils
import std/typetraits

import ../util/util
import ../token/token
import ../lexer/lexer

type
  Precedence = enum
    prLowest
    prEquals
    prLTGT
    prAddSub
    prDivMul
    prPrefix
    prCall

  NodeType* = enum
    ntStatement,
    ntExpression
  Node* = ref object of RootObj
    case node_type*: NodeType
    of ntStatement:
      statement_name*: string
    of ntExpression:
      expression_name*: string

  IdentifierExpression* = ref object of Node
    token*: token.Token
    value*: string

  IntegerLiteralExpression* = ref object of Node
    token*: token.Token
    value*: int64

  PrefixExpression* = ref object of Node
    token*: token.Token
    operator*: string
    value*: Node # invariant: must be an expression

  InfixExpression* = ref object of Node
    token*: token.Token
    operator*: string
    left_value*: Node # invariant: must be an expression
    right_value*: Node # invariant: must be an expression

  LetStatement* = ref object of Node
    token*: token.Token
    name*: IdentifierExpression
    value*: Node # invariant: must be an expression

  ReturnStatement* = ref object of Node
    token*: token.Token
    value*: Node # invariant: must be an expression

  ExpressionStatement* = ref object of Node
    token*: token.Token
    expression*: Node # invariant: must be an expression

  Error* = object
    msg: string
    src: string

  Program* = object
    statements*: seq[Node]

  PrefixParseFn* = proc(p: var Parser): Node
  InfixParseFn*  = proc(p: var Parser, node: Node): Node

  Parser* = object
    lexer*: lexer.Lexer
    curr_token*: token.Token
    next_token*: token.Token
    errors*: seq[Error]
    prefix_parse_fns*: tables.Table[token.TokenType, PrefixParseFn]
    infix_parse_fns*: tables.Table[token.TokenType, InfixParseFn]

const
  precedences = {
    token.EQ: Precedence.prEQUALS,
    token.NEQ: Precedence.prEQUALS,
    token.LT: Precedence.prLTGT,
    token.GT: Precedence.prLTGT,
    token.PLUS: Precedence.prAddSub,
    token.MINUS: Precedence.prAddSub,
    token.SLASH: Precedence.prDivMul,
    token.ASTERISK: Precedence.prDivMul 
  }.toTable

# forward declarations
proc parse_statement(p: var Parser): Node
proc parse_let_statement(p: var Parser): LetStatement
proc parse_return_statement(p: var Parser): ReturnStatement
proc parse_expression_statement(p: var Parser): ExpressionStatement
proc parse_expression(p: var Parser, pr: Precedence): Node
proc parse_identifier(p: var Parser): Node
proc parse_integer_literal(p: var Parser): Node
proc parse_prefix(p: var Parser): Node
proc parse_infix(p: var Parser, left: Node): Node
proc register_prefix(p: var Parser, token_type: token.TokenType, prfn: PrefixParseFn)
proc register_infix(p: var Parser, token_type: token.TokenType, infn: InfixParseFn)
proc lexer_next_token(p: var Parser)
proc curr_precedence(p: Parser): Precedence
proc next_precedence(p: Parser): Precedence
proc expect_peek(p: var Parser, token_type: token.TokenType): bool
proc peek_error(p: var Parser, token_type: token.TokenType)
proc no_prefix_parse_fn_error(p: var Parser, token_type: token.TokenType)
proc curr_token_is(p: Parser, token_type: token.TokenType): bool
proc next_token_is(p: Parser, token_type: token.TokenType): bool
proc check_parser_errors*(p: Parser)

# procedures
# node procs
method `$`(n: Node): string =
  if n != nil:
    case n.node_type
    of ntExpression:
      n.expression_name
    of ntStatement:
      n.statement_name
  else:
    "<nil>"

method token_literal*(n: Node): string {.base.} =
  raise newException(Exception, "PURE VIRTUAL CALL")

method string*(n: Node): string {.base.} =
  raise newException(Exception, "PURE VIRTUAL CALL")

# identifier expression procs
proc identifier_expression*(t: token.Token, v: string): IdentifierExpression =
  IdentifierExpression(node_type: ntExpression, expression_name: "IdentifierExpression",
                       token: t, value: v)

method token_literal*(ie: IdentifierExpression): string = 
  ie.token.literal

method string*(ie: IdentifierExpression): string = 
  ie.value

# integer literal expression procs
proc integer_literal_expression*(t: token.Token, v: string): IntegerLiteralExpression =
  IntegerLiteralExpression(node_type: ntExpression, expression_name: "IntegerLiteralExpression",
                           token: t, value: v.parseInt)

method token_literal*(ie: IntegerLiteralExpression): string = 
  ie.token.literal

method string*(ie: IntegerLiteralExpression): string = 
  $ie.value

# prefix expression procs
proc prefix_expression*(t: token.Token, o: string, v: Node): PrefixExpression =
  PrefixExpression(node_type: ntExpression, expression_name: "PrefixExpression",
                   token: t, operator: o, value: v)

method token_literal*(pe: PrefixExpression): string = 
  pe.token.literal

method string*(pe: PrefixExpression): string =
  "(" & pe.operator & pe.value.string() & ")"

# infix expression procs
proc infix_expression*(t: token.Token, o: string, lv, rv: Node): InfixExpression =
  InfixExpression(node_type: ntExpression, expression_name: "InfixExpression",
                  token: t, operator: o, left_value: lv, right_value: rv)

method token_literal*(ie: InfixExpression): string = 
  ie.token.literal

method string*(ie: InfixExpression): string =
  "(" & ie.left_value.string() & " " & ie.operator & " " & ie.right_value.string() & ")"

# let statement procs
proc let_statement*(t: token.Token, n: IdentifierExpression, v: Node): LetStatement =
  assert v.node_type == ntExpression # invariant
  LetStatement(node_type: ntStatement, statement_name: "LetStatement",
               token: t, name: n, value: v)

method token_literal*(ls: LetStatement): string = 
  ls.token.literal

method string*(ls: LetStatement): string =
  result = ls.token_literal() & " " & ls.name.string() & " = "
  if ls.value != nil:
    result &= ls.value.string()
  result &= ";"

# return statement procs
proc return_statement*(t: token.Token, v: Node): ReturnStatement =
  assert v.node_type == ntExpression # invariant
  ReturnStatement(node_type: ntStatement, statement_name: "ReturnStatement",
                  token: t, value: v)

method token_literal*(ls: ReturnStatement): string =
  ls.token.literal

method string*(rs: ReturnStatement): string =
  result = rs.token_literal() & " "
  if rs.value != nil:
    result &= rs.value.string()
  result &= ";"

# return statement procs
proc expression_statement*(t: token.Token, e: Node): ExpressionStatement =
  assert e.node_type == ntExpression # invariant
  ExpressionStatement(node_type: ntStatement, statement_name: "ExpressionStatement",
                      token: t, expression: e)

method token_literal*(es: ExpressionStatement): string =
  es.token.literal

method string*(es: ExpressionStatement): string =
  result = "<empty>"
  if es.expression != nil:
    result = es.expression.string()

# error procs
proc error(m: string): Error = 
  Error(msg: m, src: "")

proc error(m: string, s: string): Error = 
  Error(msg: m, src: s)

proc log_msg(e: Error) =
  if e.src == "": echo "Error: ", e.msg
  else: echo e.src, " - Error: ", e.msg

# program procs
proc program*(s: seq[Node]): Program = 
  Program(statements: s)

proc token_literal*(p: Program): string =
  if len(p.statements) > 0:
    result = p.statements[0].token_literal()

proc string*(p: Program): string =
  for statement in p.statements:
    result &= statement.string() & "\n"

# parser procs
proc parser*(l: Lexer): Parser =
  result = Parser(
    lexer: l,
    curr_token: DEFAULT_TOKEN,
    next_token: DEFAULT_TOKEN,
    errors: @[],
    prefix_parse_fns: tables.initTable[token.TokenType, PrefixParseFn](),
    infix_parse_fns: tables.initTable[token.TokenType, InfixParseFn]()
  )

  result.register_prefix(token.IDENT, parse_identifier)
  result.register_prefix(token.INT, parse_integer_literal)
  result.register_prefix(token.BANG, parse_prefix)
  result.register_prefix(token.MINUS, parse_prefix)

  result.register_infix(token.PLUS, parse_infix)
  result.register_infix(token.MINUS, parse_infix)
  result.register_infix(token.SLASH, parse_infix)
  result.register_infix(token.ASTERISK, parse_infix)
  result.register_infix(token.LT, parse_infix)
  result.register_infix(token.GT, parse_infix)
  result.register_infix(token.EQ, parse_infix)
  result.register_infix(token.NEQ, parse_infix)

  result.lexer_next_token()
  result.lexer_next_token()

proc parse_program*(p: var Parser): Program =
  result = program(@[])

  while p.curr_token.token_type != token.EOF:
    let statement = p.parse_statement()
    if statement != nil:
      result.statements.add(statement)
    p.lexer_next_token()

# invariant: returns a statement node
proc parse_statement(p: var Parser): Node =
  case p.curr_token.token_type
  of token.LET:
    result = p.parse_let_statement()
  of token.RETURN:
    result = p.parse_return_statement()
  else:
    result = p.parse_expression_statement()

proc parse_let_statement(p: var Parser): LetStatement =
  new result
  result.token = p.curr_token

  if not p.expect_peek(token.IDENT):
    return nil

  result.name = identifier_expression(p.curr_token, p.curr_token.literal)

  if not p.expect_peek(token.ASSIGN):
    return nil

  # todo uncomment below
  #p.lexer_next_token()
  #result.value = p.parse_expression(Precedence.prLowest)

  while not p.curr_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_return_statement(p: var Parser): ReturnStatement =
  new result
  result.token = p.curr_token

  p.lexer_next_token()

  # todo uncomment below
  #result.value = p.parse_expression(Precedence.prLowest)

  while not p.curr_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_expression_statement(p: var Parser): ExpressionStatement =
  new result
  result.token = p.curr_token
  result.expression = p.parse_expression(Precedence.prLowest)

  if p.next_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_expression(p: var Parser, pr: Precedence): Node =
  var prefix_fn = p.prefix_parse_fns.getOrDefault(p.curr_token.token_type, nil)
  if prefix_fn == nil:
    p.no_prefix_parse_fn_error(p.curr_token.token_type)
    return nil
  result = prefix_fn(p) # parse left side

  # break loop on equal precedence to assure only one expression of the same type is evaluated,
  # though still allow the expr's in the expr <op> expr pattern to represent something like expr = (expr0 <op> expr1)
  while not p.next_token_is(token.SEMICOLON) and pr < p.next_precedence():
    let infix_fn = p.infix_parse_fns.getOrDefault(p.next_token.token_type, nil)
    if infix_fn == nil: return
    p.lexer_next_token()
    # evaluate the next expr <op> expr pattern, and continue looping at lowest precedence 
    # -> (((expr <op> expr) <op> expr) <op> expr) etc..
    result = infix_fn(p, result) # parse right side, passing in already parsed left side

# jump table functions -> generic, return Node type
proc parse_identifier(p: var Parser): Node =
  identifier_expression(p.curr_token, p.curr_token.literal)

proc parse_integer_literal(p: var Parser): Node =
  var il = IntegerLiteralExpression()
  il.token = p.curr_token
  try:
    il.value = p.curr_token.literal.parseInt
  except: 
    p.errors.add(error("could not parse " & p.curr_token.literal & " as integer!"))
    return nil
  result = il

proc parse_prefix(p: var Parser): Node =
  var pe = PrefixExpression()
  pe.token = p.curr_token
  pe.operator = p.curr_token.literal

  p.lexer_next_token()
  pe.value = p.parse_expression(Precedence.prPrefix)
  result = pe

proc parse_infix(p: var Parser, left: Node): Node =
  var ie = InfixExpression()
  ie.token = p.curr_token
  ie.operator = p.curr_token.literal
  ie.left_value = left

  var pre = p.curr_precedence()
  p.lexer_next_token()

  # right associative on `+` operator -> decrement precedence
  if ie.operator == "+":
    dec pre

  ie.right_value = p.parse_expression(pre) 
  result = ie

proc register_prefix(p: var Parser, token_type: token.TokenType, prfn: PrefixParseFn) =
  p.prefix_parse_fns[token_type] = prfn

proc register_infix(p: var Parser, token_type: token.TokenType, infn: InfixParseFn) =
  p.infix_parse_fns[token_type] = infn

proc lexer_next_token(p: var Parser) = 
  p.curr_token = p.next_token
  p.next_token = p.lexer.next_token()

proc curr_precedence(p: Parser): Precedence =
  precedences.getOrDefault(p.curr_token.token_type, Precedence.prLowest)

proc next_precedence(p: Parser): Precedence =
  precedences.getOrDefault(p.next_token.token_type, Precedence.prLowest)

proc expect_peek(p: var Parser, token_type: token.TokenType): bool =
  if p.next_token_is(token_type):
    p.lexer_next_token()
    result = true
  else:
    p.peek_error(token_type)
    result = false

proc peek_error(p: var Parser, token_type: token.TokenType) =
  p.errors.add(error("expected next token to be " & token_type & " - instead got " & p.next_token.token_type))

proc no_prefix_parse_fn_error(p: var Parser, token_type: token.TokenType) =
  p.errors.add(error("no prefix parse function for " & token_type & " found"))

proc curr_token_is(p: Parser, token_type: token.TokenType): bool = 
  result = p.curr_token.token_type == token_type

proc next_token_is(p: Parser, token_type: token.TokenType): bool = 
  result = p.next_token.token_type == token_type

proc check_parser_errors*(p: Parser) =
  if len(p.errors) == 0: return
  echo "Parser had ", len p.errors, " errors:"
  for error in p.errors:
    error.log_msg()

suite "test parser":
  setup:
    type
      TestIdent = object
        expected_identifier: string

      TestPrefix = object
        input: string
        operator: string
        integer_value: int64

      TestInfix = object
        input: string
        operator: string
        left_value: int64
        right_value: int64

      TestPrecedence = object
        input: string
        expected: string

    proc test_ident(ei: string): TestIdent =
      TestIdent(expected_identifier: ei)

    proc test_prefix(i, o: string, iv: int64): TestPrefix =
      TestPrefix(input: i, operator: o, integer_value: iv)

    proc test_infix(i, o: string, lv, rv: int64): TestInfix =
      TestInfix(input: i, operator: o, left_value: lv, right_value: rv)

    proc test_precedence(i, e: string): TestPrecedence =
      TestPrecedence(input: i, expected: e)

    # tests for literals, not expression statements
    proc test_integer_literal(e: Node, value: int64): bool =
      let ile = dynamic_cast[IntegerLiteralExpression](e)
      result = ile != nil
      result = result and ile.value == value
      result = result and ile.token_literal() == $value

    proc test_identifier(e: Node, value: string): bool =
      let ie = dynamic_cast[IdentifierExpression](e)
      result = ie != nil
      result = result and ie.value == value
      result = result and ie.token_literal() == value

    proc test_literal(e: Node, value: auto): bool =
      when value.type.name == "int64":
        test_integer_literal(e, value)
      elif value.type.name == "string":
        test_identifier(e, value)
      else:
        echo "No literal test for type " & value.type.name
        false

    proc test_infix(e: Node, operator: string, left, right: auto): bool =
      let ie = dynamic_cast[InfixExpression](e)
      result = ie != nil
      result = result and test_literal(ie.left_value, left)
      result = result and ie.operator == operator
      result = result and test_literal(ie.right_value, right)
    # end tests for literals

    proc test_let_statement(s: Node, name: string): bool =
      let ls = dynamic_cast[LetStatement](s)
      result = ls != nil
      result = result and test_literal(ls.name, name)

    proc test_return_statement(s: Node): bool =
      let rs = dynamic_cast[ReturnStatement](s)
      result = rs != nil
      result = result and rs.token_literal == "return"

    proc test_identifier_expression(e: Node, id: string): bool =
      let es = dynamic_cast[ExpressionStatement](e)
      result = es != nil
      let ie = dynamic_cast[IdentifierExpression](es.expression)
      result = result and test_literal(ie, id)

    proc test_integer_literal_expression(e: Node, v: int64): bool =
      let es = dynamic_cast[ExpressionStatement](e)
      result = es != nil
      let ile = dynamic_cast[IntegerLiteralExpression](es.expression)
      result = result and ile != nil
      result = result and ile.value == v
      result = result and ile.token_literal() == $v

    proc test_prefix_expression(e: Node, tp: TestPrefix): bool =
      let es = dynamic_cast[ExpressionStatement](e)
      result = es != nil
      let pe = dynamic_cast[PrefixExpression](es.expression)
      result = result and pe != nil
      result = result and pe.operator == tp.operator
      result = result and test_integer_literal(pe.value, tp.integer_value)

    proc test_infix_expression(e: Node, ti: TestInfix): bool =
      let es = dynamic_cast[ExpressionStatement](e)
      result = es != nil
      let ie = dynamic_cast[InfixExpression](es.expression)
      result = result and ie != nil
      result = result and test_integer_literal(ie.left_value, ti.left_value)
      result = result and ie.operator == ti.operator
      result = result and test_integer_literal(ie.right_value, ti.right_value)

  test "test let statement parsing":
    let input = """
      let x = 5;
      let y = 10;
      let foobar = 20040605;
    """
    let tests = @[
      test_ident("x"),
      test_ident("y"),
      test_ident("foobar")
    ]
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    check len(program.statements) == 3

    for i, test in tests.pairs:
      let statement_node = program.statements[i]
      check test_let_statement(statement_node, test.expected_identifier)

  test "test error logging":
    let input = """
      let x 5;
      let = 10;
      let 20040605;
    """
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    #check_parser_errors(parser) #commment/uncomment to start/stop the test
    check len(program.statements) == 4 # there are 4 expression statements (-> x, 5, 10, 20040605)

  test "test return statement parsing":
    let input = """
      return 5;
      return foobar;
      return add(15, 15);
    """
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    check len(program.statements) == 3

    for i in 0..<3:
      let statement_node = program.statements[i]
      check test_return_statement(statement_node)

  test "test string parsing":
    let s = let_statement(
      token.token(token.LET, "let"),
      identifier_expression(
        token.token(token.IDENT, "variable_a"),
        "variable_a"
      ),
      identifier_expression(
        token.token(token.IDENT, "variable_b"),
        "variable_b"
      )
    ) # end of let_statement
    let p = program(@[Node(s)])
    check p.string() == "let variable_a = variable_b;\n"

  test "test identifier expression parsing":
    let input = """
      foobar;
    """
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    check:
      len(program.statements) == 1
      test_identifier_expression(program.statements[0], "foobar")

  test "test integer literal expression parsing":
    let input = """
      5;
    """
    let lexer = lexer.lexer(input)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    check:
      len(program.statements) == 1
      test_integer_literal_expression(program.statements[0], 5)

  test "test prefix expression parsing":
    let tests = @[
      test_prefix("!5;", "!", 5),
      test_prefix("-15;", "-", 15)
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_prefix_expression(program.statements[0], test)

  test "test infix expression parsing":
    let tests = @[
      test_infix("5 + 5;",  "+",  5, 5),
      test_infix("5 - 5;",  "-",  5, 5),
      test_infix("5 * 5;",  "*",  5, 5),
      test_infix("5 / 5;",  "/",  5, 5),
      test_infix("5 < 5;",  "<",  5, 5),
      test_infix("5 > 5;",  ">",  5, 5),
      test_infix("5 == 5;", "==", 5, 5),
      test_infix("5 != 5;", "!=", 5, 5)
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_infix_expression(program.statements[0], test)
        #test_infix(program.statements[0].ExpressionStatement.expression, test.operator, test.left_value, test.right_value)

  test "test infix expression operator precedence parsing":
    let tests = @[
      test_precedence("-a * b;", "((-a) * b)"),
      test_precedence("!-a;", "(!(-a))"),
      test_precedence("a + b + c;", "(a + (b + c))"),
      test_precedence("a + b - c;", "(a + (b - c))"),
      test_precedence("a * b * c;", "((a * b) * c)"),
      test_precedence("a * b / c;", "((a * b) / c)"),
      test_precedence("a + b / c;", "(a + (b / c))",),
      test_precedence("a + b * c + d / e - f;", "(a + ((b * c) + ((d / e) - f)))"),
      test_precedence("3 + 4; -5 * 5;", "(3 + 4)\n((-5) * 5)"),
      test_precedence("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4))"),
      test_precedence("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4))"),
      test_precedence("3 + 4 * 5 == 3 * 1 + 4 * 5;", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        program.string() == test.expected & "\n"