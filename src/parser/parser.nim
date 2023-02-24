import std/unittest
import std/tables
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

# forward declarations
proc parse_statement(p: var Parser): Node
proc parse_let_statement(p: var Parser): LetStatement
proc parse_return_statement(p: var Parser): ReturnStatement
proc parse_expression_statement(p: var Parser): ExpressionStatement
proc parse_expression(p: var Parser, pr: Precedence): Node
proc parse_identifier(p: var Parser): Node
proc register_prefix(p: var Parser, token_type: token.TokenType, prfn: PrefixParseFn)
proc register_infix(p: var Parser, token_type: token.TokenType, infn: InfixParseFn)
proc lexer_next_token(p: var Parser)
proc expect_peek(p: var Parser, token_type: token.TokenType): bool
proc peek_error(p: var Parser, token_type: token.TokenType)
proc curr_token_is(p: Parser, token_type: token.TokenType): bool
proc peek_token_is(p: Parser, token_type: token.TokenType): bool
proc check_parser_errors(p: Parser)

# procedures
# node procs
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

  # todo

  while not p.curr_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_return_statement(p: var Parser): ReturnStatement =
  new result
  result.token = p.curr_token

  p.lexer_next_token()

  # todo

  while not p.curr_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_expression_statement(p: var Parser): ExpressionStatement =
  new result
  result.token = p.curr_token
  result.expression = p.parse_expression(Precedence.prLowest)

  if p.peek_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_expression(p: var Parser, pr: Precedence): Node =
  var prefix_fn = p.prefix_parse_fns.getOrDefault(p.curr_token.token_type, nil)
  if prefix_fn == nil:
    return nil

  result = prefix_fn(p)

# jump table functions -> generic, return Node type
proc parse_identifier(p: var Parser): Node =
  identifier_expression(p.curr_token, p.curr_token.literal)

proc register_prefix(p: var Parser, token_type: token.TokenType, prfn: PrefixParseFn) =
  p.prefix_parse_fns[token_type] = prfn

proc register_infix(p: var Parser, token_type: token.TokenType, infn: InfixParseFn) =
  p.infix_parse_fns[token_type] = infn

proc lexer_next_token(p: var Parser) = 
  p.curr_token = p.next_token
  p.next_token = p.lexer.next_token()

proc expect_peek(p: var Parser, token_type: token.TokenType): bool =
  if p.peek_token_is(token_type):
    p.lexer_next_token()
    result = true
  else:
    p.peek_error(token_type)
    result = false

proc peek_error(p: var Parser, token_type: token.TokenType) =
  p.errors.add(error("expected next token to be " & token_type & " - instead got " & p.next_token.token_type))

proc curr_token_is(p: Parser, token_type: token.TokenType): bool = 
  result = p.curr_token.token_type == token_type

proc peek_token_is(p: Parser, token_type: token.TokenType): bool = 
  result = p.next_token.token_type == token_type

proc check_parser_errors(p: Parser) =
  if len(p.errors) == 0: return
  echo "Parser had ", len p.errors, " errors:"
  for error in p.errors:
    error.log_msg()

suite "test parser":
  setup:
    type TestIdent = object
      expected_identifier: string

    proc test_ident(ei: string): TestIdent = 
      TestIdent(expected_identifier: ei)

    proc test_let_statement(s: Node, name: string): bool = 
      let ls = dynamic_cast[LetStatement](s)
      result = ls != nil
      result = result and ls.name.value == name
      result = result and ls.name.token_literal() == name

    proc test_return_statement(s: Node): bool =
      let rs = dynamic_cast[ReturnStatement](s)
      result = rs != nil
      result = result and rs.token_literal == "return"

    proc test_identifier_expression(e: Node): bool =
      let es = dynamic_cast[ExpressionStatement](e)
      result = es != nil
      let ie = dynamic_cast[IdentifierExpression](es.expression)
      result = result and ie != nil
      result = result and ie.value == "foobar"
      result = result and ie.token_literal() == "foobar"

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

  test "test expression statement parsing":
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
      test_identifier_expression(program.statements[0])
