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

  NodeType* {.pure.} = enum
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

  BooleanExpression* = ref object of Node
    token*: token.Token
    value*: bool

  PrefixExpression* = ref object of Node
    token*: token.Token
    operator*: string
    value*: Node # invariant: must be an expression

  InfixExpression* = ref object of Node
    token*: token.Token
    operator*: string
    left_value*: Node # invariant: must be an expression
    right_value*: Node # invariant: must be an expression

  IfExpression* = ref object of Node
    token*: token.Token
    condition*: Node # invariant: must be an expression
    consequence*: BlockStatement
    alternative*: BlockStatement

  FnExpression* = ref object of Node
    token*: token.Token
    parameters*: seq[Node] # invariant: nodes must be identifier expressions
    body*: BlockStatement

  FnCallExpression* = ref object of Node
    token*: token.Token
    function*: Node # invariant: must be an identifier expression
    arguments*: seq[Node] # invariant: nodes must be expressions

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

  BlockStatement* = ref object of Node
    token*: token.Token
    statements*: seq[Node] # invariant: nodes must be statements

  Error* = object
    msg: string
    src: string

  Program* = ref object of Node
    statements*: seq[Node] # invariant: nodes must be statements

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
    token.ASTERISK: Precedence.prDivMul,
    token.LPAREN: Precedence.prCall
  }.toTable

# forward declarations
proc parse_statement(p: var Parser): Node
proc parse_let_statement(p: var Parser): LetStatement
proc parse_return_statement(p: var Parser): ReturnStatement
proc parse_expression_statement(p: var Parser): ExpressionStatement
proc parse_block_statement(p: var Parser): BlockStatement
proc parse_expression(p: var Parser, pr: Precedence): Node
proc parse_identifier(p: var Parser): Node
proc parse_integer_literal(p: var Parser): Node
proc parse_boolean(p: var Parser): Node
proc parse_prefix(p: var Parser): Node
proc parse_infix(p: var Parser, left: Node): Node
proc parse_grouped(p: var Parser): Node
proc parse_if(p: var Parser): Node
proc parse_fn(p: var Parser): Node
proc parse_fn_parameters(p: var Parser): seq[Node]
proc parse_fn_call(p: var Parser, fn: Node): Node
proc parse_fn_call_arguments(p: var Parser): seq[Node]
proc register_prefix(p: var Parser, token_type: token.TokenType, prfn: PrefixParseFn)
proc register_infix(p: var Parser, token_type: token.TokenType, infn: InfixParseFn)
proc lexer_next_token(p: var Parser)
proc curr_precedence(p: Parser): Precedence
proc next_precedence(p: Parser): Precedence
proc expect_next(p: var Parser, token_type: token.TokenType): bool
proc peek_error(p: var Parser, token_type: token.TokenType)
proc no_prefix_parse_fn_error(p: var Parser, token_type: token.TokenType)
proc curr_token_is(p: Parser, token_type: token.TokenType): bool
proc next_token_is(p: Parser, token_type: token.TokenType): bool
proc check_parser_errors*(p: Parser)

# procedures
# node procs
proc `$`(n: Node): string =
  if n != nil:
    case n.node_type
    of ntExpression:
      n.expression_name
    of ntStatement:
      n.statement_name
  else:
    "nil"

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

# boolean expression procs
proc boolean_expression*(t: token.Token, v: bool): BooleanExpression =
  BooleanExpression(node_type: ntExpression, expression_name: "BooleanExpression",
                    token: t, value: v)

method token_literal*(be: BooleanExpression): string = 
  be.token.literal

method string*(be: BooleanExpression): string = 
  be.token.literal

# prefix expression procs
proc prefix_expression*(t: token.Token, o: string, v: Node): PrefixExpression =
  assert v.node_type == ntExpression
  PrefixExpression(node_type: ntExpression, expression_name: "PrefixExpression",
                   token: t, operator: o, value: v)

method token_literal*(pe: PrefixExpression): string = 
  pe.token.literal

method string*(pe: PrefixExpression): string =
  "(" & pe.operator & pe.value.string() & ")"

# infix expression procs
proc infix_expression*(t: token.Token, o: string, lv, rv: Node): InfixExpression =
  assert lv.node_type == ntExpression and rv.node_type == ntExpression
  InfixExpression(node_type: ntExpression, expression_name: "InfixExpression",
                  token: t, operator: o, left_value: lv, right_value: rv)

method token_literal*(ie: InfixExpression): string = 
  ie.token.literal

method string*(ie: InfixExpression): string =
  "(" & ie.left_value.string() & " " & ie.operator & " " & ie.right_value.string() & ")"

# if expression procs
proc if_expression*(t: token.Token, c: Node, csq, alt: BlockStatement): IfExpression =
  assert c.node_type == ntExpression
  IfExpression(node_type: ntExpression, expression_name: "IfExpression",
               token: t, condition: c, consequence: csq, alternative: alt)

method token_literal*(ie: IfExpression): string = 
  ie.token.literal

method string*(ie: IfExpression): string =
  result = "if " & ie.condition.string() & ":\n" & ie.consequence.string()
  if ie.alternative != nil: 
    result &= "else:\n" & ie.alternative.string()

# fn expression procs
proc fn_expression*(t: token.Token, p: seq[Node], b: BlockStatement): FnExpression =
  for param in p:
    assert param.node_type == ntExpression  

  FnExpression(node_type: ntExpression, expression_name: "FnExpression",
               token: t, parameters: p, body: b)

method token_literal*(fe: FnExpression): string = 
  fe.token.literal

method string*(fe: FnExpression): string =
  var params: string
  for param in fe.parameters:
    params.add(param.string())

  fe.token_literal() & "(" & params.join(", ") & "):\n" & fe.body.string()

# fn call expression procs
proc fn_call_expression*(t: token.Token, fn: Node, a: seq[Node]): FnCallExpression =
  assert fn.node_type == ntExpression #and fn.expression_name == "IdentifierExpression"
  for arg in a:
    assert arg.node_type == ntExpression  

  FnCallExpression(node_type: ntExpression, expression_name: "FnCallExpression",
                   token: t, function: fn, arguments: a)

method token_literal*(fce: FnCallExpression): string = 
  fce.token.literal

method string*(fce: FnCallExpression): string =
  var args: string
  for arg in fce.arguments:
    args.add(arg.string())
    args.add(", ")
  args.delete(len(args)-2..<len(args))

  fce.function.string() & "(" & args & ")"

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

# expression statement procs
proc expression_statement*(t: token.Token, e: Node): ExpressionStatement =
  assert e.node_type == ntExpression # invariant
  ExpressionStatement(node_type: ntStatement, statement_name: "ExpressionStatement",
                      token: t, expression: e)

method token_literal*(es: ExpressionStatement): string =
  es.token.literal

method string*(es: ExpressionStatement): string =
  result = "<nil>"
  if es.expression != nil:
    result = es.expression.string()

# block statement procs
proc block_statement*(t: token.Token, s: seq[Node]): BlockStatement =
  BlockStatement(node_type: ntStatement, statement_name: "BlockStatement",
                 token: t, statements: s)

method token_literal*(bs: BlockStatement): string =
  bs.token.literal

method string*(bs: BlockStatement): string =
  for statement in bs.statements:
    result &= statement.string() & "\n"

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

method token_literal*(p: Program): string =
  if len(p.statements) > 0:
    result = p.statements[0].token_literal()

method string*(p: Program): string =
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
  result.register_prefix(token.TRUE, parse_boolean)
  result.register_prefix(token.FALSE, parse_boolean)
  result.register_prefix(token.BANG, parse_prefix)
  result.register_prefix(token.MINUS, parse_prefix)
  result.register_prefix(token.LPAREN, parse_grouped)
  result.register_prefix(token.IF, parse_if)
  result.register_prefix(token.FUNCTION, parse_fn)

  result.register_infix(token.PLUS, parse_infix)
  result.register_infix(token.MINUS, parse_infix)
  result.register_infix(token.SLASH, parse_infix)
  result.register_infix(token.ASTERISK, parse_infix)
  result.register_infix(token.LT, parse_infix)
  result.register_infix(token.GT, parse_infix)
  result.register_infix(token.EQ, parse_infix)
  result.register_infix(token.NEQ, parse_infix)
  result.register_infix(token.LPAREN, parse_fn_call)

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

  if not p.expect_next(token.IDENT):
    return nil

  result.name = identifier_expression(p.curr_token, p.curr_token.literal)

  if not p.expect_next(token.ASSIGN):
    return nil

  p.lexer_next_token()
  result.value = p.parse_expression(Precedence.prLowest)

  while not p.curr_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_return_statement(p: var Parser): ReturnStatement =
  new result
  result.token = p.curr_token

  p.lexer_next_token()

  result.value = p.parse_expression(Precedence.prLowest)

  while not p.curr_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_expression_statement(p: var Parser): ExpressionStatement =
  new result
  result.token = p.curr_token
  result.expression = p.parse_expression(Precedence.prLowest)

  if p.next_token_is(token.SEMICOLON):
    p.lexer_next_token()

proc parse_block_statement(p: var Parser): BlockStatement =
  new result
  result.token = p.curr_token
  result.statements = @[]

  p.lexer_next_token()

  while not p.curr_token_is(token.RBRACE) and not p.curr_token_is(token.EOF):
    let s = p.parse_statement()
    if s != nil:
      result.statements.add(s)
    p.lexer_next_token() # advance up to rbrace

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
  var ile = IntegerLiteralExpression()
  ile.token = p.curr_token
  try:
    ile.value = p.curr_token.literal.parseInt
  except: 
    p.errors.add(error("could not parse " & p.curr_token.literal & " as integer!"))
    return nil
  result = ile

proc parse_boolean(p: var Parser): Node =
  var be = BooleanExpression()
  be.token = p.curr_token
  be.value = p.curr_token_is(token.TRUE)
  result = be

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

proc parse_grouped(p: var Parser): Node =
  p.lexer_next_token()
  result = p.parse_expression(Precedence.prLowest)

  if not p.expect_next(token.RPAREN):
    result = nil

proc parse_if(p: var Parser): Node =
  var ie = IfExpression()
  ie.token = p.curr_token

  if not p.expect_next(token.LPAREN):
    return nil

  p.lexer_next_token()
  ie.condition = p.parse_expression(Precedence.prLowest)

  if not p.expect_next(token.RPAREN):
    return nil
  if not p.expect_next(token.LBRACE):
    return nil

  ie.consequence = p.parse_block_statement() # advance up to `}`

  if p.next_token_is(token.ELSE):
    p.lexer_next_token() # skip `}`
    if not p.expect_next(token.LBRACE):
      return nil

    ie.alternative = p.parse_block_statement() # advance up to `}`
  result = ie

proc parse_fn(p: var Parser): Node =
  var fe = FnExpression()
  fe.token = p.curr_token

  if not p.expect_next(token.LPAREN):
    return nil

  fe.parameters = p.parse_fn_parameters()

  if not p.expect_next(token.LBRACE):
    return nil

  fe.body = p.parse_block_statement()
  result = fe

proc parse_fn_parameters(p: var Parser): seq[Node] =
  result = @[]

  if p.next_token_is(token.RPAREN):
    p.lexer_next_token()
    return

  p.lexer_next_token()
  let ident = identifier_expression(p.curr_token, p.curr_token.literal)
  result.add(ident)

  while p.next_token_is(token.COMMA):
    p.lexer_next_token()
    p.lexer_next_token()
    let ident = identifier_expression(p.curr_token, p.curr_token.literal)
    result.add(ident)

  if not p.expect_next(token.RPAREN):
    return @[]

proc parse_fn_call(p: var Parser, fn: Node): Node =
  var fce = FnCallExpression()
  fce.token = p.curr_token
  fce.function = fn
  fce.arguments = p.parse_fn_call_arguments()
  result = fce

proc parse_fn_call_arguments(p: var Parser): seq[Node] =
  result = @[]

  if p.next_token_is(token.RPAREN):
    p.lexer_next_token()
    return

  p.lexer_next_token()
  result.add(p.parse_expression(Precedence.prLowest))

  while p.next_token_is(token.COMMA):
    p.lexer_next_token()
    p.lexer_next_token()
    result.add(p.parse_expression(Precedence.prLowest))

  if not p.expect_next(token.RPAREN):
    return @[]

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

proc expect_next(p: var Parser, token_type: token.TokenType): bool =
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

      TestLet[T] = object
        input: string
        expected_identifier: string
        expected_value: T

      TestReturn[T] = object
        input: string
        expected_value: T

      TestBoolean = object
        input: string
        expected_state: bool

      TestPrefix[T] = object
        input: string
        operator: string
        value: T

      TestInfix[T] = object
        input: string
        operator: string
        left_value: T
        right_value: T

      TestPrecedence = object
        input: string
        expected: string

      TestFn = object
        input: string
        expected_params: seq[string]

    proc test_identn(ei: string): TestIdent =
      TestIdent(expected_identifier: ei)

    proc test_letn[T](i: string, ei: string, ev: T): TestLet[T] =
      TestLet[T](input: i, expected_identifier: ei, expected_value: ev)

    proc test_returnn[T](i: string, ev: T): TestReturn[T] =
      TestReturn[T](input: i, expected_value: ev)

    proc test_booleann(i: string, es: bool): TestBoolean =
      TestBoolean(input: i, expected_state: es)

    proc test_prefixn[T](i, o: string, v: T): TestPrefix[T] =
      TestPrefix[T](input: i, operator: o, value: v)

    proc test_infixn[T](i, o: string, lv, rv: T): TestInfix[T] =
      TestInfix[T](input: i, operator: o, left_value: lv, right_value: rv)

    proc test_precedencen(i, e: string): TestPrecedence =
      TestPrecedence(input: i, expected: e)

    proc test_fnn(i: string, ep: seq[string]): TestFn =
      TestFn(input: i, expected_params: ep)

    # tests for literals, not expression statements
    proc test_integer_literal(e: Node, value: int64): bool =
      let ile = dynamic_cast[IntegerLiteralExpression](e)
      if ile == nil: return false
      result = ile.value == value
      result = result and ile.token_literal() == $value

    proc test_identifier(e: Node, value: string): bool =
      let ie = dynamic_cast[IdentifierExpression](e)
      if ie == nil: return false
      result = ie.value == value
      result = result and ie.token_literal() == value

    proc test_boolean(e: Node, value: bool): bool =
      let be = dynamic_cast[BooleanExpression](e)
      if be == nil: return false
      result = be.value == value
      result = result and be.token_literal() == $value

    proc test_literal(e: Node, value: auto): bool =
      const type_name = value.type.name
      when type_name == "int64" or type_name == "int":
        test_integer_literal(e, value)
      elif type_name == "string":
        test_identifier(e, value)
      elif type_name == "bool":
        test_boolean(e, value)
      else:
        echo "No literal test for type " & type_name
        false
    # end of tests for literals

    proc test_prefix(e: Node, operator: string, left: auto): bool =
      let pe = dynamic_cast[PrefixExpression](e)
      if pe == nil: return false
      result = pe.operator == operator
      result = result and test_literal(pe.value, left)

    proc test_infix(e: Node, operator: string, left, right: auto): bool =
      let ie = dynamic_cast[InfixExpression](e)
      if ie == nil: return false
      result = test_literal(ie.left_value, left)
      result = result and ie.operator == operator
      result = result and test_literal(ie.right_value, right)
    # end tests for literals

    proc test_let_statement(s: Node, test: TestLet): bool =
      let ls = dynamic_cast[LetStatement](s)
      if ls == nil: return false
      result = test_literal(ls.name, test.expected_identifier)
      result = result and test_literal(ls.value, test.expected_value)

    proc test_return_statement(s: Node, test: TestReturn): bool =
      let rs = dynamic_cast[ReturnStatement](s)
      if rs == nil: return false
      result = rs.token_literal == "return"

  test "test let statement parsing":
    let tests_int = @[
      test_letn("let x = 5;", "x", 5),
      test_letn("let y = 10;", "y", 10),
      test_letn("let foobar = 20040605;", "foobar", 20040605),
    ]
    let tests_string = @[
      test_letn("let barfoo = x;", "barfoo", "x")
    ]
    for i, test in tests_int.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_let_statement(program.statements[0], test)

    for i, test in tests_string.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_let_statement(program.statements[0], test)


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
    let tests_int = @[
      test_returnn("return 5;", 5),
      test_returnn("return 10;", 10)
    ]
    let tests_string = @[
      test_returnn("return x;", "x")
    ]
    for i, test in tests_int.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_return_statement(program.statements[0], test)

    for i, test in tests_string.pairs:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_return_statement(program.statements[0], test)

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
      test_identifier(program.statements[0].ExpressionStatement.expression, "foobar")

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
      test_integer_literal(program.statements[0].ExpressionStatement.expression, 5)

  test "test prefix expression parsing":
    let tests_int = @[
      test_prefixn("!5;", "!", 5),
      test_prefixn("-15;", "-", 15)
    ]
    let tests_bool = @[
      test_prefixn("!true;", "!", true),
      test_prefixn("!false;", "!", false)
    ]
    for test in tests_int:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_prefix(program.statements[0].ExpressionStatement.expression, test.operator, test.value.int64)

    for test in tests_bool:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_prefix(program.statements[0].ExpressionStatement.expression, test.operator, test.value.bool)

  test "test infix expression parsing":
    let tests_int = @[
      test_infixn("5 + 5;",  "+",  5, 5),
      test_infixn("5 - 5;",  "-",  5, 5),
      test_infixn("5 * 5;",  "*",  5, 5),
      test_infixn("5 / 5;",  "/",  5, 5),
      test_infixn("5 < 5;",  "<",  5, 5),
      test_infixn("5 > 5;",  ">",  5, 5),
      test_infixn("5 == 5;", "==", 5, 5),
      test_infixn("5 != 5;", "!=", 5, 5),
    ]
    let tests_bool = @[
      test_infixn("true == true;", "==", true, true),
      test_infixn("true != false;", "!=", true, false),
      test_infixn("false == false;", "==", false, false),
    ]
    for test in tests_int:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_infix(program.statements[0].ExpressionStatement.expression, test.operator, test.left_value.int64, test.right_value.int64)

    for test in tests_bool:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        len(program.statements) == 1
        test_infix(program.statements[0].ExpressionStatement.expression, test.operator, test.left_value.bool, test.right_value.bool)

  test "test infix expression operator precedence parsing":
    let tests = @[
      test_precedencen("-a * b;", "((-a) * b)"),
      test_precedencen("!-a;", "(!(-a))"),
      test_precedencen("a + b + c;", "(a + (b + c))"),
      test_precedencen("a + b - c;", "(a + (b - c))"),
      test_precedencen("a * b * c;", "((a * b) * c)"),
      test_precedencen("a * b / c;", "((a * b) / c)"),
      test_precedencen("a + b / c;", "(a + (b / c))",),
      test_precedencen("a + b * c + d / e - f;", "(a + ((b * c) + ((d / e) - f)))"),
      test_precedencen("3 + 4; -5 * 5;", "(3 + 4)\n((-5) * 5)"),
      test_precedencen("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4))"),
      test_precedencen("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4))"),
      test_precedencen("3 + 4 * 5 == 3 * 1 + 4 * 5;", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
      test_precedencen("true;", "true"),
      test_precedencen("false;", "false"),
      test_precedencen("3 > 5 == false", "((3 > 5) == false)"),
      test_precedencen("3 < 5 == true", "((3 < 5) == true)")
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        program.string() == test.expected & "\n"

  test "test boolean expression parsing":
    let tests = @[
      test_booleann("true;", true),
      test_booleann("false;", false)
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        test_literal(program.statements[0].ExpressionStatement.expression, test.expected_state)

  test "test forced precedence expression parsing":
    let tests = @[
      test_precedencen("1 + (2 + 3) + 4", "(1 + ((2 + 3) + 4))"),
      test_precedencen("(5 + 5) * 2", "((5 + 5) * 2)"),
      test_precedencen("2 / (5 + 5)", "(2 / (5 + 5))"),
      test_precedencen("-(5 + 5)", "(-(5 + 5))"),
      test_precedencen("!(true == true)", "(!(true == true))"),
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        program.string() == test.expected & "\n"

  test "test if expression parsing":
    let test = "if (x < y) { x };"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let ie = dynamic_cast[IfExpression](program.statements[0].ExpressionStatement.expression)
    check:
      ie != nil
      len(program.statements) == 1
      test_infix(ie.condition, "<", "x", "y")
      len(ie.consequence.statements) == 1
      test_literal(ie.consequence.statements[0].ExpressionStatement.expression, "x")
      ie.alternative == nil

  test "test if else expression parsing":
    let test = "if (x < y) { x } else { y };"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let ie = dynamic_cast[IfExpression](program.statements[0].ExpressionStatement.expression)
    check:
      ie != nil
      len(program.statements) == 1
      test_infix(ie.condition, "<", "x", "y")
      len(ie.consequence.statements) == 1
      test_literal(ie.consequence.statements[0].ExpressionStatement.expression, "x")
      test_literal(ie.alternative.statements[0].ExpressionStatement.expression, "y")

  test "test fn expression parsing":
    let test = "fn (x, y) { x + y; };"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let fe = dynamic_cast[FnExpression](program.statements[0].ExpressionStatement.expression)
    check:
      fe != nil
      len(program.statements) == 1
      test_literal(fe.parameters[0], "x")
      test_literal(fe.parameters[1], "y")
      len(fe.body.statements) == 1
      test_infix(fe.body.statements[0].ExpressionStatement.expression, "+", "x", "y")

  test "test fn expression edge case parsing":
    let tests = @[
      test_fnn("fn () {};", @[]),
      test_fnn("fn (x) {};", @["x"]),
      test_fnn("fn (x, y, z) {};", @["x", "y", "z"])
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      let fe = dynamic_cast[FnExpression](program.statements[0].ExpressionStatement.expression)
      check:
        fe != nil
        len(program.statements) == 1
        len(fe.parameters) == len(test.expected_params)

      for i, ep in test.expected_params.pairs:
        check test_literal(fe.parameters[i], ep)

  test "test fn call expression parsing":
    let test = "add(1, 2 * 3, 4 + 5);"
    let lexer = lexer.lexer(test)
    var parser = parser(lexer)
    let program = parser.parse_program()
    parser.check_parser_errors()
    let fce = dynamic_cast[FnCallExpression](program.statements[0].ExpressionStatement.expression)
    check:
      fce != nil
      len(program.statements) == 1
      test_literal(fce.function, "add")
      len(fce.arguments) == 3
      test_literal(fce.arguments[0], 1)
      test_infix(fce.arguments[1], "*", 2, 3)
      test_infix(fce.arguments[2], "+", 4, 5)


  test "test fn call expression precedence parsing":
    let tests = @[
      test_precedencen("a + add(b * c) + d;", "(a + (add((b * c)) + d))"),
      test_precedencen("add(a, b + c, 1 * (2 + 3) / 5, add(5, 1 + 2));", "add(a, (b + c), ((1 * (2 + 3)) / 5), add(5, (1 + 2)))"),
      test_precedencen("add(a + b + c * d / f + g);", "add((a + (b + (((c * d) / f) + g))))")
    ]
    for test in tests:
      let lexer = lexer.lexer(test.input)
      var parser = parser(lexer)
      let program = parser.parse_program()
      parser.check_parser_errors()
      check:
        program.string() == test.expected & "\n"