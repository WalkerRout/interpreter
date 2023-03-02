import std/unittest

import ../src/token

suite "test token":
  setup:
    let tok = token(IDENT, "IDENT")

  test "test create token":
    check:
      tok.token_type == IDENT
      tok.literal == "IDENT"

  test "test lookup ident":
    check:
      lookup_ident("fn") == FUNCTION
      lookup_ident("let") == LET
      lookup_ident("this_is_a_custom_identifier") == IDENT