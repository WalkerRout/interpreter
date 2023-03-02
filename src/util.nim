import std/times
import std/os
import std/strutils

template dynamic_cast*[T](root: untyped): untyped =
  try:
    T(root)
  except:
    nil

template benchmark*(bench_name: string, statements: untyped): untyped =
  block:
    let t0 = epochTime()

    let result = statements

    let t1 = epochTime()
    let dt = t1 - t0
    let elapsed = strutils.formatFloat(dt, ffDecimal, 5)
    echo "CPU Time [", bench_name, "] ", elapsed, "s"

    result