
template dynamic_cast*[T](root: untyped): untyped =
  try:
    T(root)
  except:
    nil