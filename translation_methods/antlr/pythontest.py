def foo(_0, _1):
  a = _0
  if a <= 6:
    def baz():
      return 6
    return baz()
  a = _0
  f = _1
  if f([a]):
    def bar():
      return 123
    return a + bar() + 5
  return 1010
def constTrue(_0):
  return True
def main():
  return foo(888, constTrue)
