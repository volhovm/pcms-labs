def divides(_0, _1):
  n = _0
  m = _1
  return ((n / m) * m) == n
def check(_0, _1):
  n = _0
  m = _1
  if m == 1:
    return True
  n = _0
  m = _1
  if divides(n, m):
    return False
  n = _0
  m = _1
  def k():
    return (m - 1)
  return check(n, k())
def checkSimple(_0):
  n = _0
  def m():
    return n / 2
  return check(n, m())

print checkSimple(5)
print checkSimple(6)
print checkSimple(127)
print checkSimple(128)
print checkSimple(1023)
