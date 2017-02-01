def divides(_0, _1):
  def n(): return _0
  def m(): return _1
  return n() % m() == 0

def check(_0, _1):
  def n(): return _0
  def m(): return _1
  if m() == 1:
    return True
  def n(): return _0
  def m(): return _1
  if divides(n(), m()):
    return False
  def n(): return _0
  def m(): return _1
  return check(n(), (m() - 1))

def isPrime(_0):
  def n(): return _0
  return check(n(), (n() // 2))

def gcd(_0, _1):
  def n(): return _0
  def m(): return _1
  if m() == 0:
    return n()
  def n(): return _0
  def m(): return _1
  return gcd(m(), (n() % m()))

print(isPrime(5))
print(isPrime(7))
print(isPrime(8))
print(isPrime(9))
print(isPrime(13))

print (gcd(20,8))
