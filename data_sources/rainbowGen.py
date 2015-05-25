from random import seed, randint

seed()
out = open("rainbow.in", "w")

out.write("100 5000\n")
for i in range(0, 5000):
    a = randint(1, 99)
    b = randint(a + 1, 100)
    c = randint(1, 100)
    out.write("%d %d %d\n" % (a, b, c))
