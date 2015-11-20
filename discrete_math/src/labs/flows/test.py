from subprocess import call
from random import randint

cnt = 1
while True:
    f = open('circulation.in', 'w')
    n = 200;
    m = 15000;
    print(n, m, file=f)
    edges = []
    for i in range(m):
        q, w, r = randint(1, n), randint(1, n), randint(1, 10 ** 5)
        l = randint(1, r)
        print(q, w, l, r, file=f)
        edges.append((l, r))
    f.close()
    call('./a.out')
    f = open('circulation.out', 'r')
    if f.readline() == "YES":
        for c in edges:
            cf = int(f.readline())
            if cf <= c[0] or cf >= c[1]:
                quit(1)
    print(cnt, "OK")
    cnt += 1
    f.close()
