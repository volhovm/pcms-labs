# written by Dima Mukhutdinov
import itertools

alphabet_1 = "_01ao()"
alphabet_2 = "_01"
alphabet_3 = "_ao("

def unique(seq):
    seen = set()
    seen_add = seen.add
    return [x for x in seq if not (x in seen or seen_add(x))]

def open_wildcard(line, w, alphabet):
    return list(unique(map(lambda c: line.replace(w, c), alphabet)))

def open_wildcards(lines, w, alphabet):
    expanded = map(lambda l: open_wildcard(l, w, alphabet), lines)
    return list(itertools.chain(*expanded))

def lines(s):
    return s.split('\n')

def unlines(ss):
    return '\n'.join(ss)

def expand_all(txt):
    e1 = open_wildcards(lines(txt), "*", alphabet_1)
    e2 = open_wildcards(e1, "$", alphabet_2)
    e3 = open_wildcards(e2, "%", alphabet_3)
    return unlines(e3)

f = open("infix.out.pre", "r")
print expand_all(f.read())
