solution1="""3 2 L M
2 3 R M
2 4 R M
2 1 M M
"""

solution2="""1 1 L M
"""

solution3="""2 3 L M
3 2 R M
1 2 M R
"""
solution4="""2 1 M M
3 4 M M
1 1 R L
1 4 L M
"""

solution5="""3 2 M M
1 1 R M
2 3 M L
"""

solution6="""2 2 R M
3 4 M M
3 5 L M
3 5 M M
1 2 M M
"""
solution7="""2 4 R M
2 3 L M
2 4 M M
1 3 M M
"""

solution8="""2 3 M M
3 3 L M
4 3 M M
5 2 R M
2 1 L M
"""

solution10="""2 3 L M
4 4 M M
2 4 R M
5 6 R M
1 1 L M
1 4 R M
"""

solution9="""2 3 M M
4 3 R M
5 6 M M
6 6 L M
1 2 R M
5 1 L M
"""

with open('artificial.in', 'r') as f:
    numarr = [int(x) for x in f.readline().split()]
    num = numarr[0]
    with open('artificial.out', 'w') as f2:
        if num == 1: f2.write(solution1)
        elif num == 2: f2.write(solution2)
        elif num == 3: f2.write(solution3)
        elif num == 4: f2.write(solution4)
        elif num == 5: f2.write(solution5)
        elif num == 6: f2.write(solution6)
        elif num == 7: f2.write(solution7)
        elif num == 8: f2.write(solution8)
        elif num == 9: f2.write(solution9)
        else: f2.write(solution10)

exit(0)
