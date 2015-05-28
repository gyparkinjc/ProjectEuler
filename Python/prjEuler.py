import sys
import time

answer = {}
with open("../data/euler_answer.txt", "r") as f:
    for line in f.readlines():
        a = line.split('. ')
        num = int(a[0])
        value = a[1].rstrip()
        answer[num] = value

answer_func = {}
def register_solution(proj, f):
    answer_func[proj] = f
    
def solution(proj, f):
    start_time = time.clock()
    value = f()
    elapsed_time = time.clock() - start_time
    print("[Proj {0:>3}] : {1}, {2:>15}, Time : {3}".format(str(proj),
                    str(value) == answer[proj], str(value), elapsed_time))
    sys.stdout.flush()

def solution_all():
    a_list = []
    for k in answer_func: a_list.append((k, answer_func[k]))
    a_list.sort()
    start_time = time.clock()
    for pair in a_list:
        proj = pair[0]
        solution(proj, answer_func[proj])
    time_elapsed = time.clock() - start_time    
    print("[total number] : %d, [total time] : %f" % (len(a_list), time_elapsed))

#--------------------------------------------
# Utility functions
#--------------------------------------------
def fib_seq(): # fibonacci number generator
    a , b = 1, 1
    while True:
        yield a
        a, b = a + b, a
def prime_seq():
    from itertools import count
    
    yield 2
    for value in count(3, 2):
        if is_prime(value): yield value
def triangle_seq():
    from itertools import count
    
    value = 1
    for i in count(2, 1):
        yield value
        value += i
def is_prime(value):
    from math import sqrt
    
    if value == 2: return True
    if value < 2 or value % 2 == 0: return False
    flag = True
    for i in range(3, int(sqrt(value)) + 1, 2):
        if value % i == 0:
            flag = False
            break
    return flag
def is_palindrome(x):
    s1 = int(str(x)[::-1])
    return s1 == x
def prime_factors(value):
    factors = []
    for i in prime_seq():
        while value % i == 0:
            value = value // i
            factors.append(i)
        if value == 1:
            return factors
def divisor_list(value):
    from itertools import combinations
    from functools import reduce

    a_list = prime_factors(value)
    divisors = set(a_list)
    divisors.add(1)
    for i in range(2, len(a_list)+1):
        for pair in combinations(a_list, i):
            divisors.add(reduce(lambda x, y: x * y, pair))
    return divisors
            
#--------------------------------------------
# project euler 1 : 233168
# Find the sum of all the multiples of 3 or 5 below 1000.
#--------------------------------------------
def solution_1():
    return sum([ x for x in range(1, 1000) if (x % 3) == 0 or (x % 5) == 0 ])
register_solution(1, solution_1)

#--------------------------------------------
# project euler 2 : 4613732
# Find the sum of the even-valued fibonacci numbers under 4000000.
#--------------------------------------------
def solution_2():
    from itertools import takewhile
    
    even_fib = (i for i in fib_seq() if i % 2 == 0)
    return sum(takewhile(lambda x: x <= 4000000, even_fib))
answer_func[2] = solution_2

#--------------------------------------------
# project euler 3 : 6857
# What is the largest prime factor of the number 600851475143?
#--------------------------------------------
def solution_3():
    value = 600851475143
    for i in prime_seq():
        while value % i == 0:
            value = value // i
        if value == 1:
            return i
answer_func[3] = solution_3

#--------------------------------------------
# project euler 4 : [993, 913, 906609]
# Find the largest palindrome made from the product of two 3-digit numbers.
#--------------------------------------------
def solution_4():
    product_list = ((i, j, i*j) for i in range(100, 1000) for j in range(i, 1000))
    max_value = 0
    for i, j, p in product_list:
        if is_palindrome(p) and max_value < max(p, max_value):
            max_value = p
    return max_value
answer_func[4] = solution_4

#--------------------------------------------
# project euler 5 : 232792560
# What is the smallest positive number that is evenly divisible 
# by all of the numbers from 1 to 20?
#--------------------------------------------
def scm(x, y):
    a, b, result = x, y, 1
    for p in prime_seq():
        if p > a or p > b:
            result = a * b * result
            break
        while a % p == 0 and b % p == 0:
            result *= p
            a = a // p
            b = b // p
    return result

def solution_5():
    from functools import reduce
    return reduce(scm, range(2, 21))        
answer_func[5] = solution_5
#--------------------------------------------
# project euler 6 : 25164150
# Find the difference between the sum of the squares of 
# the first one hundred natural numbers and the square of the sum.
#--------------------------------------------
def solution_6():
    square = lambda x: x*x
    return square(sum(range(1,101))) - sum(map(square, range(1,101)))
answer_func[6] = solution_6

#--------------------------------------------
# project euler 7 : 104743 
# What is the 10001st prime number?
#--------------------------------------------
def solution_7():
    from itertools import islice    
    return next(islice(prime_seq(), 10000, 10001))
answer_func[7] = solution_7

#--------------------------------------------
# project euler 8 : 40824
# Find the greatest product of five consecutive digits 
# in the 1000-digit number.
#--------------------------------------------
def solution_8():
    digits = """
        73167176531330624919225119674426574742355349194934
        96983520312774506326239578318016984801869478851843 
        85861560789112949495459501737958331952853208805511 
        12540698747158523863050715693290963295227443043557 
        66896648950445244523161731856403098711121722383113 
        62229893423380308135336276614282806444486645238749 
        30358907296290491560440772390713810515859307960866 
        70172427121883998797908792274921901699720888093776 
        65727333001053367881220235421809751254540594752243 
        52584907711670556013604839586446706324415722155397 
        53697817977846174064955149290862569321978468622482 
        83972241375657056057490261407972968652414535100474 
        82166370484403199890008895243450658541227588666881 
        16427171479924442928230863465674813919123162824586 
        17866458359124566529476545682848912883142607690042 
        24219022671055626321111109370544217506941658960408 
        07198403850962455444362981230987879927244284909188 
        84580156166097919133875499200524063689912560717606 
        05886116467109405077541002256983155200055935729725 
        71636269561882670428252483600823257530420752963450
    """        
    digits = ''.join(digits.split())
    digits = list(digits)
    
    elements = []
    for i, e in enumerate(digits):
        product = 1
        for j in range(i, i+5):
            try:
                product *= int(digits[j])
            except IndexError:
                pass
        elements.append(product)
        
    return max(elements)
answer_func[8] = solution_8

#--------------------------------------------
# project euler 9 : 31875000
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.
#--------------------------------------------
def solution_9():
    from functools import reduce
    for a in range(2, 1000):
        for c in range(a, 1000):
            t1 = 1000 - a
            t2 = a * a
            if 2 * c == t1 + (t2 / t1):
                pairs = (a, 1000-(a+c), c)
                return reduce(lambda x, y: x * y, pairs)
answer_func[9] = solution_9        

#--------------------------------------------
# project euler 10 : 142913828922
# Find the sum of all the primes below two million.
#--------------------------------------------
def solution_10():
    from itertools import takewhile
    
    return sum(takewhile(lambda x: x <= 2000000, prime_seq()))
answer_func[10] = solution_10

#--------------------------------------------
# project euler 11 : 70600674
# What is the greatest product of four adjacent numbers 
# in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
#--------------------------------------------
def solution_11():
    grid = []
    with open("../data/20x20.txt", "r") as f:
        for i in range(3): grid.append([1 for j in range(26)])
        for line in f.readlines():
            row = [1, 1, 1]
            row += [int(i) for i in line.strip().split(' ')]
            row += [1, 1, 1]
            grid.append(row)
        for i in range(3): grid.append([1 for j in range(26)])
        
    a_list = []
    for i in range(3, 23):
        for j in range(3, 23):
            row_p, col_p, rdia_p, ldia_p = 1, 1, 1, 1
            for k in range(0, 4):
                row_p *= grid[i][j+k]
                col_p *= grid[i+k][j]
                rdia_p *= grid[i+k][j+k]
                ldia_p *= grid[i+k][j-k]
            a_list.append(max([row_p, col_p, rdia_p, ldia_p]))            
    return max(a_list)
answer_func[11] = solution_11

#--------------------------------------------
# project euler 12 : 76576500
# What is the value of the first triangle number 
# to have over five hundred divisors?
#--------------------------------------------
def solution_12():
    for value in triangle_seq():
        if len(divisor_list(value)) > 500: return value
    
answer_func[12] = solution_12

#--------------------------------------------
# project euler 13 : 5537376230
# Work out the first ten digits of the sum of the following 
# one-hundred 50-digit numbers.
#--------------------------------------------
def solution_13():
    num_list = [ int(x) for x in open("../data/numbers.txt").readlines() ]
    return int(str(sum(num_list))[0:10])
answer_func[13] = solution_13

#--------------------------------------------
# project euler 14 : 837799
# n → n/2 (n is even)
# n → 3n + 1 (n is odd)
# Which starting number, under one million, produces the longest chain?
#--------------------------------------------
def solution_14():
    cache = {1 : 4}    
    def collatz(num):        
        count = 1
        n = num
        while n > 1:
            if n in cache:
                cache[num] = count + cache[n]
                return cache[num]
            else:
                count += 1
                n = (n >> 1 if (n & 1) == 0 else (3*n) + 1)
        cache[num] = count
        return cache[num]
    max_t = 4
    for i in range(1000000):
        t = collatz(i)
        if max_t < t: 
            max_t = t
            result = i
    return result
register_solution(14, solution_14)

#--------------------------------------------
# project euler 15 : 137846528820
# Lattice paths, How many such routes are there through a 20×20 grid?
#--------------------------------------------
def solution_15():
    cache = {}
    def paths(x, y):
        if x == 0 or y == 0: return 1
        else:
            if not (x,y) in cache: cache[(x,y)] = paths(x-1, y) + paths(x, y-1)
        return cache[(x,y)]
    return paths(20, 20)
answer_func[15] = solution_15

#--------------------------------------------
# project euler 16 : 1366
# What is the sum of the digits of the number 2**1000?
#--------------------------------------------
def solution_16():
    from operator import add
    from functools import reduce
    
    return reduce(add, map(int, list(str(pow(2, 1000)))))
register_solution(16, solution_16)

#--------------------------------------------
# project euler 17 : 21124
# If all the numbers from 1 to 1000 (one thousand) inclusive were written out
# in words, how many letters would be used?
#--------------------------------------------
def solution_17():
    pairs = {
        1: "one",
        2: "two",
        3: "three",
        4: "four",
        5: "five",
        6: "six",
        7: "seven",
        8: "eight",
        9: "nine",
        10: "ten",
        11: "eleven",
        12: "twelve",
        13: "thirteen",
        14: "fourteen",
        15: "fifteen",
        16: "sixteen",
        17: "seventeen",
        18: "eighteen",
        19: "nineteen",
        20: "twenty",
        30: "thirty",
        40: "forty",
        50: "fifty",
        60: "sixty",
        70: "seventy",
        80: "eighty",
        90: "ninety",
        100: "hundred",
        1000: "thousand"
    }
    
    def convert(num):
        s = ''
        x = num
        for i in [100, 10, 1]:
            digit = x // i
            if digit > 0:
                if i == 100: 
                    s += pairs[digit] + pairs[100]
                    s += "and" if (x % i) != 0 else ""
                elif i == 10:
                    if digit == 1: s += pairs[x]; break
                    else: s += pairs[digit * 10]
                else: s += pairs[digit]
            x %= i
            if x == 0: break
        s = "onethousand" if num == 1000 else s
        return s                
    return len(''.join([convert(num) for num in range(1, 1001)]))
answer_func[17] = solution_17

#--------------------------------------------
# project euler 18 : 1074
# Find the maximum total from top to bottom of the triangle below:
#--------------------------------------------
def solution_18():
    def max_path(t):
        if len(t) == 1: return t[0][0]
        else:
            top = t.pop(0)[0]
            tl = [ row[:-1] for row in t ]
            tr = [ row[1:] for row in t ]
            return max([top + max_path(tl), top + max_path(tr)])
    triangle = []
    with open("../data/triangle_15.txt", "r") as f:
        for line in f.readlines():
            triangle.append([int(s) for s in line.rstrip().split(' ')])    
    return max_path(triangle)
register_solution(18, solution_18)

#--------------------------------------------
# project euler 19 : 171
# How many Sundays fell on the first of the month 
# during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
#--------------------------------------------
def solution_19():
    days_in_month = {
        31 : [1, 3, 5, 7, 8, 10, 12],
        30 : [4, 6, 9, 11],
        28 : [2]
    }
    weekdays = ['Mon', 'Tue', 'Wed', 'Thr', 'Fri', 'Sat', 'Sun']

    def weekdayOfMonth(year, month, day):
        def leapYear(year):
            (year % 4 == 0 and not year % 100 == 0) or year % 400 == 0
            
        def daysOfMonth(year, month):
            for k in days_in_month:
                if month in days_in_month[k]:
                    return 29 if leapYear(year) and month == 2 else k
        
        y_diff, m_diff, d_diff = 0, 0, 0
        y_diff = sum([366 if leapYear(y) else 365 for y in range(1900, year)])
        m_diff = sum([daysOfMonth(year, m) for m in range(1, month)])
        d_diff = day - 1
        
        total_diff = y_diff + m_diff + d_diff
        result = ''
        while total_diff > 0:
            for x in weekdays:
                if total_diff == 0:
                    result = x
                    break
                total_diff -= 1
        return result
    return len([(year, month, 1) for year in range(1901, 2001) for month in range(1, 13) if weekdayOfMonth(year, month, 1) == 'Sun'])
answer_func[19] = solution_19

#--------------------------------------------
# project euler 20 : 648
# Find the sum of the digits in the number 100!
#--------------------------------------------
def solution_20():
    from math import factorial
    
    digits = list(str(factorial(100)))
    return sum([int(x) for x in digits])
answer_func[20] = solution_20

#--------------------------------------------
# project euler 21 : 31626
# Evaluate the sum of all the amicable numbers under 10000.
#--------------------------------------------
def solution_21():
    def d(n):
        divisors = divisor_list(n)
        divisors.remove(n)
        return sum(divisors)
    result = 0
    for a in range(2, 10000):
        b = d(a)
        if a == d(b) and a != b: result += a    
    return result
register_solution(21, solution_21)

#--------------------------------------------
# project euler 22 : 871198282
# What is the total of all the name scores in the file?
#--------------------------------------------
def solution_22():
    def name_score(s):
        return sum([ord(c) - 64 for c in list(s)])            
    with open("../data/names.txt", "r") as f:
        names = f.readline().split(',')
        names = sorted([name[1:-1] for name in names])
    return sum([(idx+1) * name_score(name) for idx, name in enumerate(names)])    
register_solution(22, solution_22)

#--------------------------------------------
# project euler 23 : 4179871
# Find the sum of all the positive integers 
# which cannot be written as the sum of two abundant numbers.
#--------------------------------------------
def solution_23():
    def abundant_list(n):
        return list(filter(lambda x: 2 * x < sum(divisor_list(x)), range(12, n)))
    num_set = set(range(1, 28123))
    a_list = abundant_list(28123)
    a_set = set([x+y for x in a_list for y in a_list])
    return sum(num_set - a_set)
register_solution(23, solution_23)

#--------------------------------------------
# project euler 24 : 2783915460
# What is the millionth lexicographic permutation of the digits 
# 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
#--------------------------------------------
def solution_24():
    from functools import reduce
    
    def f(n):
        return reduce(lambda x, y: x*y, range(1, n+1))
    perm = list(range(0, 10))
    level = 1
    which = 1000000
    while f(level) < which:
        level += 1
    
    digits = perm[10-level:]
    for i in range(level-1, 0, -1):
        count = 0
        while which - f(i) > 0:
            count += 1
            which -= f(i)
        perm[10-(i+1)] = digits[count]
        digits.pop(count)
    
    perm = perm[:-len(digits)] + digits
    return ''.join([str(x) for x in perm])            
register_solution(24, solution_24)

#--------------------------------------------
# project euler 25 : 4782
# What is the first term in the Fibonacci sequence to contain 1000 digits?
#--------------------------------------------
def solution_25():
    for idx, fib in enumerate(fib_seq()):
        if len(str(fib)) == 1000: return idx + 2
register_solution(25, solution_25)

#--------------------------------------------
# project euler 26 : 983
# Find the value of d < 1000 for which 1/d contains 
# the longest recurring cycle in its decimal fraction part.
#--------------------------------------------
def solution_26():
    result = 0
    max = 0
    for d in range(2, 1000):
        re_list = []
        remainder = 1 % d
        while True:
            if remainder == 0: break
            elif remainder in re_list:
                start = re_list.index(remainder)
                length = len(re_list[start:])
                if max < length:
                    max = length
                    result = d
                break
            else: re_list.append(remainder)
            remainder = (10*remainder) % d
    return result
register_solution(26, solution_26)

#--------------------------------------------
# project euler 27 : -59231
# n² + an + b, where |a| < 1000 and |b| < 1000
# Find the product of the coefficients, a and b, for the quadratic expression
# that produces the maximum number of primes for consecutive values of n, starting with n = 0.
#--------------------------------------------
def solution_27():
    from itertools import takewhile
    from itertools import count
    
    max_t = (0, 0, 0)
    a = range(-999, 1000)
    b = takewhile(lambda x: x < 1000, prime_seq())
    coeff = [[y, x] for x in b for y in a]
    for a, b in coeff:
        cnt = 0
        for n in count(0, 1):
            value = n*n + a*n + b
            if is_prime(value):    
                cnt += 1
            else: break
        if max_t[0] < cnt: max_t = (cnt, a, b)
    return max_t[1] * max_t[2]
register_solution(27, solution_27)

#--------------------------------------------
# project euler 28 : 669171001
# What is the sum of the numbers on the diagonals 
# in a 1001 by 1001 spiral formed in the same way?
#--------------------------------------------
def solution_28():
    def f(n):
        if n == 1: return [1]
        else:
            start = f(n-2)[-1] + (n-1)
            return [start + (n-1)*i for i in range(4)]
    return sum([sum(f(x)) for x in range(1, 1002, 2)])
register_solution(28, solution_28)

#--------------------------------------------
# project euler 29 : 9183
# How many distinct terms are in the sequence generated by 
# a**b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
#--------------------------------------------
def solution_29():
    from itertools import product
    
    a_list = [x for x in range(2, 101)]
    b_list = sorted([pow(pair[0], pair[1]) for pair in product(a_list, a_list)])
    return len(set(b_list))
register_solution(29, solution_29)

#--------------------------------------------
# project euler 30 : 443839
# Find the sum of all the numbers that can be written as 
# the sum of fifth powers of their digits.
#--------------------------------------------
def solution_30():
    sums = 0
    for n in range(2, 354295):
        seq = map(int, list(str(n)))
        if n == sum([pow(num, 5) for num in seq]): sums += n
    return sums
register_solution(30, solution_30)

if __name__ == "__main__":
    proj = int(input("Input Problem Number : "))
    if proj == 0: solution_all()
    else: solution(proj, answer_func[proj])