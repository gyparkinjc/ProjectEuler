module Euler
    $answers = {}
    File.open("../data/euler_answer.txt", "r") do |f|
        f.each do |line|        
            a_list = line.split()
            num = a_list[0].sub(/\./, '').to_i
            $answers[num] = a_list[1]
        end
    end
    
    def solution(proj)
        start_time = Time.now
        answer = eval("solution_#{proj}")
        elapsed_time = Time.now - start_time
        puts "[Proj #{proj}] : #{answer.to_s == $answers[proj]} #{answer} Time : #{elapsed_time}"
        $stdout.flush
    end
    
    def solution_all(n)
        start_time = Time.now
        (1..n).each do |proj|
            begin
                solution(proj)
            rescue
                p "#{proj}th solution is not found"
            end
        end
        elapsed_time = Time.now - start_time
        p "[total time] : #{elapsed_time}"
    end
    
    #--------------------------------------------
    # Utility functions
    #--------------------------------------------
    def sequence(&block)
        Enumerator.new do |y|
            n = 0
            loop do
                y.yield block.call(n)
                n += 1
            end
        end
    end
    $fibs = Enumerator.new do |y|
        a = b = 1
        y << a
        loop do
            y << a
            a, b = a + b, a
        end
    end
    $primes = Enumerator.new do |y|
        y << 2
        p = 1
        loop do
            y << p if is_prime?(p)
            p += 2
        end        
    end
    $tri_numbers = Enumerator.new do |y|
        a = cnt = 1
        loop do
            cnt += 1
            y.yield a
            a += cnt            
        end
    end
    def is_prime?(value)
        return true if value == 2
        return false if value < 2 or value % 2 == 0
        is_prime = true
        (3..Math.sqrt(value).ceil).step(2) do |x| 
            if value % x == 0
                is_prime = false
                break
            end
        end
        is_prime
    end
    def is_palindrome?(x)
        x == x.to_s.reverse.to_i
    end
    def prime_factors(value)
        factors = []
        $primes.each do |p|
            while value % p == 0
                value /= p
                factors.push(p)
            end
            break if value == 1
        end
        factors
    end
    require 'set'
    def divisor_list(value)
        a_list = prime_factors(value)
        divisors = a_list.to_set
        divisors.add(1)
        (2..a_list.length).each do |i|
            a_list.combination(i).each do |pair|
                divisors.add(pair.reduce(:*))
            end
        end
        divisors
    end    

    #--------------------------------------------
    # project euler 1 : 233168
    # Find the sum of all the multiples of 3 or 5 below 1000.
    #--------------------------------------------
    def solution_1
        (1...1000).select { |x| x % 3 == 0 or x % 5 == 0 }.reduce(:+)
    end
    
    #--------------------------------------------
    # project euler 2 : 4613732
    # Find the sum of the even-valued fibonacci numbers under 4000000.
    #--------------------------------------------
    def solution_2
        $fibs.take_while { |x| x < 4000000 }.select { |x| x % 2 == 0 }.reduce(:+)
    end

    #--------------------------------------------
    # project euler 3 : 6857
    # What is the largest prime factor of the number 600851475143?
    #--------------------------------------------
    def solution_3
        value = 600851475143
        $primes.each do |x|
            while value % x == 0
                value /= x
            end
            return x if value == 1
        end
    end

    #--------------------------------------------
    # project euler 4 : [993, 913, 906609]
    # Find the largest palindrome made from the product of two 3-digit numbers.
    #--------------------------------------------
    def solution_4        
        def product_seq(n)
            Enumerator.new do |y|
                (100...n).each do |a|
                    (a...n).each { |b| y.yield a*b }
                end
            end
        end

        product_seq(1000).select { |x| is_palindrome?(x) }.max        
    end

    #--------------------------------------------
    # project euler 5 : 232792560
    # What is the smallest positive number that is evenly divisible 
    # by all of the numbers from 1 to 20? (scm : smallest common muliplier)
    #--------------------------------------------
    def solution_5
        (2..20).inject do |memo, i|
            a, b, scm = memo, i, 1
            $primes.each do |p|
                if p > a or p > b
                    scm = scm * a * b
                    break
                end
                while a % p == 0 and b % p == 0
                    scm *= p
                    a /= p
                    b /= p
                end
            end
            scm
        end
    end
    
    #--------------------------------------------
    # project euler 6 : 25164150
    # Find the difference between the sum of the squares of 
    # the first one hundred natural numbers and the square of the sum.
    #--------------------------------------------
    def solution_6
        sum_square = (1..100).map { |x| x * x }.reduce(:+)
        square_sum = (1..100).reduce(:+) ** 2
        square_sum - sum_square
    end

    #--------------------------------------------
    # project euler 7 : 104743
    # What is the 10001st prime number?
    #--------------------------------------------
    def solution_7
        i = 1
        $primes.take(10001)[-1]
    end

    #--------------------------------------------
    # project euler 8 : 40824
    # Find the greatest product of five consecutive digits 
    # in the 1000-digit number.
    #--------------------------------------------
    def solution_8
        digits = 
           "73167176531330624919225119674426574742355349194934 
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
            71636269561882670428252483600823257530420752963450"            
        digits.gsub!(/[\t\n]/, '')
        
        elements = []
        digits = digits.split(//)
        digits.each_index do |i|
            product = 1
            digits[i..i+4].each { |e| product *= e.to_i }
            elements.push(product)
        end
        
        elements.max
    end
    
    #--------------------------------------------
    # project euler 9 : 
    # There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    # Find the product abc.
    #--------------------------------------------
    def solution_9
        pairs = []
        (2..1000).each do |a|
            (a..1000).each do |c|
                t1 = 1000 - a.to_f
                t2 = (a**2).to_f
                if (2 * c).to_f == (t2 / t1) + t1
                    pairs = [a, 1000-(a+c), c]
                    return pairs.reduce(:*)
                end
            end
        end
    end
    
    #--------------------------------------------
    # project euler 10 : 142913828922
    # Find the sum of all the primes below two million.
    #--------------------------------------------
    def solution_10
        sum = 0
        $primes.take_while { |x| x < 2000000 }.reduce(:+)
    end

    #--------------------------------------------
    # project euler 11 : 70600674
    # What is the greatest product of four adjacent numbers 
    # in the same direction (up, down, left, right, or diagonally) in the 20×20 grid?
    #--------------------------------------------
    def solution_11
        grid = []
        File.open("../data/20x20.txt", "r") do |f|
            row = []
            3.times { grid.push(Array.new(26, 1)) }
            f.each do |line|
                row = [1, 1, 1]
                row += line.split(' ').map { |i| i.to_i }
                row += [1, 1, 1]
                grid.push(row)
            end            
            3.times { grid.push(Array.new(26, 1)) }
        end
        
        a_list = []
        (3..22).each do |i|
            (3..22).each do |j|
                row_p, col_p, rdia_p, ldia_p = 1, 1, 1, 1
                (0..3).each do |k|
                    row_p *= grid[i][j+k]
                    col_p *= grid[i+k][j]
                    rdia_p *= grid[i+k][j+k]
                    ldia_p *= grid[i+k][j-k]
                end
                a_list.push([row_p, col_p, rdia_p, ldia_p].max)
            end            
        end
        a_list.max
    end
    
    #--------------------------------------------
    # project euler 12 : 76576500
    # What is the value of the first triangle number 
    # to have over five hundred divisors?
    #--------------------------------------------
    def solution_12
        def num_of_divisor(n)
            prime_factors(n)
            .map { |p| [p, 1] }
            .inject([]) do |acc, pair|
                if acc.empty? or acc.last.first != pair.first
                    acc.push(pair)
                else
                    temp = acc.pop
                    acc.push([pair.first, pair.last + temp.last])
                end
            end
            .map { |pair| pair.last + 1 }
            .inject(1) { |acc, n| acc * n }
        end
        $tri_numbers.each { |v| return v if num_of_divisor(v) > 500 }
    end
    
    #--------------------------------------------
    # project euler 13 : 5537376230
    # Work out the first ten digits of the sum of the following 
    # one-hundred 50-digit numbers.
    #--------------------------------------------
    def solution_13
        IO.readlines("../data/numbers.txt").map { |x| x.to_i }.reduce(:+).to_s[0..9]
    end

    #--------------------------------------------
    # project euler 14 : 837799
    # n → n/2 (n is even)
    # n → 3n + 1 (n is odd)
    # Which starting number, under one million, produces the longest chain?
    #--------------------------------------------
    def solution_14
        cache = {1 => 4}
        collatz = lambda do |num|
            count = 1
            n = num
            while n > 1
                if cache.has_key?(n)
                    cache[num] = count + cache[n]
                    return cache[num]
                else                
                    (n % 2 == 0) ? (n = n / 2) : (n = 3*n + 1)
                    count += 1
                end
            end
            cache[num] = count
            cache[num]
        end
        max_value = [1, 4]
        (1..1000000).each do |n|
            value = [n, collatz.call(n)]
            max_value = value if max_value[1] < value[1]
        end
        max_value[0]
    end
    
    #--------------------------------------------
    # project euler 15 : 137846528820
    # Lattice paths, How many such routes are there through a 20×20 grid?
    #--------------------------------------------
    def solution_15
        cache = {}
        paths = lambda do |x, y|
            if x == 0 or y == 0 then return 1
            else
                cache[[x,y]] = paths.call(x-1, y) + paths.call(x, y-1) if not cache.has_key?([x,y])
            end            
            return cache[[x,y]]
        end
        paths.call(20,20)
    end

    #--------------------------------------------
    # project euler 16 : 1366
    # What is the sum of the digits of the number 2**1000?
    #--------------------------------------------
    def solution_16
        (2**1000).to_s.split('').map { |s| s.to_i }.reduce(:+)
    end
    
    #--------------------------------------------
    # project euler 17 : 21124
    # If all the numbers from 1 to 1000 (one thousand) inclusive were written out
    # in words, how many letters would be used?
    #--------------------------------------------
    def solution_17
        pairs = {
            1 => "one",
            2 => "two",
            3 => "three",
            4 => "four",
            5 => "five",
            6 => "six",
            7 => "seven",
            8 => "eight",
            9 => "nine",
            10 => "ten",
            11 => "eleven",
            12 => "twelve",
            13 => "thirteen",
            14 => "fourteen",
            15 => "fifteen",
            16 => "sixteen",
            17 => "seventeen",
            18 => "eighteen",
            19 => "nineteen",
            20 => "twenty",
            30 => "thirty",
            40 => "forty",
            50 => "fifty",
            60 => "sixty",
            70 => "seventy",
            80 => "eighty",
            90 => "ninety",
            100 => "hundred",
            1000 => "thousand"
        }
        
        (1..1000).map do |num|
            s = ''
            x = num
            [100, 10, 1].each do |i|
                digit = x / i
                if digit > 0
                    if i == 100
                        s += pairs[digit] + pairs[100]
                        s += 'and' if x % i != 0
                    elsif i == 10
                        s += (digit == 1) ? pairs[x] : pairs[digit * 10]
                        break if digit == 1
                    else
                        s += pairs[digit]
                    end
                end                
                x = x % i
                break if x == 0
            end
            s = (num == 1000) ? 'onethousand' : s
        end.join('').length
    end
    
    #--------------------------------------------
    # project euler 18 : 1074
    # Find the maximum total from top to bottom of the triangle below:
    #--------------------------------------------
    def solution_18
        def max_path(t)
            if t.length == 1
                return t[0][0]
            else
                top = t.delete_at(0)[0]
                tl = t.map { |row| row[0..-2] }
                tr = t.map { |row| row[1..-1] }
                return [top + max_path(tl), top + max_path(tr)].max
            end
        end            
        triangle = []
        File.open("../data/triangle_15.txt", "r") do |f|
            f.each { |line| triangle.push(line.chomp.split(' ')) }
            triangle.map! do |row|
                row.map! { |s| s.to_i }
            end
        end
        return max_path(triangle)
    end        

    #--------------------------------------------
    # project euler 19 : 171
    # How many Sundays fell on the first of the month 
    # during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
    #--------------------------------------------
    def solution_19
        def weekdayOfDate(year, month, day)
            def leapYear?(x)
                ((x % 4 == 0) and (not x % 100 == 0)) or (x % 400 == 0)
            end
            
            days_in_month = {
                31 => [1, 3, 5, 7, 8, 10, 12],
                30 => [4, 6, 9, 11],
                28 => [2]
            }
            weekdays = ['Mon', 'Tue', 'Wed', 'Thr', 'Fri', 'Sat', 'Sun']            
            
            daysOfMonth = lambda do |year, month|
                days_in_month.each_pair do |key, value|
                    if value.include?(month)
                        return (leapYear?(year) and month == 2) ? 29 : key
                    end
                end
            end
            
            y_diff, m_diff, d_diff = 0, 0, 0
            (1900...year).each { |y| y_diff += (leapYear?(y) ? 366 : 365) }
            (1...month).each { |m| m_diff += daysOfMonth.call(year, month) }
            d_diff += day - 1
            
            total_diff = y_diff + m_diff + d_diff
            
            result = ''
            weekdays.cycle do |x|
                if total_diff == 0
                    result = x
                    break
                end
                total_diff -= 1
            end

            return result
        end

        years = (1901..2000); months = (1..12); sundays = 0
        years.each do |year|
            months.each { |month| sundays += 1 if 'Sun' == weekdayOfDate(year, month, 1) }
        end
        sundays
    end

    #--------------------------------------------
    # project euler 20 : 648
    # Find the sum of the digits in the number 100!
    #--------------------------------------------
    def solution_20
        (2..100).reduce(1, :*).to_s.split('').inject(0) { |sum, i| sum + i.to_i }
    end
    
    #--------------------------------------------
    # project euler 21 : 31626
    # Evaluate the sum of all the amicable numbers under 10000.
    #--------------------------------------------
    def solution_21
        def d(n)
            divisors = divisor_list(n)
            divisors.delete(n)
            divisors.reduce(:+)
        end
        result = 0
        (2..9999).each do |a|
            b = d(a)
            result += a if a == d(b) and a != b
        end
        result
    end
    
    #--------------------------------------------
    # project euler 22 : 871198282
    # What is the total of all the name scores in the file?
    #--------------------------------------------
    def solution_22
        def name_score(s)
            s.split('').map { |c| c.ord - 64 }.reduce(:+)
        end
        names = ''
        File.open("../data/names.txt", "r") { |f| names = f.read }
        names = names.split(',').map { |name| name[1..-2] }.sort
        names.each_with_index.map { |name, idx| (idx+1) * name_score(name) }.reduce(:+)        
    end
    
    #--------------------------------------------
    # project euler 23 : 4179871
    # Find the sum of all the positive integers 
    # which cannot be written as the sum of two abundant numbers.
    #--------------------------------------------
    def solution_23
        def abundant_list(n)
            (12...n).select { |num| 2 * num < divisor_list(num).reduce(:+) }
        end
        num_set = (1...28123).to_set
        a_list = abundant_list(28123)
        a_set = [].to_set
        a_list.each do |x|
            a_list.each { |y| a_set.add(x + y) }
        end
        (num_set - a_set).reduce(:+)
    end
    
    #--------------------------------------------
    # project euler 24 : 2783915460
    # What is the millionth lexicographic permutation of the digits 
    # 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
    #--------------------------------------------
    def solution_24
        def f(n)
            (1..n).reduce(:*)
        end
        
        perm = (0..9).to_a
        level = 1        
        which = 1000000
        while f(level) < which
            level += 1
        end
        
        digits = perm[10-level..-1]
        (level-1).downto(1).each do |i|
            count = 0
            while which - f(i) > 0
                count += 1
                which -= f(i)
            end
            perm[10-(i+1)] = digits[count]
            digits.delete_at(count)
        end
        perm = perm[0..-(digits.length+1)] + digits
        perm.join('').to_i        
    end
    
    #--------------------------------------------
    # project euler 25 : 4782
    # What is the first term in the Fibonacci sequence to contain 1000 digits?
    #--------------------------------------------
    def solution_25
        $fibs.each_with_index.select { |x, idx| return idx+1 if x.to_s.length == 1000 }
    end
    
    #--------------------------------------------
    # project euler 26 : 983
    # Find the value of d < 1000 for which 1/d contains 
    # the longest recurring cycle in its decimal fraction part.
    #--------------------------------------------
    def solution_26
        result = 0
        max = 0
        (2..999).each do |d|
            re_list = []
            remainder = 1 % d
            loop do                
                if remainder == 0
                    break
                elsif re_list.include?(remainder)
                    start = re_list.index(remainder)
                    len = re_list[start..-1].length
                    if max < len
                        max = len
                        result = d
                    end
                    break
                else                    
                    re_list.push(remainder)
                end
                remainder = (10*remainder) % d
            end
        end
        result
    end
    
    #--------------------------------------------
    # project euler 27 : -59231
    # n² + an + b, where |a| < 1000 and |b| < 1000
    # Find the product of the coefficients, a and b, for the quadratic expression
    # that produces the maximum number of primes for consecutive values of n, starting with n = 0.
    #--------------------------------------------
    def solution_27
        max_t = [0, 0, 0]
        a = (-999..999)
        b = []; coeff = []
        b = $primes.take_while { |p| p < 1000 }
        b.each do |x|
            a.each { |y| coeff.push([y, x]) }
        end
        coeff.each do |a, b|
            count = 0
            sequence { |i| i }.each do |n|
                value = n*n + a*n + b
                if is_prime?(value)
                    count += 1
                else
                    break
                end
            end
            max_t = [count, a, b] if max_t[0] < count
        end
        max_t[1] * max_t[2]
    end
    
    #--------------------------------------------
    # project euler 28 : 669171001
    # What is the sum of the numbers on the diagonals 
    # in a 1001 by 1001 spiral formed in the same way?
    #--------------------------------------------
    def solution_28
        def f(n)
            if n == 1
                return [1]
            else
                start = f(n-2).last + (n-1)
                return (0..3).map { |i| start + (n-1)*i }
            end
        end
        sum = 0
        1.step(1001, 2) { |i| sum += f(i).reduce(:+) }
        sum
    end
    
    #--------------------------------------------
    # project euler 29 : 9183
    # How many distinct terms are in the sequence generated by 
    # a**b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
    #--------------------------------------------
    def solution_29
        a = (2..100).to_a
        a.product(a).map { |pair| pair[0]**pair[1] }.sort.to_set.length
    end
    
    #--------------------------------------------
    # project euler 30 : 443839
    # Find the sum of all the numbers that can be written as 
    # the sum of fifth powers of their digits.
    #--------------------------------------------
    def solution_30
        sums = 0
        (2..354294).select do |n|
            if n == n.to_s.each_char.map(&:to_i).inject(0) { |sum, n| sum += n**5 }
                sums += n
            end
        end
        sums
    end
end

include Euler
print "Input Problem Number : "
$stdout.flush
proj = gets.chomp.to_i
(proj == 0) ? solution_all(30) : solution(proj)
