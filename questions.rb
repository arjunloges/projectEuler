# Multiples of 3 and 5
# Problem 1

def sum_of_multiples(n)
  sum = 0
  while n > 3
    n -= 1
    if n % 3 == 0 || n % 5 == 0
      sum += n
    end
  end
  sum
end

puts sum_of_multiples(1000)

# Even Fibonacci numbers
# Problem 2

def fibonacci_evens(n)
  sum = 0
  current_value = 1
  sequence = [1, 1]

  i = 2
  while current_value < n
    current_value = sequence[i - 1] + sequence[i - 2]
    sequence << current_value
    if current_value.even? && current_value < n
      sum += current_value
    end
    i += 1
  end
  sum
end

puts fibonacci_evens(4000000)

# Largest prime factor
# Problem 3

def factors(n)
  factors = [1, n]
  j = n
  i = 2
    while i < j
      if n % i == 0
        factors << i
        factors << n / i
        j = n / i
      end
      i += 1
    end
  factors
end

def largest_prime(n)
  primes = []
  factors = factors(n).reverse
  factors.each do |factor|
      return factor if factors(factor).length == 2
  end
end

puts largest_prime(600851475143)

# Largest palindrome product
# Problem 4

def largest_palindrome(n)
  palindrome = 999
  second = (99..999).to_a.reverse
  i = 999
    while i > 99
      second.each do |int|
        result = (i * int)
        reversed = result.to_s.reverse.to_i
          if result == reversed && result > palindrome
            palindrome = result
          end
      end
      i -= 1
    end
  palindrome
end

puts largest_palindrome(1000)

# Smallest multiple
# Problem 5

def smallest_multiple(n)
  elements = (2..n).to_a
  largest_prime_count = {}

  elements.each do |element|
    prime_count = {}
    factorized = prime_factorization(element).to_a

      factorized.each do |num|
        if prime_count[num]
          prime_count[num] += 1
        else
          prime_count[num] = 1
        end
      end

      prime_count.each do |k, v|
          if largest_prime_count[k]
            if largest_prime_count[k] > v
            else
              largest_prime_count[k] = v
            end

          else
            largest_prime_count[k] = v
          end
      end
  end

      product = 1
      largest_prime_count.each do |k, v|
        product *= (k ** v)
      end

  product
end

def prime_factorization(n)
  primes = [2, 3, 5, 7, 11, 13, 17, 19]
  factorized = []
  remainder = n

    i = 0
    while i < primes.length
      return factorized if remainder == 1

      if remainder % primes[i] == 0
        factorized << primes[i]
        remainder /= primes[i]
        i = 0
      else
        i += 1
      end
    end
end

puts smallest_multiple(20)

# Sum square difference
# Problem 6

def sum_square_difference(n)
  sum = 0
  integers = (1..n).each {|int| sum += int}

  sum = (sum ** 2)

  product = 0
  elements = (1..n).map {|num| num ** 2}.each {|num| product += num}

  (sum - product)
end

puts sum_square_difference(100)


# 10001st prime
# Problem 7

def certain_prime(n)
  prime = 0
  ith = 0
  integer = 1

    while ith < (n + 1)
      if factors_of(integer).length == 2
        prime = integer
        ith += 1
      end
      integer += 1
    end
  prime
end

def factors_of(n)
  factors = [1, n]
  i = 2
  j = n
    while i < j
      if n % i == 0
        factors << (n / i)
        factors << i
        j = (n / i)
      end
      i += 1
    end
  factors
end

puts certain_prime(10001)

# Largest product in a series
# Problem 8

def largest_product(n, str)
  elements = str.split('').map{|el| el == "\n" ? nil : el.to_i}.compact
  product = 1
  i = 0
  j = i + (n - 1)

  while j < elements.length

    current_product = 1
    current_set = elements[i..j]

    current_set.each do |num|
      current_product *= num
    end

    product = current_product if current_product > product
    i += 1
    j += 1
  end

  product
end

str = "73167176531330624919225119674426574742355349194934
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

n = 13
puts largest_product(n, str)

# Special Pythagorean triplet
# Problem 9

def pythagorean_trip(n)
  solving = true
  a_arr = (1..n).to_a

    while solving
      a_arr.each do |a|
        b_arr = ((a + 1)..n).to_a

        b_arr.each do |b|
          c = (n - a - b)

            if trip_check(a, b, c)
              product = (a * b * c)
              solving = false
              return product
            end

        end
      end
    end
end

def trip_check(a, b, c)
  if ((a ** 2) + (b ** 2) == (c ** 2)) && (a + b + c == 1000)
    return true
  else
    return false
  end
end

puts pythagorean_trip(1000)

# Summation of primes
# Problem 10

def prime?(n)
  return true if n == 2

  j = Math.sqrt(n).to_i
    if j > 2
      integers = (2..j).to_a
    else
      integers = [2]
    end

      i = 0
      while i < integers.length
        if n % (integers[i]) == 0
          return false
        end
        i += 1
      end

  return true
end

def sum_of_primes(n)
  sum = 0
  i = 2

  while i <= n
    sum  += i if prime?(i)
    i += 1
  end

  return sum
end

puts sum_of_primes(2000000)

# Largest product in a grid
# Problem 11
#

def largest_grid_product
  arr = File.readlines('grid.txt').map{|el| el.chomp.split(' ').map(&:to_i)}
  largest_product = 0
  i = 0

    while i < arr.length
      a = 0
      b = 1
      c = 2
      d = 3
      while d < arr[i].length
        current_product = (arr[i][a] * arr[i][b] * arr[i][c] * arr[i][d])
        largest_product = current_product if current_product > largest_product
        a += 1
        b += 1
        c += 1
        d += 1
      end
      i += 1
    end

  i = 0
    while (i < arr.length - 3)
      j = i + 1
      k = i + 2
      l = i + 3
      a = 0
      while a < arr[i].length
        current_product = arr[i][a] * arr[j][a] * arr[k][a] * arr[l][a]
        largest_product = current_product if current_product > largest_product
        a += 1
      end
      i += 1
    end

    i = 0
    while (i < arr.length - 3)
      j = i + 1
      k = i + 2
      l = i + 3
      a = 0
      while (a < arr[i].length - 3)
        b = a + 1
        c = a + 2
        d = a + 3
        current_product = arr[i][a] * arr[j][b] * arr[k][c] * arr[l][d]
        largest_product = current_product if current_product > largest_product
        a += 1
      end
      i += 1
    end

    i = 0
    while i < arr.length - 3
      j = i + 1
      k = i + 2
      l = i + 3
      a = 3
      while a < arr[i].length
        b = a - 1
        c = a - 2
        d = a - 3
        current_product = arr[i][a] * arr[j][b] * arr[k][c] * arr[l][d]
        largest_product = current_product if current_product > largest_product
        a += 1
      end
      i += 1
    end

  largest_product
end

puts largest_grid_product

Highly divisible triangular number
Problem 12

def highly_divisible_triangluar(n)
  integers = [1]
    i = 0

    while i < integers.length
      factors = []
      factors = factors_of(integers[i]).uniq

      if factors.length > n
        return integers[i]
      else
        additive = integers[i] + (i + 2)
        integers << additive
        i += 1
      end
    end
end

def factors_of(n)
  factors = []
    i = 1
    j = n

    while i <= j
      if n % i == 0
        j = (n / i)
        factors << i << j
      end
      i += 1
    end

  factors
end

puts highly_divisible_triangluar(500)

Large sum
Problem 13

str = "37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690"

def large_sum(str)
  elements = str.split('').map{|el| el == "\n" ? nil : el.to_i}.compact

  fifty_digit_elements = []
  fifty_at_a_time = []
    elements.each do |el|
      fifty_at_a_time << el

        if fifty_at_a_time.length == 50
          fifty_digit_elements << fifty_at_a_time
          fifty_at_a_time = []
        end
    end

    large_sum = 0
    fifty_digit_elements.each do |arr|
      large_num = arr.join('').to_i
      large_sum += large_num
    end

    large_sum = large_sum.to_s
    return large_sum[(0..9)]
end

puts large_sum(str)


# Longest Collatz sequence
# Problem 14

def longest_collatz_sequence(n)

  longest_length = 0
  longest_collatz = 0
  i = n

  while i > 4
    current_sequence = [i]
    current_length = 0
    j = 0

    while current_sequence[j] > 1
      integer = current_sequence[j]
        if integer.even?
          integer = integer / 2
        else
          integer = (integer * 3) + 1
        end
      current_sequence << integer
      j += 1
    end
      if current_sequence.length > longest_length
        longest_length = current_sequence.length
        longest_collatz = current_sequence[0]
      end
    i -= 1

  end

  longest_collatz
end

puts longest_collatz_sequence(1000000)

Lattice paths
Problem 15

def lattice_paths(n)

  matrix = Array.new(n + 1){1}

  n.times do
    (1..n).each do |i|
      matrix[i] = matrix[i] + matrix[i - 1]

    end
  end

 matrix[-1]
end

puts lattice_paths(20)

Power digit sum
Problem 16

def power_digit_sum(n, exp)
  sum = 0
  result = (n ** exp).to_s
  arr_of_result = result.split('').map {|i| i.to_i}

    arr_of_result.each do |int|
      sum += int
    end

  sum
end

puts power_digit_sum(2, 1000)

Number letter counts
Problem 17

def number_letter_counts(n)
  elements = (1..n).to_a
  sum = 0

  elements.each do |num|
    sum += letter_count(num)
  end

  sum
end

def letter_count(n)
  integer = n
  sum = 0

  letter_hash = {1 => "one", 2 => "two", 3 => "three", 4 => "four", 5 => "five",
    6 => "six", 7 => "seven", 8 => "eight", 9 => "nine", 10 => "ten", 11 => "eleven",
   12 => "twelve", 13 => "thirteen", 14 => "fourteen", 15 => "fifteen", 16 => "sixteen",
  17 => "seventeen", 18 => "eighteen", 19 => "nineteen", 20 => "twenty", 30 => "thirty",
 40 => "forty", 50 => "fifty", 60 => "sixty", 70 => "seventy", 80 => "eighty", 90 => "ninety",
 100 => "hundred"}

  while integer > 0
    element = integer.to_s

    if element.length > 2
      if element.length == 4
        sum += 11
        integer = 0
      elsif element.length == 3 && integer % 100 == 0
        subtract = element[0].to_i
        sum += letter_hash[subtract].length
        sum += letter_hash[100].length
        integer -= (subtract * 100)
      elsif element.length == 3 && integer % 100 != 0
        subtract = element[0].to_i
        sum += letter_hash[subtract].length
        sum += letter_hash[100].length
        sum += 3
        integer -= (subtract * 100)
      end
    elsif element.length <= 2
      if integer < 21
        sum += letter_hash[integer].length
        integer = 0
      elsif integer >= 21 && integer % 10 == 0
        sum += letter_hash[integer].length
        integer = 0
      elsif integer >= 21
        subtract = element[1].to_i
        integer -= subtract
        sum += letter_hash[subtract].length
      end
    end
  end
  sum
end

puts number_letter_counts(1000)


# Maximum path sum I
# Problem 18

def max_path_sum
  triangle = File.readlines('triangle.txt').map do |line|
                line.split(" ").map(&:to_i)
            end

   while triangle.length > 1
     arr1 = triangle.last
     arr2 = triangle[-2]
     result = combine(arr1, arr2)
     triangle.pop
     triangle.pop
     triangle << result
   end

   triangle
end

def combine(arr1, arr2)
  result = []
  i = 0

  while i < arr2.length
    j = i
    sum1 = arr2[i] + arr1[j]
    sum2 = arr2[i] + arr1[j + 1]

    if sum1 >= sum2
      result << sum1
    elsif sum2 > sum1
      result << sum2
    end

    i += 1
  end

  result
end
 puts max_path_sum

# Counting Sundays
# Problem 19

def counting_sundays(year_start, year_end)
  week =  ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]
  start_day = "Tuesday"
  current_index = 2
  month_days = {"January" => 31, "February" => 28, "March" => 31, "April" => 30,
     "May" => 31, "June" => 30, "July" => 31, "August" => 31,
      "September" => 30, "October" => 31, "November" => 30, "December" => 31}
  years = (year_start..year_end).to_a
  sunday_count = 0

    years.each do |year|

      month_days.each do |month, days|

        if year % 4 != 0 && year % 400 != 0
          current_index = (current_index + days) % 7
          start_day = week[current_index]
        elsif year % 4 == 0

          if month == "February"
            current_index = (current_index + days + 1) % 7
            start_day = week[current_index]
          else
            current_index = (current_index + days) % 7
            start_day = week[current_index]
          end

        end
        sunday_count += 1 if start_day == "Sunday"
      end

    end

  sunday_count
end

puts counting_sundays(1901, 2000)

#Factorial digit sum
# Problem 20

def factorial_digit_sum(n)
  integers = (1..n).to_a
  multi_result = 1
  sum = 0

    integers.each do |num|
      multi_result *= num
    end

    product = multi_result.to_s.split('').map {|num| num.to_i}
    product.each do |int|
      sum += int
    end

  sum
end

puts factorial_digit_sum(100)

# Amicable numbers
# Problem 21

def amicable_numbers(n)
  integers = (1...n)
  sum = 0
  amicable_nums = []

  integers.each do |int|
    current_sum = sum_of_divisors(int)
    amicable_sum = sum_of_divisors(current_sum)
      if (int == amicable_sum) && (int != current_sum)
        amicable_nums << int << current_sum
      end
  end

    amicable_nums = amicable_nums.uniq

    amicable_nums.each do |num|
      sum += num
    end

  sum
end

def sum_of_divisors(integer)
  divisors = [1]
  sum = 0
  i = integer
  j = 2

    while j <= i
      if integer % j == 0
        i = integer / j
        divisors << j unless j == integer
        divisors << i unless i == integer
      end
      j += 1
    end

    divisors = divisors.uniq
    divisors.each do |int|
      sum += int
    end

  sum
end

puts amicable_numbers(10000)

# Names scores
# Problem 22

def names_scores
  array_of_names = File.read('names.txt').chomp.split(",").map{|name| name[1..-2]}.sort!
  alphabet = ("A".."Z").zip(1..26).to_h
  sum = 0

  array_of_names.each do |name|
    i = (array_of_names.index(name) + 1)
    current_name = name.split('')
    current_sum = 0

    current_name.each do |letter|
      current_sum += alphabet[letter]
    end

    current_sum = current_sum * i
    sum += current_sum
  end

  sum
end
puts names_scores


# Non-abundant sums
# Problem 23
#
def non_abundant_sums(limit)
  abundant_nums = []
  non_abundant_sum = 0
  abundant_sums = []

  (1..limit).each do |num|
    abundant_nums << num if abundant?(num)
  end


     abundant_nums.each do |i|
      current_sum = 0
      j = abundant_nums.index(i)

      while j < abundant_nums.length && current_sum < limit
        current_sum = i + abundant_nums[j]
        if current_sum < limit
          abundant_sums << current_sum unless abundant_sums.include?(current_sum)
        end
        j += 1
      end
    end

      (1...limit).each do |int|
        non_abundant_sum += int unless abundant_sums.include?(int)
      end

  non_abundant_sum
end

def abundant?(n)
  factors = []
  i = 1
  j = n

    while i < j
      if n % i == 0
        factors << i
        j = n / i
        factors << j unless j == n
      end
      i += 1
    end

    factors.uniq!
    return true ? factors.sum > n : false
end

puts non_abundant_sums(28123)

# Lexicographic permutations
# Problem 24

def lexicographic_permutations(n, limit)
  sum = 1
  possible_integers = (0..n).to_a
  max_possibilities = (1..possible_integers.length).map {|j| sum *= j}.last

  answer = ""
  answer_length = 0
  current_length = 0

    while answer_length < limit

      i = nil
      while i == nil
        max_possibilities = (max_possibilities / (possible_integers.length))
        i = (limit - answer_length) / max_possibilities
        current_length += ((max_possibilities * i))
        answer_length = current_length + 1
        answer << possible_integers[i].to_s
        possible_integers.delete_at(i)
      end

      sum = 1
      max_possibilities = (1..possible_integers.length).map {|j| sum *= j}.last
      answer << possible_integers[0].to_s if possible_integers.length == 1

    end

  answer.to_i
end

puts lexicographic_permutations(9, 1000000)

# 1000-digit Fibonacci number
# Problem 25

def fibs_idx(n)
  first = 1
  second = 1
  solving = true
  index = 2

    while solving
      index += 1
      current_fibs = first + second
      fibs_str = current_fibs.to_s
        if fibs_str.length == n
          solving = false
        else
          first = second
          second = current_fibs
          solving = true
        end
    end
  index
end

puts fibs_idx(1000)

# Reciprocal cycles
# Problem 26

def reciprocal_cycles(n)
  integers = (2...n).to_a
  longest_length = 0
  longest_denominator = 0

  integers.each do |num|
    current_length = 1
    magic_num = 9
    solving = true

      while solving
        if magic_num % num == 0
          solving = false
        else
          current_length += 1
          magic_num *= 10
          magic_num += 9
        end
        solving = false if magic_num > (10 ** n)
      end

        if current_length > longest_length && magic_num % num == 0
          longest_length = current_length
          longest_denominator = num
        end
  end

  longest_denominator
end
puts reciprocal_cycles(1000)

# Quadratic primes
# Problem 27

require 'mathn'
def quadratic_primes(n)
  pair = []
  max_integers = 0

    ((-n)..n).each do |a|
      (0..n).each do |b|
        i = 0
        while ((i ** 2) + (a * i) + b).prime?
          i += 1
        end

          if i > max_integers
            max_integers = i
            pair = [a, b]
          end
      end
    end

  puts (pair[0] * pair[1])
end

puts quadratic_primes(1000)

Number spiral diagonals
Problem 28

def spiral_diagonals(n)
  first = [1]
  second = [1]
  third = [1]
  fourth = [1]
  i = 2
  j = i + 2
  k = i + 4
  l = i + 6

  while (first.length * 2) < n + 1
      first << (first[-1] + i)
      second << (second[-1] + j)
      third << (third[-1] + k)
      fourth << (fourth[-1] + l)

      i += 8
      j += 8
      k += 8
      l += 8
  end

  spiral = first + second + third + fourth
  sum = -3

  spiral.each do |int|
    sum += int
  end

  sum
end

puts spiral_diagonals(1001)

# Distinct powers
# Problem 29

def distinct_powers(n)
  integers = (2..n).to_a
  powers = (2..n).to_a
  unique_powers = []

    integers.each do |int|

      powers.each do |pwr|
        result = int ** pwr
        unique_powers << result unless unique_powers.include?(result)
      end
    end
    unique_powers.length
end

puts distinct_powers(100)

Digit fifth powers
Problem 30

def sum_of_powers(n)
  sum = 0
  power_list = []
  i = 10
  limit = (10 ** (n + 1))

    while i < limit
      power_list << i if power_sum(i, n)
      i += 1
    end
      power_list.each do |int|
        sum += int
      end

  sum
end

def power_sum(num, n)

  digits = num.to_s.split('').map {|integer| integer.to_i}
  sum = 0

    digits.each do |digit|
      sum += (digit ** n)
    end

  return true ? sum == num : false
end

puts sum_of_powers(5)
