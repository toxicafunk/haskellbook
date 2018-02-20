-- learn.hs

module Learn where

x= 10 * 5 + y

myResult = x * 5

y = 10

mult1 = x * y
  where x = 5
        y = 6

t1 = x * 3 + y
  where x = 3
        y = 1000

t3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

triple x = x * 3

waxOff x = triple x
