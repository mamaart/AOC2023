from math import sqrt, floor, ceil
from functools import reduce

def rng (a,y): 
   low  = (a - sqrt((a**2-4*y)))/2
   high = (a + sqrt(a**2-4*y))/2
   return len(list(range(
       int(low+1)   if int(low)  == low  else ceil(low), 
       (int(high-1) if int(high) == high else floor(high)) + 1
   )))


data  = [[int(x) for x in x.split()[1:]] for x in open('./myinput.txt')]
pairs = list(zip(data[0], data[1]))

print(reduce(lambda a, b: a * b, [rng(x,y) for (x,y) in pairs]))
