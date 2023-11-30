package aoc
package utils

def lcm(a: Long, b: Long): Long =
  a * b / gcd(a, b)

def gcd(a: Long, b: Long): Long =
  if b == 0 then a else gcd(b, a % b)
