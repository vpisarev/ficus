/*
    This file is a part of ficus language project.
    See ficus/LICENSE for the licensing terms
*/

from UTest import *
import Set, Map, Hashmap, Hashset

TEST("parallel.primes", fun()
{
    var primes: int list = []

    fun is_prime(n: int)
    {
        if n <= 1 {false} else if n % 2 == 0 {n == 2}
        else {
            all(for p<-3:floor(sqrt(double(n)))+1:2 {n % p != 0})
        }
    }

    @parallel for i <- 10000000:10100000 {
        if is_prime(i) @sync {
            primes = i :: primes
        }
    }

    EXPECT_EQ(`primes.length()`, 6241)
})
