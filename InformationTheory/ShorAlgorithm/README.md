### Shor's Algorithm

From [wikipedia](https://en.wikipedia.org/wiki/Shor%27s_algorithm):

Shor's algorithm is a polynomial-time quantum computer algorithm for integer factorization. Informally, it solves the 
following problem: Given an integer *N*, find its prime factors. It was invented in 1994 by the American 
mathematician Peter Shor.

On a quantum computer, to factor an integer *N*, Shor's algorithm runs in polynomial time 
(the time taken is polynomial in *log N*, the size of the integer given as input).

#### Classical simulation of a quantum algorithm
Note that the algorithm is a **simulation** of the quantum version, following step by step the protocol that a quantum
would run. Besides, since this code is running on a classical computer it never could achieve factorization in polynomial
time.

#### Installation requirements

**Python**: v2.7.15 (any 2.7 should be fine)

The only required library is numpy as shown in the requirements.txt.

In order to run the code you should execute from the ShorAlgorithm folder:
```
python ShorAlgSim.py
```

The program will ask you to introduce the number you want to factorize, write it and  the program will return 2
 factors of it.