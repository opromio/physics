import numpy
import random
import timeit


def gcd(a, b):
    while b != 0:
        c = a % b
        a = b
        b = c
    return a


Ni = int(raw_input('Enter N (the integer number you would like to factorize): '))
N = Ni
lt = []
per = []
factor = 1
g = True
start = timeit.default_timer()
while g:
    a = random.randrange(1, N, 1)
    #    print(a)
    for i in range(N):
        lt.append(pow(a, i, N))
    # print(lt)
    arr = numpy.array(lt)
    fourier = numpy.fft.fft(arr)
    n = arr.size
    freq = numpy.fft.fftfreq(n)
    # print(period)
    for i in range(1, n):
        period = 1 / freq[i]
        r = abs(int(period))
        if pow(a, 0, N) == pow(a, r, N) and r % 2 == 0 and pow(a, r / 2) != 1:
            per.append(r)
        else:
            continue
    for i in range(len(per)):
        r = per[i]
        n1 = gcd(pow(a, r / 2) + 1, N)
        n2 = gcd(pow(a, r / 2) - 1, N)

        if (n1 != 1) and (n2 != 1) and (n1 != N) and (n2 != N):
            if n1 * n2 == N:
                print 'Two factors of N=', Ni, 'are:', factor * int(n1), ',', int(n2)
                stop = timeit.default_timer()
                print(stop - start)
                exit()
            else:
                N = N / max(n1, n2)
                factor = factor * max(n1, n2)
                if N * factor == Ni:
                    print 'Two factors of N=', Ni, 'are:', factor, ',', N
                    stop = timeit.default_timer()
                    print(stop - start)
                    exit()
                continue
        else:
            continue
