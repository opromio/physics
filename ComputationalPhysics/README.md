# Introduction
This are all the practices done for the Computational Physics course. They were a total of 10 and below I will include a brief explanation of what they do.

**Important note:** All the code is written in Fortran77, which is quite outdated nowadays. To compile it you will need gfortran compiler and the command:
```
gfortran NameOfFile.f -o outfile.out
```
I have tested the code and everything should be fine but if you find any problem you can report a bug and I will have a look into it.

If the compilation does not rise any problem you will be able to run the executable by:
```
./outfile.out
``` 
 
Generally this will generate one or more .dat files with the result of the calculations. In all practices you will find several .gnu files that will graph the output to better visualize the represented behaviour. If you have Gnuplot installed to run a .gnu script simply run:

```
gnuplot NameOfFile.gnu
```

Finally, is also worth noting that inside each folder you will find  a PDF file with the problem definition given by the professors. Only problem is that it is only available in Spanish or Catalan.

### List of Practices

I. Testing Fortran & Gnuplot. Computing and plotting functions.

II. Subroutines & Functions. I/O and Interpolation.

III. Numeric Integration techniques.

IV. Van der Waals equation analysis. Deriving by Newton-Raphson and Bisection methods. Comparison between both and finite differences methods.

V. Gaussian distribution. Acceptance-Rejection Technique to Generate Random Variate.

VI. Montecarlo method for integration. Single and Multi-dimensional Montecarlo integration

VII. ODEs resolution techniques. Euler method for armonic equation.

VIII. ODEs resolution techniques. Runge-Kutta. Computing eigenvalues and eigenvectors of the Schrödinger Equation.

IX. The Poisson equation. PDE resolution techniques. Gauss-Seidel, Jacobi and Successive over relaxation methods.

X. The Possion equation. Parabolic equations, Crank-Nicholson method.