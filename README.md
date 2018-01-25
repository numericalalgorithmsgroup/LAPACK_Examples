# Example Programs for LAPACK Routines
This repository contains example programs written in Fortran 90 showing
how to call double precision versions of most top-level LAPACK (driver
and computational) routines.

The programs are derived from the examples used in documentation of these
routines for the NAG Fortran Library https://www.nag.com/nag-fortran-library,
but they don't require use of that library because everything needed is
here in this repository, including various auxiliary routines.

# File organization
Once you've taken a copy of the repository, you'll find these items:
  * interface_blocks    - a directory containing a few Fortran module files
                          used when compiling the rest of the example source
  * auxiliary           - a small set of auxiliary routines used by the
                          example programs, for sorting and printing results
  * examples/source     - source code of individual LAPACK examples
  * examples/data       - input data files, one needed by each LAPACK example
  * GNUmakefile         - a makefile that can be used (with minor modification)
                          to compile and run all the LAPACK examples

# How to build manually
We assume that you already have a library of LAPACK and BLAS routines that
you can link the example programs to; they may be libraries that you have
compiled yourself, from the netlib LAPACK source code, or they may be libraries
obtained from elsewhere, e.g. the NAG Library, Intel's MKL, or the ARM
Performance Library. These libraries must be compatible with the compiler
that you use to compile the current example programs. Here we assume use
of the NAG Fortran compiler, nagfor, but you can  use any other modern
Fortran compiler of your choice.

  * Go to the interface_blocks directory, and compile the file
    lapack_precision.f90:
```
    cd interface_blocks
    nagfor -c lapack_precision.f90
```
This module defines dp and sp, double and single precision
kind types respectively, and it is needed by almost all the
other Fortran files.

  * In the same interface_blocks directory, compile the rest of the files:
```
    nagfor -c *.f90
```
The object files produced by these compilations are not important, but
the module files produced are.

  * Go to the auxiliary directory, and compile all the files there, using the
    modules in the interface_blocks directory:
```
    cd auxiliary
    nagfor -c -I../interface_blocks *.f90
```
  * Still in the auxiliary directory, put all the object files into a library:
      ar rv libaux.a *.o

Now you've done all the preparation needed before you can compile an
LAPACK example program.

  * Let's use dgetrf_example.f90:
```
    cd examples/source
    nagfor -I../../interface_blocks dgetrf_example.f90 ../../auxiliary/libaux.a
      full_path_to/liblapack.a full_path_to/libblas.a -o dgetrf_example.exe
```
Here we assume that the LAPACK and BLAS routines are held in two
separate libraries. You may have one library containing all of them.

  * Run the program, using the approprate data file as input:
```
    ./dgetrf_example.exe < ../data/dgetrf_example.d
```
That's it. If you had no problems, then you should be able to compile and
run all the other example programs too.


# How to build using the GNUmakefile
There is a GNUmakefile which contains commands to compile and run all
the example programs using one of various compilers.

First of all, edit GNUmakefile and modify these two lines:

  * LIBLAPACK := ../lapack-3.8.0/$(compiler)/liblapack.a
  * LIBBLAS := ../lapack-3.8.0/$(compiler)/librefblas.a

to give the paths of your LAPACK and BLAS libraries (if you have just
one library containing both LAPACK and BLAS routines, you can set
them both to be the same library, or one of them to be blank).

Then a command such as:
```
    make compiler=nagfor
```
will compile all the necessary auxiliary (matrix printing) code into a
library, and then compile, link and run each individual LAPACK example program.
The results will be compared with a set of expected results which are located
in the examples/baseresults directory.

To compile a single example, a command such as:
```
    make compiler=gfortran dgetrf_example.r
```
should work.

# Test platforms
The GNUmakefile currently has knowledge of the following Fortran compilers:

  * NAG Fortran compiler, nagfor
  * GNU Fortran compiler, gfortran
  * Intel Fortran compiler, ifort
  * PGI Fortran compiler, pgf90
  * ARM Fortran compiler, armflang

The example programs have been shown to work with all these compilers
on various 64-bit GNU/Linux operating systems, and the first three also on
64-bit Microsoft Windows.

# Example program result differences
Small differences may show up between the "base" results and the results
you get when you run the example programs. These can be because
of small rounding differences either in the LAPACK library you're using,
or because different compilers have subtle differences in the way that
they print numbers (for example, in Fortran formatted output, some
compilers print tiny negative numbers which round to zero when formatted
as -0.0000 and other compilers print as 0.0000). Occasionally eigenvalues
and eigenvectors may be produced in a different order.
