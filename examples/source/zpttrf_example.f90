    Program zpttrf_example

!     ZPTTRF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: zpttrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: info, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: e(:)
      Real (Kind=dp), Allocatable :: d(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZPTTRF Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (e(n-1), d(n))

!     Read the lower bidiagonal part of the tridiagonal matrix A from
!     data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Factorize the tridiagonal matrix A
      Call zpttrf(n, d, e, info)

      If (info>0) Then
        Write (nout, 100) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

!     Print details of the factorization

      Write (nout, *) 'Details of factorization'
      Write (nout, *)
      Write (nout, *) ' The diagonal elements of D'
      Write (nout, 110) d(1:n)
      Write (nout, *)
      Write (nout, *) ' Subdiagonal elements of the Cholesky factor L'
      Write (nout, 110) e(1:n-1)

100   Format (1X, A, I3, A)
110   Format (1X, 8F9.4)
    End Program
