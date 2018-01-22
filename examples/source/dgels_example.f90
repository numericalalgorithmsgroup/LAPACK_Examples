    Program dgels_example

!     DGELS Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_interfaces, Only: dgels
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rnorm
      Integer :: i, info, lda, ldb, lwork, m, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:), work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGELS Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = n + nb*m
      Allocate (a(lda,n), b(m), work(lwork))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *) b(1:m)

!     Solve the least squares problem min( norm2(b - Ax) ) for x

      nrhs = 1
      ldb = m

      Call dgels('No transpose', m, n, nrhs, a, lda, b, ldb, work, lwork, &
        info)

!     Print solution

      Write (nout, *) 'Least squares solution'
      Write (nout, 100) b(1:n)

!     Compute and print estimate of the square root of the residual
!     sum of squares

      rnorm = dnrm2(m-n, b(n+1), 1)
      Write (nout, *)
      Write (nout, *) 'Square root of the residual sum of squares'
      Write (nout, 110) rnorm

100   Format (1X, 7F11.4)
110   Format (3X, 1P, E11.2)
    End Program
