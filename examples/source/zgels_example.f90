    Program zgels_example

!     ZGELS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_interfaces, Only: zgels
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rnorm
      Integer :: i, info, lda, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:), work(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZGELS Example Program Results'
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
      Call zgels('No transpose', m, n, 1, a, lda, b, m, work, lwork, info)

!     Print solution

      Write (nout, *) 'Least squares solution'
      Write (nout, 100) b(1:n)

!     Compute and print estimate of the square root of the residual
!     sum of squares

      rnorm = dznrm2(m-n, b(n+1), 1)
      Write (nout, *)
      Write (nout, *) 'Square root of the residual sum of squares'
      Write (nout, 110) rnorm

100   Format (4(' (',F7.4,',',F7.4,')',:))
110   Format (1X, 1P, E10.2)
    End Program
