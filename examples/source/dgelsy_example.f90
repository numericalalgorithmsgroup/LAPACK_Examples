    Program dgelsy_example

!     DGELSY Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgelsy
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, info, lda, lwork, m, n, rank
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:), work(:)
      Integer, Allocatable :: jpvt(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGELSY Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = 3*n + nb*(n+1)
      Allocate (a(lda,n), b(m), work(lwork), jpvt(n))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *) b(1:m)

!     Initialize JPVT to be zero so that all columns are free

      jpvt(1:n) = 0

!     Choose RCOND to reflect the relative accuracy of the input data

      rcond = 0.01_dp

!     Solve the least squares problem min( norm2(b - Ax) ) for the x
!     of minimum norm.

      Call dgelsy(m, n, 1, a, lda, b, m, jpvt, rcond, rank, work, lwork, info)

!     Print solution

      Write (nout, *) 'Least squares solution'
      Write (nout, 100) b(1:n)

!     Print the effective rank of A

      Write (nout, *)
      Write (nout, *) 'Tolerance used to estimate the rank of A'
      Write (nout, 110) rcond
      Write (nout, *) 'Estimated rank of A'
      Write (nout, 120) rank

100   Format (1X, 7F11.4)
110   Format (3X, 1P, E11.2)
120   Format (1X, I6)
    End Program
