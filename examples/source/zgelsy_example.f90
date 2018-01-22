    Program zgelsy_example

!     ZGELSY Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgelsy
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, info, lda, lwork, m, n, rank
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:), work(:)
      Real (Kind=dp), Allocatable :: rwork(:)
      Integer, Allocatable :: jpvt(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZGELSY Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = nb*(n+1)
      Allocate (a(lda,n), b(m), work(lwork), rwork(2*n), jpvt(n))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *) b(1:m)

!     Initialize JPVT to be zero so that all columns are free

      jpvt(1:n) = 0

!     Choose RCOND to reflect the relative accuracy of the input data

      rcond = 0.01_dp

!     Solve the least squares problem min( norm2(b - Ax) ) for the x
!     of minimum norm.

      Call zgelsy(m, n, 1, a, lda, b, m, jpvt, rcond, rank, work, lwork, &
        rwork, info)

!     Print solution

      Write (nout, *) 'Least squares solution'
      Write (nout, 100) b(1:n)

!     Print the effective rank of A

      Write (nout, *)
      Write (nout, *) 'Tolerance used to estimate the rank of A'
      Write (nout, 110) rcond
      Write (nout, *) 'Estimated rank of A'
      Write (nout, 120) rank

100   Format (4(' (',F7.4,',',F7.4,')',:))
110   Format (1X, 1P, E10.2)
120   Format (1X, I6)
    End Program
