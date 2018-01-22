    Program zgglse_example

!     ZGGLSE Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_interfaces, Only: zgglse
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rnorm
      Integer :: i, info, lda, ldb, lwork, m, n, p
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), c(:), d(:), work(:), &
        x(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZGGLSE Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, p
      lda = m
      ldb = p
      lwork = p + n + nb*(m+n)
      Allocate (a(lda,n), b(ldb,n), c(m), d(p), work(lwork), x(n))

!     Read A, B, C and D from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:n), i=1, p)
      Read (nin, *) c(1:m)
      Read (nin, *) d(1:p)

!     Solve the equality-constrained least squares problem

!     minimize ||c - A*x|| (in the 2-norm) subject to B*x = D

      Call zgglse(m, n, p, a, lda, b, ldb, c, d, x, work, lwork, info)

!     Print least squares solution

      Write (nout, *) 'Constrained least squares solution'
      Write (nout, 100) x(1:n)

!     Compute the square root of the residual sum of squares

      rnorm = dznrm2(m-n+p, c(n-p+1), 1)
      Write (nout, *)
      Write (nout, *) 'Square root of the residual sum of squares'
      Write (nout, 110) rnorm

100   Format (4(' (',F7.4,',',F7.4,')',:))
110   Format (1X, 1P, E10.2)
    End Program
