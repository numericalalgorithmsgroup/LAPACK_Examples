    Program dggglm_example

!     DGGGLM Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_interfaces, Only: dggglm
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rnorm
      Integer :: i, info, lda, ldb, lwork, m, n, p
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), d(:), work(:), x(:), &
        y(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGGGLM Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, p
      lda = m
      ldb = m
      lwork = n + m + nb*(m+p)
      Allocate (a(lda,n), b(ldb,p), d(m), work(lwork), x(n), y(p))

!     Read A, B and D from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:p), i=1, m)
      Read (nin, *) d(1:m)

!     Solve the weighted least squares problem

!     minimize ||inv(B)*(d - A*x)|| (in the 2-norm)

      Call dggglm(m, n, p, a, lda, b, ldb, d, x, y, work, lwork, info)

!     Print least squares solution, x

      Write (nout, *) 'Weighted least squares solution'
      Write (nout, 100) x(1:n)

!     Print residual vector y = inv(B)*(d - A*x)

      Write (nout, *)
      Write (nout, *) 'Residual vector'
      Write (nout, 110) y(1:p)

!     Compute and print the square root of the residual sum of
!     squares

      rnorm = dnrm2(p, y, 1)

      Write (nout, *)
      Write (nout, *) 'Square root of the residual sum of squares'
      Write (nout, 110) rnorm

100   Format (1X, 7F11.4)
110   Format (3X, 1P, 7E11.2)
    End Program
