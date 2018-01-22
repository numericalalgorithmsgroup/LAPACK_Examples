    Program dsposv_example

!     DSPOSV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dsposv
      Use lapack_precision, Only: sp, dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, iter, lda, ldb, ldx, n, r
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), work(:, :), x(:, :)
      Real (Kind=sp), Allocatable :: swork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSPOSV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, r
      lda = n
      ldb = n
      ldx = n
      Allocate (a(lda,n), b(n,r), work(n,r), x(ldx,r), swork(n*(n+r)))

!     Read the upper triangular part of A from data file

      Read (nin, *)(a(i,i:n), i=1, n)

!     Read B from data file

      Read (nin, *)(b(i,1:r), i=1, n)

!     Solve the equations Ax = b for x
      Call dsposv('U', n, r, a, lda, b, ldb, x, ldx, work, swork, iter, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100)(x(i,1:r), i=1, n)
      Else
        Write (nout, 110) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format ((3X,7F11.4))
110   Format ((1X,A,I3,A))
    End Program
