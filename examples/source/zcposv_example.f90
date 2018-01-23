    Program zcposv_example

!     ZCPOSV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zcposv
      Use lapack_precision, Only: sp, dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, iter, lda, ldb, ldx, n, r
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), work(:, :), x(:, :)
      Complex (Kind=sp), Allocatable :: swork(:)
      Real (Kind=dp), Allocatable :: rwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZCPOSV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, r
      lda = n
      ldb = n
      ldx = n
      Allocate (a(lda,n), b(ldb,r), work(n,r), x(ldx,r), swork(n*(n+ &
        r)), rwork(n))

!     Read A and B from data file

      Read (nin, *)(a(i,i:n), i=1, n)
      Read (nin, *)(b(i,1:r), i=1, n)

!     Solve the equations Ax = b for x
      Call zcposv('U', n, r, a, lda, b, ldb, x, ldx, work, swork, rwork, iter, &
        info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100)(x(i,1:r), i=1, n)

      Else
        Write (nout, 110) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format ((3X,4(' (',F7.4,',',F7.4,')',:)))
110   Format (1X, A, I3, A)
    End Program
