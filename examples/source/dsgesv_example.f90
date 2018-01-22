    Program dsgesv_example

!     DSGESV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dsgesv
      Use lapack_precision, Only: sp, dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, iter, lda, ldb, ldx, n, r
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), work(:), x(:, :)
      Real (Kind=sp), Allocatable :: swork(:)
      Integer, Allocatable :: ipiv(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSGESV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, r
      lda = n
      ldb = n
      ldx = n
      Allocate (a(lda,n), b(n,r), work(n*r), x(ldx,r), swork(n*(n+ &
        r)), ipiv(n))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:r), i=1, n)

!     Solve the equations Ax = b for x

      Call dsgesv(n, r, a, lda, ipiv, b, ldb, x, ldx, work, swork, iter, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100)(x(i,1:r), i=1, n)

!       Print pivot indices

        Write (nout, *)
        Write (nout, *) 'Pivot indices'
        Write (nout, 110) ipiv(1:n)

      Else
        Write (nout, 120) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format ((3X,7F11.4))
110   Format ((3X,7I11))
120   Format (1X, A, I3, A, I3, A, A)
    End Program
