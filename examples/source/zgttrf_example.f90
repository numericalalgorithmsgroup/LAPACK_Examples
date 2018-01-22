    Program zgttrf_example

!     ZGTTRF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgttrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: info, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: d(:), dl(:), du(:), du2(:)
      Integer, Allocatable :: ipiv(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZGTTRF Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (d(n), dl(n-1), du(n-1), du2(n-2), ipiv(n))

!     Read the tridiagonal matrix A from data file

      Read (nin, *) du(1:n-1)
      Read (nin, *) d(1:n)
      Read (nin, *) dl(1:n-1)

!     Factorize the tridiagonal matrix A
      Call zgttrf(n, dl, d, du, du2, ipiv, info)

      If (info>0) Then
        Write (nout, 100) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

!     Print details of the factorization

      Write (nout, *) 'Details of factorization'
      Write (nout, *)
      Write (nout, *) ' Second superdiagonal of U'
      Write (nout, 110) du2(1:n-2)
      Write (nout, *)
      Write (nout, *) ' First superdiagonal of U'
      Write (nout, 110) du(1:n-1)
      Write (nout, *)
      Write (nout, *) ' Main diagonal of U'
      Write (nout, 110) d(1:n)
      Write (nout, *)
      Write (nout, *) ' Multipliers'
      Write (nout, 110) dl(1:n-1)
      Write (nout, *)
      Write (nout, *) ' Vector of interchanges'
      Write (nout, 120) ipiv(1:n)

100   Format (1X, A, I3, A, I3, A, A)
110   Format (4(' (',F8.4,',',F8.4,')',:))
120   Format (1X, 5I7)
    End Program
