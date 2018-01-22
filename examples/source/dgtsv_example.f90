    Program dgtsv_example

!     DGTSV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgtsv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: info, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: b(:), d(:), dl(:), du(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGTSV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (b(n), d(n), dl(n-1), du(n-1))

!     Read the tridiagonal matrix A and the right hand side B from
!     data file

      Read (nin, *) du(1:n-1)
      Read (nin, *) d(1:n)
      Read (nin, *) dl(1:n-1)
      Read (nin, *) b(1:n)

!     Solve the equations Ax = b for x

      Call dgtsv(n, 1, dl, d, du, b, n, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100) b(1:n)

      Else
        Write (nout, 110) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format ((1X,7F11.4))
110   Format (1X, A, I3, A, I3, A, A)
    End Program
