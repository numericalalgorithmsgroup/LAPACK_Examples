    Program zgtsv_example

!     ZGTSV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgtsv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: info, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: b(:), d(:), dl(:), du(:)
!     .. Executable Statements ..
      Write (nout, *) 'ZGTSV Example Program Results'
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
      Call zgtsv(n, 1, dl, d, du, b, n, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100) b(1:n)

      Else
        Write (nout, 110) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format (4(' (',F8.4,',',F8.4,')',:))
110   Format (1X, A, I3, A, I3, A, A)
    End Program
