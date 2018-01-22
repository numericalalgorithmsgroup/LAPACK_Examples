    Program dsptrd_example

!     DSPTRD Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dsptrd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, info, j, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ap(:), d(:), e(:), tau(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs
!     .. Executable Statements ..
      Write (nout, *) 'DSPTRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap(n*(n+1)/2), d(n), e(n-1), tau(n-1))

!     Read A from data file and copy A into AW

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

!     Reduce A to tridiagonal form
      Call dsptrd(uplo, n, ap, d, e, tau, info)

      If (info==0) Then
!       Print the diagonal and off-diagonal of tridiagonal T.
!       The absolute value of E is printed since this can vary by a change of
!       sign (corresponding to multiplying through a column of Q by -1).

        Write (nout, *)
        Write (nout, *) &
          'Diagonal and off-diagonal elements of tridiagonal form'
        Write (nout, *)
        Write (nout, 100) 'i', 'D', 'E'
        Do i = 1, n - 1
          Write (nout, 110) i, d(i), abs(e(i))
        End Do
        Write (nout, 110) n, d(n)

      Else
        Write (nout, 120) info
      End If

100   Format (5X, A, 9X, A, 12X, A)
110   Format (1X, I5, 2(1X,F12.5))
120   Format (1X, '** DSPTRD retuned with INFO = ', I10)

    End Program
