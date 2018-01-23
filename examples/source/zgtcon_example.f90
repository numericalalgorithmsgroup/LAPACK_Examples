    Program zgtcon_example

!     ZGTCON Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgtcon, zgttrf, zlangt
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, rcond
      Integer :: info, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: d(:), dl(:), du(:), du2(:), work(:)
      Integer, Allocatable :: ipiv(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZGTCON Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (d(n), dl(n-1), du(n-1), du2(n-2), work(2*n), ipiv(n))

!     Read the tridiagonal matrix A from data file

      Read (nin, *) du(1:n-1)
      Read (nin, *) d(1:n)
      Read (nin, *) dl(1:n-1)

!     Compute the 1-norm of A
      anorm = zlangt('1-norm', n, dl, d, du)

!     Factorize the tridiagonal matrix A
      Call zgttrf(n, dl, d, du, du2, ipiv, info)

      If (info==0) Then

!       Estimate the condition number of A
        Call zgtcon('1-norm', n, dl, d, du, du2, ipiv, anorm, rcond, work, &
          info)

!       Print the estimated condition number

        If (rcond>=epsilon(1.0E0_dp)) Then
          Write (nout, 100) 'Estimate of condition number = ', 1.0E0_dp/rcond
        Else
          Write (nout, 100) 'A is singular to working precision. RCOND = ', &
            rcond
        End If

      Else
        Write (nout, 110) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format (1X, A, 1P, E10.2)
110   Format (1X, A, I3, A, I3, A, A)
    End Program
