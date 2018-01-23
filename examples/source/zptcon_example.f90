    Program zptcon_example

!     ZPTCON Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zlanht, zptcon, zpttrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, rcond
      Integer :: info, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: e(:)
      Real (Kind=dp), Allocatable :: d(:), rwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZPTCON Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (e(n-1), d(n), rwork(n))

!     Read the lower bidiagonal part of the tridiagonal matrix A from
!     data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Compute the 1-norm of A
      anorm = zlanht('1-norm', n, d, e)

!     Factorize the tridiagonal matrix A
      Call zpttrf(n, d, e, info)

      If (info==0) Then

!       Estimate the condition number of A
        Call zptcon(n, d, e, anorm, rcond, rwork, info)

!       Print the estimated condition number

        If (rcond>=epsilon(1.0E0_dp)) Then
          Write (nout, 100) 'Estimate of condition number = ', 1.0E0_dp/rcond
        Else
          Write (nout, 100) 'A is singular to working precision. RCOND = ', &
            rcond
        End If

      Else
        Write (nout, 110) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format (1X, A, 1P, E10.2)
110   Format (1X, A, I3, A)
    End Program
