    Program zgecon_example

!     ZGECON Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgecon, zgetrf, zlange
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: norm = '1'
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, rcond
      Integer :: i, info, lda, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), work(:)
      Real (Kind=dp), Allocatable :: rwork(:)
      Integer, Allocatable :: ipiv(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZGECON Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      Allocate (a(lda,n), work(2*n), rwork(2*n), ipiv(n))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Compute norm of A
      anorm = zlange(norm, n, n, a, lda, rwork)

!     Factorize A
      Call zgetrf(n, n, a, lda, ipiv, info)

      Write (nout, *)
      If (info==0) Then

!       Estimate condition number

        Call zgecon(norm, n, a, lda, anorm, rcond, work, rwork, info)

        If (rcond>=epsilon(1.0E0_dp)) Then
          Write (nout, 100) 'Estimate of condition number =', 1.0_dp/rcond
        Else
          Write (nout, *) 'A is singular to working precision'
        End If
      Else
        Write (nout, *) 'The factor U is singular'
      End If

100   Format (1X, A, 1P, E10.2)
    End Program
