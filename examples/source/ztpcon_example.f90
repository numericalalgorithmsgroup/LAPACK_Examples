    Program ztpcon_example

!     ZTPCON Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: ztpcon
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: diag = 'N', norm = '1'
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, info, j, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), work(:)
      Real (Kind=dp), Allocatable :: rwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZTPCON Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap(n*(n+1)/2), work(2*n), rwork(n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

!     Estimate condition number
      Call ztpcon(norm, uplo, diag, n, ap, rcond, work, rwork, info)

      Write (nout, *)
      If (rcond>=epsilon(1.0E0_dp)) Then
        Write (nout, 100) 'Estimate of condition number =', 1.0E0_dp/rcond
      Else
        Write (nout, *) 'A is singular to working precision'
      End If

100   Format (1X, A, 1P, E10.2)
    End Program
