    Program dtbcon_example

!     DTBCON Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: dtbcon
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: diag = 'N', norm = '1'
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, info, j, kd, ldab, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), work(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, min
!     .. Executable Statements ..
      Write (nout, *) 'DTBCON Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      Allocate (ab(ldab,n), work(3*n), iwork(n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Do i = 1, n
          Read (nin, *)(ab(kd+1+i-j,j), j=i, min(n,i+kd))
        End Do
      Else If (uplo=='L') Then
        Do i = 1, n
          Read (nin, *)(ab(1+i-j,j), j=max(1,i-kd), i)
        End Do
      End If

!     Estimate condition number
      Call dtbcon(norm, uplo, diag, n, kd, ab, ldab, rcond, work, iwork, info)

      Write (nout, *)
      If (rcond>=epsilon(1.0E0_dp)) Then
        Write (nout, 100) 'Estimate of condition number =', 1.0E0_dp/rcond
      Else
        Write (nout, *) 'A is singular to working precision'
      End If

100   Format (1X, A, 1P, E10.2)
    End Program
