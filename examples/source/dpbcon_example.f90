    Program dpbcon_example

!     DPBCON Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dlansb, dpbcon, dpbtrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, rcond
      Integer :: i, info, j, kd, ldab, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), work(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, min
!     .. Executable Statements ..
      Write (nout, *) 'DPBCON Example Program Results'
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

!     Compute norm of A

      anorm = dlansb('1-norm', uplo, n, kd, ab, ldab, work)

!     Factorize A
      Call dpbtrf(uplo, n, kd, ab, ldab, info)

      Write (nout, *)
      If (info==0) Then

!       Estimate condition number
        Call dpbcon(uplo, n, kd, ab, ldab, anorm, rcond, work, iwork, info)

        If (rcond>=epsilon(1.0E0_dp)) Then
          Write (nout, 100) 'Estimate of condition number =', 1.0_dp/rcond
        Else
          Write (nout, *) 'A is singular to working precision'
        End If
      Else
        Write (nout, *) 'A is not positive definite'
      End If

100   Format (1X, A, 1P, E10.2)
    End Program
