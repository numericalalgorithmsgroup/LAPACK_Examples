    Program dsycon_example

!     DSYCON Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dlansy, dsycon, dsytrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, rcond
      Integer :: i, info, lda, lwork, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), work(:)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'DSYCON Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      lwork = 64*n
      Allocate (a(lda,n), work(lwork), ipiv(n), iwork(n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If

!     Compute norm of A
      anorm = dlansy('1-norm', uplo, n, a, lda, work)

!     Factorize A
      Call dsytrf(uplo, n, a, lda, ipiv, work, lwork, info)

      Write (nout, *)
      If (info==0) Then

!       Estimate condition number
        Call dsycon(uplo, n, a, lda, ipiv, anorm, rcond, work, iwork, info)

        If (rcond>=epsilon(1.0E0_dp)) Then
          Write (nout, 100) 'Estimate of condition number =', 1.0_dp/rcond
        Else
          Write (nout, *) 'A is singular to working precision'
        End If
      Else
        Write (nout, *) 'The factor D is singular'
      End If

100   Format (1X, A, 1P, E10.2)
    End Program
