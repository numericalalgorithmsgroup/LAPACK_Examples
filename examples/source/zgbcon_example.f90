    Program zgbcon_example

!     ZGBCON Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgbcon, zgbtrf, zlangb
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: norm = '1'
!     .. Local Scalars ..
      Real (Kind=dp) :: anorm, rcond
      Integer :: i, info, j, k, kl, ku, ldab, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), work(:)
      Real (Kind=dp), Allocatable :: rwork(:)
      Integer, Allocatable :: ipiv(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZGBCON Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kl, ku
      ldab = 2*kl + ku + 1
      Allocate (ab(ldab,n), work(2*n), rwork(n), ipiv(n))

!     Read A from data file

      k = kl + ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, n)

      anorm = zlangb(norm, n, kl, ku, ab(kl+1,1), ldab, rwork)

!     Factorize A
      Call zgbtrf(n, n, kl, ku, ab, ldab, ipiv, info)

      Write (nout, *)
      If (info==0) Then

!       Estimate condition number
        Call zgbcon(norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond, work, &
          rwork, info)

        If (rcond>=epsilon(1.0E0_dp)) Then
          Write (nout, 100) 'Estimate of condition number =', 1.0E0_dp/rcond
        Else
          Write (nout, *) 'A is singular to working precision'
        End If
      Else
        Write (nout, *) 'The factor U is singular'
      End If

100   Format (1X, A, 1P, E10.2)
    End Program
