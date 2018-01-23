    Program dgbcon_example

!     DGBCON Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: dgbcon, dgbtrf, dlangb
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
      Real (Kind=dp), Allocatable :: ab(:, :), work(:)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, min
!     .. Executable Statements ..
      Write (nout, *) 'DGBCON Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kl, ku
      ldab = 2*kl + ku + 1
      Allocate (ab(ldab,n), work(3*n), ipiv(n), iwork(n))

!     Read A from data file

      k = kl + ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, n)

!     Compute norm of A
      anorm = dlangb(norm, n, kl, ku, ab(kl+1,1), ldab, work)

!     Factorize A

      Call dgbtrf(n, n, kl, ku, ab, ldab, ipiv, info)

      Write (nout, *)
      If (info==0) Then

!       Estimate condition number

        Call dgbcon(norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond, work, &
          iwork, info)

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
