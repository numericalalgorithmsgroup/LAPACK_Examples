    Program zgesvdx_example

!     ZGESVDX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgesvdx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: vl, vu
      Integer :: i, il, info, iu, lda, ldu, ldvt, liwork, lrwork, lwork, m, n, &
        ns
      Character (1) :: range
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), u(:, :), vt(:, :), work(:)
      Real (Kind=dp), Allocatable :: rwork(:), s(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: nint
!     .. Executable Statements ..
      Write (nout, *) 'ZGESVDX Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldu = m
      ldvt = n
      lwork = 2*n**2 + 4*n
      lrwork = 2*n*2 + 34*n
      liwork = 12*n
      Allocate (a(lda,n), s(n), vt(ldvt,n), u(ldu,m), iwork(liwork), &
        work(lwork), rwork(lrwork))

!     Read the m by n matrix A from data file
      Read (nin, *)(a(i,1:n), i=1, m)

!     Read range for selected singular values
      Read (nin, *) range

      If (range=='I' .Or. range=='i') Then
        Read (nin, *) il, iu
      Else If (range=='V' .Or. range=='v') Then
        Read (nin, *) vl, vu
      End If

!     Compute the singular values and left and right singular vectors
!     of A.

      Call zgesvdx('V', 'V', range, m, n, a, lda, vl, vu, il, iu, ns, s, u, &
        ldu, vt, ldvt, work, lwork, rwork, iwork, info)

      If (info/=0) Then
        Write (nout, 100) 'Failure in ZGESVDX. INFO =', info
100     Format (1X, A, I4)
        Go To 120
      End If

!     Print the selected singular values of A

      Write (nout, *) 'Singular values of A:'
      Write (nout, 110) s(1:ns)
110   Format (1X, 4(3X,F11.4))

      Call compute_error_bounds(m, ns, s)

120   Continue

    Contains
      Subroutine compute_error_bounds(m, n, s)

!       Error estimates for singular values and vectors is computed
!       and printed here.

!       .. Use Statements ..
        Use lapack_interfaces, Only: ddisna
        Use lapack_precision, Only: dp
!       .. Implicit None Statement ..
        Implicit None
!       .. Scalar Arguments ..
        Integer, Intent (In) :: m, n
!       .. Array Arguments ..
        Real (Kind=dp), Intent (In) :: s(n)
!       .. Local Scalars ..
        Real (Kind=dp) :: eps, serrbd
        Integer :: i, info
!       .. Local Arrays ..
        Real (Kind=dp), Allocatable :: rcondu(:), rcondv(:), uerrbd(:), &
          verrbd(:)
!       .. Intrinsic Procedures ..
        Intrinsic :: epsilon
!       .. Executable Statements ..
        Allocate (rcondu(n), rcondv(n), uerrbd(n), verrbd(n))

!       Get the machine precision, EPS and compute the approximate
!       error bound for the computed singular values.  Note that for
!       the 2-norm, S(1) = norm(A)

        eps = epsilon(1.0E0_dp)
        serrbd = eps*s(1)

!       Call DDISNA to estimate reciprocal condition
!       numbers for the singular vectors

        Call ddisna('Left', m, n, s, rcondu, info)
        Call ddisna('Right', m, n, s, rcondv, info)

!       Compute the error estimates for the singular vectors

        Do i = 1, n
          uerrbd(i) = serrbd/rcondu(i)
          verrbd(i) = serrbd/rcondv(i)
        End Do

!       Print the approximate error bounds for the singular values
!       and vectors

        Write (nout, *)
        Write (nout, *) 'Error estimates (as multiples of machine precision):'
        Write (nout, *) '  for the singular values'
        Write (nout, 100) nint(serrbd/epsilon(1.0E0_dp))
        Write (nout, *)
        Write (nout, *) '  for the left singular vectors'
        Write (nout, 100) nint(uerrbd(1:n)/epsilon(1.0E0_dp))
        Write (nout, *)
        Write (nout, *) '  for the right singular vectors'
        Write (nout, 100) nint(verrbd(1:n)/epsilon(1.0E0_dp))

100     Format (4X, 6I11)

      End Subroutine

    End Program
