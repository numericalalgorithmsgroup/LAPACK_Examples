    Program zgesdd_example

!     ZGESDD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_interfaces, Only: zgesdd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6, prerr = 0
!     .. Local Scalars ..
      Integer :: i, info, lda, ldu, ldvt, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), a_copy(:, :), b(:), u(:, :), &
        vt(:, :), work(:)
      Complex (Kind=dp) :: dummy(1, 1)
      Real (Kind=dp), Allocatable :: rwork(:), s(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGESDD Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldu = m
      ldvt = n
      Allocate (a(lda,n), a_copy(m,n), s(m), u(ldu,m), vt(ldvt,n), b(m), &
        rwork((5*m+7)*n), iwork(8*m))

!     Read the m by n matrix A from data file
      Read (nin, *)(a(i,1:n), i=1, m)

!     Read the right hand side of the linear system
      Read (nin, *) b(1:m)

      a_copy(1:m, 1:n) = a(1:m, 1:n)

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zgesdd('A', m, n, a, lda, s, u, ldu, vt, ldvt, dummy, lwork, rwork, &
        iwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((2*m+2)*m+2*n+nb*(m+n), nint(real(dummy(1,1))))
      Allocate (work(lwork))

!     Compute the singular values and left and right singular vectors
!     of A.

      Call zgesdd('A', m, n, a, lda, s, u, ldu, vt, ldvt, work, lwork, rwork, &
        iwork, info)

      If (info/=0) Then
        Write (nout, 100) 'Failure in ZGESDD. INFO =', info
100     Format (1X, A, I4)
        Go To 120
      End If

!     Print the significant singular values of A

      Write (nout, *) 'Singular values of A:'
      Write (nout, 110) s(1:min(m,n))
110   Format (1X, 4(3X,F11.4))

      If (prerr>0) Then
        Call compute_error_bounds(m, n, s)
      End If

      If (m<n) Then
!       Compute V*Inv(S)*U^T * b to get minimum norm solution.
        Call compute_minimum_norm(m, n, a_copy, m, u, ldu, vt, ldvt, s, b)
      End If

120   Continue

    Contains
      Subroutine compute_minimum_norm(m, n, a, lda, u, ldu, vt, ldvt, s, b)

!       .. Use Statements ..
        Use blas_interfaces, Only: dznrm2, zgemv
!       .. Implicit None Statement ..
        Implicit None
!       .. Scalar Arguments ..
        Integer, Intent (In) :: lda, ldu, ldvt, m, n
!       .. Array Arguments ..
        Complex (Kind=dp), Intent (In) :: a(lda, n), u(ldu, m), vt(ldvt, n)
        Complex (Kind=dp), Intent (Inout) :: b(m)
        Real (Kind=dp), Intent (In) :: s(m)
!       .. Local Scalars ..
        Complex (Kind=dp) :: alpha, beta
        Real (Kind=dp) :: norm
!       .. Local Arrays ..
        Complex (Kind=dp), Allocatable :: x(:), y(:)
!       .. Intrinsic Procedures ..
        Intrinsic :: allocated, cmplx
!       .. Executable Statements ..
        Allocate (x(n), y(m))

!       Compute V*Inv(S)*U^H * b to get least squares solution.

!       y = U^H b
        alpha = cmplx(1.0_dp, 0.0_dp, kind=dp)
        beta = cmplx(0.0_dp, 0.0_dp, kind=dp)
        Call zgemv('C', m, m, alpha, u, ldu, b, 1, beta, y, 1)

        y(1:m) = y(1:m)/s(1:m)

!       x = V y
        Call zgemv('C', m, n, alpha, vt, ldvt, y, 1, beta, x, 1)

        Write (nout, *)
        Write (nout, *) 'Minimum norm solution:'
        Write (nout, 100) x(1:n)

        norm = dznrm2(n, x, 1)

        Write (nout, *)
        Write (nout, *) 'Norm of Solution:'
        Write (nout, 110) norm

!       Find norm of residual ||b-Ax||, should be zero.
        alpha = cmplx(-1.0_dp, 0.0_dp, kind=dp)
        beta = cmplx(1._dp, 0.0_dp, kind=dp)
        Call zgemv('N', m, n, alpha, a, lda, x, 1, beta, b, 1)

        norm = dznrm2(m, b, 1)

        Write (nout, *)
        Write (nout, *) 'Norm of Residual:'
        Write (nout, 110) norm

        If (allocated(x)) Then
          Deallocate (x)
        End If
        If (allocated(y)) Then
          Deallocate (y)
        End If

100     Format (4X, '(', F8.4, ',', F8.4, ')')
110     Format (4X, F11.4)

      End Subroutine

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
        Real (Kind=dp), Intent (In) :: s(m)
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
        Write (nout, *) 'Error estimate for the singular values'
        Write (nout, 100) serrbd
        Write (nout, *)
        Write (nout, *) 'Error estimates for the left singular vectors'
        Write (nout, 100) uerrbd(1:n)
        Write (nout, *)
        Write (nout, *) 'Error estimates for the right singular vectors'
        Write (nout, 100) verrbd(1:n)

100     Format (4X, 1P, 6E11.1)

      End Subroutine

    End Program
