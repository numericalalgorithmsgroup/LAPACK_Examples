    Program dgejsv_example

!     DGEJSV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: ddisna, dgejsv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, serrbd
      Integer :: i, ifail, info, j, lda, ldu, ldv, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), rcondu(:), rcondv(:), s(:), &
        u(:, :), v(:, :), work(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max
!     .. Executable Statements ..
      Write (nout, *) 'DGEJSV Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldu = m
      ldv = n
      lwork = max(3*n+n*n+m, 3*n+n*n+n*nb, 7)
      Allocate (a(lda,n), rcondu(m), rcondv(m), s(n), u(ldu,n), v(ldv,n), &
        work(lwork), iwork(m+3*n))

!     Read the m by n matrix A from data file
      Read (nin, *)((a(i,j),j=1,n), i=1, m)

!     Compute the singular values and left and right singular vectors
!     of A (A = U*S*V^T, m.ge.n)
      Call dgejsv('E', 'U', 'V', 'R', 'N', 'N', m, n, a, lda, s, u, ldu, v, &
        ldv, work, lwork, iwork, info)

      If (info==0) Then

!       Compute the approximate error bound for the computed singular values
!       using the 2-norm, s(1) = norm(A), and machine precision, eps.
        eps = epsilon(1.0E0_dp)
        serrbd = eps*s(1)

!       Print solution
        If (abs(work(1)-work(2))<2.0_dp*eps) Then
!         No scaling required
          Write (nout, '(1X,A)') 'Singular values'
          Write (nout, 100)(s(j), j=1, n)
        Else
          Write (nout, '(/1X,A)') 'Scaled singular values'
          Write (nout, 100)(s(j), j=1, n)
          Write (nout, '(/1X,A)') 'For true singular values, multiply by a/b,'
          Write (nout, 130) ' where a = ', work(1), ' and b = ', work(2)
        End If

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', m, n, u, ldu, &
          'Left singular vectors', ifail)

        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, v, ldv, &
          'Right singular vectors', ifail)

!       Call DDISNA to estimate reciprocal condition numbers for
!       the singular vectors.

        Call ddisna('Left', m, n, s, rcondu, info)
        Call ddisna('Right', m, n, s, rcondv, info)

!       Print the approximate error bounds for the singular values
!       and vectors.
        Write (nout, *)
        Write (nout, '(/1X,A)') &
          'Estimate of the condition number of column equilibrated A'
        Write (nout, 110) work(3)
        Write (nout, '(/1X,A)') 'Error estimate for the singular values'
        Write (nout, 110) serrbd
        Write (nout, '(/1X,A)') 'Error estimates for left singular vectors'
        Write (nout, 110)(serrbd/rcondu(i), i=1, n)
        Write (nout, '(/1X,A)') 'Error estimates for right singular vectors'
        Write (nout, 110)(serrbd/rcondv(i), i=1, n)
      Else
        Write (nout, 120) 'Failure in DGEJSV. INFO =', info
      End If

100   Format (3X, 8F8.4)
110   Format (4X, 1P, 6E11.1)
120   Format (1X, A, I4)
130   Format (1X, 2(A,1P,E13.5))
    End Program
