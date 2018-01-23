    Program zgesvj_example

!     ZGESVJ Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: ddisna, zgesvj
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, serrbd
      Integer :: i, ifail, info, j, lda, ldv, lrwork, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), cwork(:), v(:, :)
      Real (Kind=dp), Allocatable :: rcondu(:), rcondv(:), rwork(:), sva(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, max, nint
!     .. Executable Statements ..
      Write (nout, *) 'ZGESVJ Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldv = n
      lwork = n + m
      lrwork = max(6, n)
      Allocate (a(lda,n), rcondu(m), rcondv(m), sva(n), v(ldv,n), &
        cwork(lwork), rwork(lrwork))

!     Read the m by n matrix A from data file

      Read (nin, *)((a(i,j),j=1,n), i=1, m)

!     Compute the singular values and left and right singular vectors
!     of A (A = U*S*V, m.ge.n)

      Call zgesvj('G', 'U', 'V', m, n, a, lda, sva, 0, v, ldv, cwork, lwork, &
        rwork, lrwork, info)
      If (info==0) Then

!       Compute the approximate error bound for the computed singular values
!       using the 2-norm, s(1) = norm(A), and machine precision, eps.
        eps = epsilon(1.0E0_dp)
        serrbd = eps*sva(1)

!       Print solution
        Write (nout, *) 'Singular values'
        Write (nout, 100)(sva(j), j=1, n)

        If (abs(rwork(1)-1.0_dp)>eps) Then
          Write (nout, 130) 'Values need scaling by factor = ', rwork(1)
        End If
        Write (nout, *)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', m, n, a, lda, &
          'Left singular vectors', ifail)

        Write (nout, *)
        Flush (nout)

        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', n, n, v, ldv, &
          'Right singular vectors', ifail)

!       Call DDISNA to estimate reciprocal condition
!       numbers for the singular vectors
        Call ddisna('Left', m, n, sva, rcondu, info)
        Call ddisna('Right', m, n, sva, rcondv, info)

!       Print the approximate error bounds for the singular values
!       and vectors
        Write (nout, '(/1X,A)') &
          'Error estimates (as multiples of machine precision):'
        Write (nout, '(/1X,A)') '  for the singular values'
        Write (nout, 110) nint(serrbd/epsilon(1.0E0_dp))
        Write (nout, '(/1X,A)') '  for left singular vectors'
        Write (nout, 110)(nint(serrbd/rcondu(i)/epsilon(1.0E0_dp)), i=1, n)
        Write (nout, '(/1X,A)') '  for right singular vectors'
        Write (nout, 110)(nint(serrbd/rcondv(i)/epsilon(1.0E0_dp)), i=1, n)
      Else
        Write (nout, 120) 'Failure in ZGESVJ. INFO =', info
      End If

100   Format (3X, (8F8.4))
110   Format (4X, 6I4)
120   Format (1X, A, I4)
130   Format (/, 1X, A, 1P, E13.5)
    End Program
