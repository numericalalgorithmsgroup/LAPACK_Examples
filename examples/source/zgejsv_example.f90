    Program zgejsv_example

!     ZGEJSV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: ddisna, zgejsv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, serrbd
      Integer :: i, ifail, info, j, lda, ldu, ldv, lrwork, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), cwork(:), u(:, :), v(:, :)
      Real (Kind=dp), Allocatable :: rcondu(:), rcondv(:), rwork(:), sva(:)
      Integer, Allocatable :: iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, epsilon, nint
!     .. Executable Statements ..
      Write (nout, *) 'ZGEJSV Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldu = m
      ldv = n
      lwork = 5*n + n*n + 1000
      lrwork = n + 2*m + 7
      Allocate (a(lda,n), rcondu(m), rcondv(m), sva(n), u(ldu,n), v(ldv,n), &
        cwork(lwork), rwork(lrwork), iwork(m+3*n+3))

!     Read the m by n matrix A from data file
      Read (nin, *)((a(i,j),j=1,n), i=1, m)

!     Compute the singular values and left and right singular vectors
!     of A (A = U*S*V^T, m.ge.n)
      Call zgejsv('E', 'U', 'V', 'R', 'N', 'N', m, n, a, lda, sva, u, ldu, v, &
        ldv, cwork, lwork, rwork, lrwork, iwork, info)

      If (info==0) Then

!       Compute the approximate error bound for the computed singular values
!       using the 2-norm, s(1) = norm(A), and machine precision, eps.
        eps = epsilon(1.0E0_dp)
        serrbd = eps*sva(1)

!       Print solution
        If (abs(rwork(1)-rwork(2))<2.0_dp*eps) Then
!         No scaling required
          Write (nout, '(1X,A)') 'Singular values'
          Write (nout, 100)(sva(j), j=1, n)
        Else
          Write (nout, '(/1X,A)') 'Scaled singular values'
          Write (nout, 100)(sva(j), j=1, n)
          Write (nout, '(/1X,A)') 'For true singular values, multiply by a/b,'
          Write (nout, 140) ' where a = ', rwork(1), ' and b = ', rwork(2)
        End If

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', m, n, u, ldu, &
          'Left singular vectors', ifail)

        Write (nout, *)
        Flush (nout)
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', n, n, v, ldv, &
          'Right singular vectors', ifail)

!       Call DDISNA to estimate reciprocal condition numbers for
!       the singular vectors.

        Call ddisna('Left', m, n, sva, rcondu, info)
        Call ddisna('Right', m, n, sva, rcondv, info)

!       Print the approximate error bounds for the singular values
!       and vectors.
        Write (nout, *)
        Write (nout, '(/1X,A)') &
          'Estimate of the condition number of column equilibrated A'
        Write (nout, 110) rwork(3)
        Write (nout, '(/1X,A)') &
          'Error estimates (as multiples of machine precision):'
        Write (nout, '(/1X,A)') '  for the singular values'
        Write (nout, 120) nint(serrbd/epsilon(1.0E0_dp))
        Write (nout, '(/1X,A)') '  for left singular vectors'
        Write (nout, 120)(nint(serrbd/rcondu(i)/epsilon(1.0E0_dp)), i=1, n)
        Write (nout, '(/1X,A)') '  for right singular vectors'
        Write (nout, 120)(nint(serrbd/rcondv(i)/epsilon(1.0E0_dp)), i=1, n)
      Else
        Write (nout, 130) 'Failure in ZGEJSV. INFO =', info
      End If

100   Format (3X, 8F8.4)
110   Format (4X, 1P, E11.2)
120   Format (4X, 6I4)
130   Format (1X, A, I4)
140   Format (1X, 2(A,1P,E13.5))
    End Program
