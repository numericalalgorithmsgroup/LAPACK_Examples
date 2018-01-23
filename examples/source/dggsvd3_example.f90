    Program dggsvd3_example

!     DGGSVD3 Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen_comp
      Use lapack_interfaces, Only: dggsvd3, dtrcon
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, rcond, serrbd
      Integer :: i, ifail, info, irank, j, k, l, lda, ldb, ldq, ldu, ldv, &
        lwork, m, n, p
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        q(:, :), u(:, :), v(:, :), work(:)
      Real (Kind=dp) :: wdum(1)
      Integer, Allocatable :: iwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, nint
!     .. Executable Statements ..
      Write (nout, *) 'DGGSVD3 Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, p
      lda = m
      ldb = p
      ldq = n
      ldu = m
      ldv = p
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), q(ldq,n), u(ldu,m), &
        v(ldv,p), iwork(n))

!     Perform workspace query to get optimal size of work
      lwork = -1
      Call dggsvd3('U', 'V', 'Q', m, n, p, k, l, a, lda, b, ldb, alpha, beta, &
        u, ldu, v, ldv, q, ldq, wdum, lwork, iwork, info)
      lwork = nint(wdum(1))
      Allocate (work(lwork))

!     Read the m by n matrix A and p by n matrix B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:n), i=1, p)

!     Compute the generalized singular value decomposition of (A, B)
!     (A = U*D1*(0 R)*(Q**T), B = V*D2*(0 R)*(Q**T), m>=n)
      Call dggsvd3('U', 'V', 'Q', m, n, p, k, l, a, lda, b, ldb, alpha, beta, &
        u, ldu, v, ldv, q, ldq, work, lwork, iwork, info)

      If (info==0) Then

!       Print solution

        irank = k + l
        Write (nout, *) 'Number of infinite generalized singular values (K)'
        Write (nout, 100) k
        Write (nout, *) 'Number of finite generalized singular values (L)'
        Write (nout, 100) l
        Write (nout, *) 'Numerical rank of (A**T B**T)**T (K+L)'
        Write (nout, 100) irank
        Write (nout, *)
        Write (nout, *) 'Finite generalized singular values'
        Write (nout, 110)(alpha(j)/beta(j), j=k+1, irank)

        Write (nout, *)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen_comp('General', ' ', m, m, u, &
          ldu, '1P,E12.4', 'Orthogonal matrix U', 'Integer', rlabs, 'Integer', &
          clabs, 80, 0, ifail)

        Write (nout, *)
        Flush (nout)

        Call nagf_file_print_matrix_real_gen_comp('General', ' ', p, p, v, &
          ldv, '1P,E12.4', 'Orthogonal matrix V', 'Integer', rlabs, 'Integer', &
          clabs, 80, 0, ifail)

        Write (nout, *)
        Flush (nout)

        Call nagf_file_print_matrix_real_gen_comp('General', ' ', n, n, q, &
          ldq, '1P,E12.4', 'Orthogonal matrix Q', 'Integer', rlabs, 'Integer', &
          clabs, 80, 0, ifail)

        Write (nout, *)
        Flush (nout)

        Call nagf_file_print_matrix_real_gen_comp('Upper triangular', &
          'Non-unit', irank, irank, a(1,n-irank+1), lda, '1P,E12.4', &
          'Nonsingular upper triangular matrix R', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

!       Call DTRCON to estimate the reciprocal condition
!       number of R

        Call dtrcon('Infinity-norm', 'Upper', 'Non-unit', irank, &
          a(1,n-irank+1), lda, rcond, work, iwork, info)

        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal condition number for R'
        Write (nout, 120) rcond
        Write (nout, *)

!       So long as irank = n, get the machine precision, eps, and
!       compute the approximate error bound for the computed
!       generalized singular values

        If (irank==n) Then
          eps = epsilon(1.0E0_dp)
          serrbd = eps/rcond
          Write (nout, *) 'Error estimate for the generalized singular values'
          Write (nout, 120) serrbd
        Else
          Write (nout, *) '(A**T B**T)**T is not of full rank'
        End If
      Else
        Write (nout, 130) 'Failure in DGGSVD3. INFO =', info
      End If

100   Format (1X, I5)
110   Format (3X, 8(1P,E12.4))
120   Format (1X, 1P, E11.1)
130   Format (1X, A, I4)
    End Program
