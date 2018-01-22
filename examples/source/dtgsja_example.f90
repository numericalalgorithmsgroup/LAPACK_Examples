    Program dtgsja_example

!     DTGSJA Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen_comp
      Use lapack_interfaces, Only: dggsvp3, dlange, dtgsja
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: eps, tola, tolb
      Integer :: i, ifail, info, irank, j, k, l, lda, ldb, ldq, ldu, ldv, &
        lwork, m, n, ncycle, p
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        q(:, :), tau(:), u(:, :), v(:, :), work(:)
      Real (Kind=dp) :: wdum(1)
      Integer, Allocatable :: iwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'DTGSJA Example Program Results'
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
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), q(ldq,n), tau(n), &
        u(ldu,m), v(ldv,p), iwork(n))

!     Determine maximum workspace needed by dggsvp3 and dtgsja
      Call dggsvp3('U', 'V', 'Q', m, p, n, a, lda, b, ldb, tola, tolb, k, l, &
        u, ldu, v, ldv, q, ldq, iwork, tau, wdum, -1, info)
      lwork = max(nint(wdum(1)), 2*n)
      Allocate (work(lwork))

!     Read the m by n matrix A and p by n matrix B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:n), i=1, p)

!     Compute tola and tolb as
!         tola = max(m,n)*norm(A)*macheps
!         tolb = max(p,n)*norm(B)*macheps

      eps = epsilon(1.0E0_dp)
      tola = real(max(m,n), kind=dp)*dlange('One-norm', m, n, a, lda, work)* &
        eps
      tolb = real(max(p,n), kind=dp)*dlange('One-norm', p, n, b, ldb, work)* &
        eps

!     Compute the factorization of (A, B)
!         (A = U1*S*(Q1**T), B = V1*T*(Q1**T))
      Call dggsvp3('U', 'V', 'Q', m, p, n, a, lda, b, ldb, tola, tolb, k, l, &
        u, ldu, v, ldv, q, ldq, iwork, tau, work, lwork, info)

!     Compute the generalized singular value decomposition of (A, B)
!         (A = U*D1*(0 R)*(Q**T), B = V*D2*(0 R)*(Q**T))
      Call dtgsja('U', 'V', 'Q', m, p, n, k, l, a, lda, b, ldb, tola, tolb, &
        alpha, beta, u, ldu, v, ldv, q, ldq, work, ncycle, info)

      If (info==0) Then

!       Print solution

        irank = k + l
        Write (nout, *) 'Number of infinite generalized singular values (K)'
        Write (nout, 100) k
        Write (nout, *) 'Number of finite generalized singular values (L)'
        Write (nout, 100) l
        Write (nout, *) ' Effective Numerical rank of (A**T B**T)**T (K+L)'
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

        Write (nout, *)
        Write (nout, *) 'Number of cycles of the Kogbetliantz method'
        Write (nout, 100) ncycle
      Else
        Write (nout, 120) 'Failure in DTGSJA. INFO =', info
      End If

100   Format (1X, I5)
110   Format (3X, 8(1P,E12.4))
120   Format (1X, A, I4)
    End Program
