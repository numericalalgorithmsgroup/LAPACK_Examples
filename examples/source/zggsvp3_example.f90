    Program zggsvp3_example

!     ZGGSVP3 Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zggsvp3, zlange, ztgsja
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
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), q(:, :), tau(:), &
        u(:, :), v(:, :), work(:)
      Complex (Kind=dp) :: wdum(1)
      Real (Kind=dp), Allocatable :: alpha(:), beta(:), rwork(:)
      Integer, Allocatable :: iwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGGSVP3 Example Program Results'
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
        u(ldu,m), v(ldv,p), rwork(2*n), iwork(n))

!     Perform workspace query to get optimal size of work
      lwork = -1
      Call zggsvp3('U', 'V', 'Q', m, p, n, a, lda, b, ldb, tola, tolb, k, l, &
        u, ldu, v, ldv, q, ldq, iwork, rwork, tau, wdum, lwork, info)
      lwork = nint(real(wdum(1)))
      Allocate (work(lwork))

!     Read the m by n matrix A and p by n matrix B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:n), i=1, p)

!     Compute tola and tolb as
!         tola = max(m,n)*norm(A)*macheps
!         tolb = max(p,n)*norm(B)*macheps

      eps = epsilon(1.0E0_dp)
      tola = real(max(m,n), kind=dp)*zlange('One-norm', m, n, a, lda, rwork)* &
        eps
      tolb = real(max(p,n), kind=dp)*zlange('One-norm', p, n, b, ldb, rwork)* &
        eps

!     Compute the factorization of (A, B)
!         (A = U*S*(Q**H), B = V*T*(Q**H))

      Call zggsvp3('U', 'V', 'Q', m, p, n, a, lda, b, ldb, tola, tolb, k, l, &
        u, ldu, v, ldv, q, ldq, iwork, rwork, tau, work, lwork, info)

!     Compute the generalized singular value decomposition of (A, B)
!         (A = U*D1*(0 R)*(Q**H), B = V*D2*(0 R)*(Q**H))

      Deallocate (work)
      Allocate (work(2*n))

      Call ztgsja('U', 'V', 'Q', m, p, n, k, l, a, lda, b, ldb, tola, tolb, &
        alpha, beta, u, ldu, v, ldv, q, ldq, work, ncycle, info)

!     Print solution

      irank = k + l
      Write (nout, *) 'Number of infinite generalized singular values (k)'
      Write (nout, 100) k
      Write (nout, *) 'Number of finite generalized singular values (l)'
      Write (nout, 100) l
      Write (nout, *) 'Effective Numerical rank of (A; B) (k+l)'
      Write (nout, 100) irank
      Write (nout, *)
      Write (nout, *) 'Finite generalized singular values'
      Write (nout, 110)(alpha(j)/beta(j), j=k+1, irank)
      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, m, u, &
        ldu, 'Bracketed', '1P,E12.4', 'Unitary matrix U', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', p, p, v, &
        ldv, 'Bracketed', '1P,E12.4', 'Unitary matrix V', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, q, &
        ldq, 'Bracketed', '1P,E12.4', 'Unitary matrix Q', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

      Call nagf_file_print_matrix_complex_gen_comp('Upper triangular', &
        'Non-unit', irank, irank, a(1,n-irank+1), lda, 'Bracketed', &
        '1P,E12.4', 'Nonsingular upper triangular matrix R', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Write (nout, *) 'Number of cycles of the Kogbetliantz method'
      Write (nout, 100) ncycle

100   Format (1X, I5)
110   Format (3X, 8(1P,E12.4))
    End Program
