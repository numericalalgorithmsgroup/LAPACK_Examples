    Program zhgeqz_example

!     ZHGEQZ Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_sort_realvec_rank, &
        nagf_file_print_matrix_complex_gen_comp, &
        nagf_sort_cmplxvec_rank_rearrange
      Use lapack_interfaces, Only: zgeqrf, zggbal, zgghd3, zhgeqz, zunmqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, ihi, ilo, info, irows, jwork, lda, ldb, ldq, ldz, &
        lwork, n, ni
      Character (1) :: compq, compz, job
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        e(:), q(:, :), tau(:), work(:), z(:, :)
      Real (Kind=dp), Allocatable :: emod(:), lscale(:), rscale(:), rwork(:)
      Integer, Allocatable :: irank(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, aimag, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZHGEQZ Example Program Results'
      Flush (nout)

!     Skip heading in data file

      Read (nin, *)
      Read (nin, *) n
      ldq = 1
      ldz = 1
      lda = n
      ldb = n
      lwork = 6*n
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), q(ldq,ldq), tau(n), &
        work(lwork), z(ldz,ldz), lscale(n), rscale(n), rwork(6*n))

!     READ matrix A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     READ matrix B from data file
      Read (nin, *)(b(i,1:n), i=1, n)

!     Balance matrix pair (A,B)
      job = 'B'
      Call zggbal(job, n, a, lda, b, ldb, ilo, ihi, lscale, rscale, rwork, &
        info)

!     Matrix A after balancing

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.4', 'Matrix A after balancing', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

!     Matrix B after balancing

      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, b, &
        ldb, 'Bracketed', 'F7.4', 'Matrix B after balancing', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

!     Reduce B to triangular form using QR
      irows = ihi + 1 - ilo

      Call zgeqrf(irows, irows, b(ilo,ilo), ldb, tau, work, lwork, info)

!     Apply the orthogonal transformation to A
      Call zunmqr('L', 'C', irows, irows, irows, b(ilo,ilo), ldb, tau, &
        a(ilo,ilo), lda, work, lwork, info)

!     Compute the generalized Hessenberg form of (A,B) -> (H,T)
      compq = 'N'
      compz = 'N'

      Call zgghd3(compq, compz, irows, 1, irows, a(ilo,ilo), lda, b(ilo,ilo), &
        ldb, q, ldq, z, ldz, work, lwork, info)

!     Matrix A (H) in generalized Hessenberg form
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.3', 'Matrix A in Hessenberg form', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

!     Matrix B (T) in generalized Hessenberg form
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, b, &
        ldb, 'Bracketed', 'F7.3', 'Matrix B is triangular', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

!     Routine ZHGEQZ
!     Workspace query: jwork = -1

      jwork = -1
      job = 'E'
      Call zhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alpha, beta, &
        q, ldq, z, ldz, work, jwork, rwork, info)
      Write (nout, *)
      Write (nout, 100) nint(real(work(1)))
      Write (nout, 110) lwork
      Write (nout, *)
      Write (nout, 120)
      Write (nout, *)
      Flush (nout)

!     Compute the generalized Schur form
!     if the workspace lwork is adequate

      If (nint(real(work(1)))<=lwork) Then

        Call zhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alpha, &
          beta, q, ldq, z, ldz, work, lwork, rwork, info)

!       Print the generalized eigenvalues in descending size order
!       Note: the actual values of beta are real and non-negative

!       Calculate the moduli of the finite eigenvalues.
        Allocate (e(n), emod(n), irank(n))
        ni = 0
        Do i = 1, n
          If (real(beta(i))/=0.0_dp) Then
            ni = ni + 1
            e(ni) = alpha(i)/beta(i)
            emod(ni) = abs(e(ni))
          Else
            Write (nout, 130) i
          End If
        End Do

!       Rearrange the finite eigenvalues in descending order of modulus.
        ifail = 0
        Call nagf_sort_realvec_rank(emod, 1, ni, 'Descending', irank, ifail)
        ifail = 0
        Call nagf_sort_cmplxvec_rank_rearrange(e, 1, ni, irank, ifail)

        Write (nout, 140)(i, '(', real(e(i)), ',', aimag(e(i)), ')', i=1, ni)
      Else
        Write (nout, 150)
      End If

100   Format (1X, 'Minimal required LWORK = ', I6)
110   Format (1X, 'Actual value of  LWORK = ', I6)
120   Format (1X, 'Generalized eigenvalues')
130   Format (1X, I4, 5X, 'Infinite eigenvalue')
140   Format (1X, I4, 5X, A, F7.3, A, F7.3, A)
150   Format (1X, 'Insufficient workspace allocated for call to ZHGEQZ')
    End Program
