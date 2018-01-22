    Program dhgeqz_example

!     DHGEQZ Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgeqrf, dggbal, dgghd3, dhgeqz, dormqr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, ihi, ilo, info, irows, jwork, lda, ldb, ldq, ldz, &
        lwork, n
      Character (1) :: compq, compz, job
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), alphai(:), alphar(:), b(:, :), &
        beta(:), lscale(:), q(:, :), rscale(:), tau(:), work(:), z(:, :)
!     .. Intrinsic Procedures ..
      Intrinsic :: nint
!     .. Executable Statements ..
      Write (nout, *) 'DHGEQZ Example Program Results'
      Flush (nout)

!     Skip heading in data file

      Read (nin, *)
      Read (nin, *) n
      ldq = 1
      ldz = 1
      lda = n
      ldb = n
      lwork = 6*n
      Allocate (alphai(n), alphar(n), beta(n), a(lda,n), lscale(n), &
        q(ldq,ldq), rscale(n), b(ldb,n), tau(n), work(lwork), z(ldz,ldz))

!     READ matrix A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     READ matrix B from data file
      Read (nin, *)(b(i,1:n), i=1, n)

!     Balance matrix pair (A,B)
      job = 'B'

      Call dggbal(job, n, a, lda, b, ldb, ilo, ihi, lscale, rscale, work, &
        info)

!     Matrix A after balancing

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Matrix A after balancing', ifail)

      Write (nout, *)
      Flush (nout)

!     Matrix B after balancing

      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, b, ldb, &
        'Matrix B after balancing', ifail)

      Write (nout, *)
      Flush (nout)

!     Reduce B to triangular form using QR
      irows = ihi + 1 - ilo

      Call dgeqrf(irows, irows, b(ilo,ilo), ldb, tau, work, lwork, info)

!     Apply the orthogonal transformation to matrix A
      Call dormqr('L', 'T', irows, irows, irows, b(ilo,ilo), ldb, tau, &
        a(ilo,ilo), lda, work, lwork, info)

!     Compute the generalized Hessenberg form of (A,B) -> (H,T)
      compq = 'N'
      compz = 'N'

      Call dgghd3(compq, compz, irows, 1, irows, a(ilo,ilo), lda, b(ilo,ilo), &
        ldb, q, ldq, z, ldz, work, lwork, info)

!     Matrix A (H) in generalized Hessenberg form.

      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Matrix A in Hessenberg form', ifail)

      Write (nout, *)
      Flush (nout)

!     Matrix B (T) in generalized Hessenberg form.

      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, b, ldb, &
        'Matrix B is triangular', ifail)

!     Routine DHGEQZ
!     Workspace query: jwork = -1

      jwork = -1
      job = 'E'

      Call dhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alphar, &
        alphai, beta, q, ldq, z, ldz, work, jwork, info)

      Write (nout, *)
      Write (nout, 100) nint(work(1))
      Write (nout, 110) lwork
      Write (nout, *)

!     Compute the generalized Schur form
!     if the workspace lwork is adequate

      If (nint(work(1))<=lwork) Then

        Call dhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, alphar, &
          alphai, beta, q, ldq, z, ldz, work, lwork, info)

!       Print the generalized eigenvalues

        Write (nout, 120)

        Do i = 1, n
          If (beta(i)/=0.0E0_dp) Then
            Write (nout, 130) i, '(', alphar(i)/beta(i), ',', &
              alphai(i)/beta(i), ')'
          Else
            Write (nout, 150) i
          End If
        End Do
      Else
        Write (nout, 140)
      End If

100   Format (1X, 'Minimal required LWORK = ', I6)
110   Format (1X, 'Actual value of  LWORK = ', I6)
120   Format (1X, 'Generalized eigenvalues')
130   Format (1X, I4, 5X, A, F7.3, A, F7.3, A)
140   Format (1X, 'Insufficient workspace allocated for call to DHGEQZ')
150   Format (1X, I4, 'Eigenvalue is infinite')
    End Program
