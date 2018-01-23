!   DGEES Example Program Text
!   Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!   For licence see
!     https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

    Module dgees_example_mod

!     DGEES Example Program Module:
!            Parameters and User-defined Routines

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Accessibility Statements ..
      Private
      Public :: select
!     .. Parameters ..
      Integer, Parameter, Public :: nb = 64, nin = 5, nout = 6
    Contains
      Function select(wr, wi)

!       Logical function select for use with DGEES (DGEES)
!       Returns the value .TRUE. if the eigenvalue is real and positive

!       .. Function Return Value ..
        Logical :: select
!       .. Scalar Arguments ..
        Real (Kind=dp), Intent (In) :: wi, wr
!       .. Executable Statements ..
        select = (wr>0._dp .And. wi==0._dp)
        Return
      End Function
    End Module
    Program dgees_example

!     DGEES Example Main Program

!     .. Use Statements ..
      Use blas_interfaces, Only: dgemm
      Use dgees_example_mod, Only: nb, nin, nout, select
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgees, dlange
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Local Scalars ..
      Real (Kind=dp) :: alpha, beta, norm
      Integer :: i, ifail, info, lda, ldc, ldd, ldvs, lwork, n, sdim
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), c(:, :), d(:, :), vs(:, :), &
        wi(:), work(:), wr(:)
      Real (Kind=dp) :: dummy(1), rwork(1)
      Logical, Allocatable :: bwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, nint
!     .. Executable Statements ..
      Write (nout, *) 'DGEES Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldc = n
      ldd = n
      ldvs = n
      Allocate (a(lda,n), c(ldc,n), d(ldd,n), vs(ldvs,n), wi(n), wr(n), &
        bwork(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call dgees('Vectors (Schur)', 'Sort', select, n, a, lda, sdim, wr, wi, &
        vs, ldvs, dummy, lwork, bwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+2)*n, nint(dummy(1)))
      Allocate (work(lwork))

!     Read in the matrix A
      Read (nin, *)(a(i,1:n), i=1, n)

!     Copy A into D
      d(1:n, 1:n) = a(1:n, 1:n)

!     Print matrix A
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Matrix A', ifail)

      Write (nout, *)
      Flush (nout)

!     Find the Schur factorization of A
      Call dgees('Vectors (Schur)', 'Sort', select, n, a, lda, sdim, wr, wi, &
        vs, ldvs, work, lwork, bwork, info)

      If (info==0 .Or. info==(n+2)) Then

!       Compute A - Z*T*Z^T from the factorization of A and store in matrix D
        alpha = 1.0_dp
        beta = 0.0_dp
        Call dgemm('N', 'N', n, n, n, alpha, vs, ldvs, a, lda, beta, c, ldc)
        alpha = -1.0_dp
        beta = 1.0_dp
        Call dgemm('N', 'T', n, n, n, alpha, c, ldc, vs, ldvs, beta, d, ldd)

!       Find norm of matrix D and print warning if it is too large
        norm = dlange('O', ldd, n, d, ldd, rwork)
        If (norm>epsilon(1.0E0_dp)**0.8_dp) Then
          Write (nout, *) 'Norm of A-(Z*T*Z^T) is much greater than 0.'
          Write (nout, *) 'Schur factorization has failed.'
        Else

!         Print solution
          Write (nout, 100) &
            'Number of eigenvalues for which SELECT is true = ', sdim, &
            '(dimension of invariant subspace)'

          Write (nout, *)
!         Print eigenvalues.
          Write (nout, *) 'Selected eigenvalues'
          Write (nout, 120)(' (', wr(i), ',', wi(i), ')', i=1, sdim)
          Write (nout, *)

          If (info==(n+2)) Then
            Write (nout, 130) '***Note that rounding errors mean ', &
              'that leading eigenvalues in the Schur form', &
              'no longer satisfy SELECT = .TRUE.'
            Write (nout, *)
          End If

        End If

      Else
        Write (nout, 110) 'Failure in DGEES. INFO = ', info
      End If

100   Format (1X, A, I4, /, 1X, A)
110   Format (1X, A, I4)
120   Format (1X, A, F8.4, A, F8.4, A)
130   Format (1X, 2A, /, 1X, A)
    End Program
