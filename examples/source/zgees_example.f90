    Module zgees_example_mod

!     ZGEES Example Program Module:
!           Parameters and User-defined Routines

!     .. Implicit None Statement ..
      Implicit None
!     .. Accessibility Statements ..
      Private
      Public :: select
    Contains
      Function select(w)
!       .. Use Statements ..
        Use lapack_precision, Only: dp
!       .. Implicit None Statement ..
        Implicit None
!       .. Function Return Value ..
        Logical :: select
!       .. Scalar Arguments ..
        Complex (Kind=dp), Intent (In) :: w
!       .. Intrinsic Procedures ..
        Intrinsic :: real
!       .. Executable Statements ..
        Continue

!       Dummy function - it is not called by ZGEES when sorting is not required.
        select = (real(w)>0._dp)

        Return
      End Function
    End Module
    Program zgees_example

!     ZGEES Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: zgemm
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgees, zlange
      Use lapack_precision, Only: dp
      Use zgees_example_mod, Only: select
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: alpha, beta
      Real (Kind=dp) :: norm
      Integer :: i, ifail, info, lda, ldc, ldd, ldvs, lwork, n, sdim
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), c(:, :), d(:, :), vs(:, :), &
        w(:), work(:)
      Complex (Kind=dp) :: wdum(1)
      Real (Kind=dp), Allocatable :: rwork(:)
      Logical :: dummy(1)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: cmplx, epsilon, max, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGEES Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldc = n
      ldd = n
      ldvs = n
      Allocate (a(lda,n), vs(ldvs,n), c(ldc,n), d(ldd,n), w(n), rwork(n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zgees('Vectors (Schur)', 'No sort', select, n, a, lda, sdim, w, vs, &
        ldvs, wdum, lwork, rwork, dummy, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+1)*n, nint(real(wdum(1))))
      Allocate (work(lwork))

!     Read in the matrix A
      Read (nin, *)(a(i,1:n), i=1, n)

!     Copy A into D
      d(1:n, 1:n) = a(1:n, 1:n)

!     Print matrix A
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.4', 'Matrix A', 'Integer', rlabs, 'Integer', &
        clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

!     Find the Schur factorization of A
      Call zgees('Vectors (Schur)', 'No sort', select, n, a, lda, sdim, w, vs, &
        ldvs, work, lwork, rwork, dummy, info)

      If (info>0) Then
        Write (nout, 100) 'Failure in ZGEES. INFO =', info
      Else

!       Compute A - Z*T*Z^H from the factorization of A and store in matrix D
        alpha = cmplx(1, kind=dp)
        beta = cmplx(0, kind=dp)
        Call zgemm('N', 'N', n, n, n, alpha, vs, ldvs, a, lda, beta, c, ldc)
        alpha = cmplx(-1, kind=dp)
        beta = cmplx(1, kind=dp)
        Call zgemm('N', 'C', n, n, n, alpha, c, ldc, vs, ldvs, beta, d, ldd)

!       Find norm of matrix D and print warning if it is too large
        norm = zlange('O', ldd, n, d, ldd, rwork)
        If (norm>epsilon(1.0E0_dp)**0.5_dp) Then
          Write (nout, *) 'Norm of A-(Z*T*Z^H) is much greater than 0.'
          Write (nout, *) 'Schur factorization has failed.'
        Else
!         Print eigenvalues.
          Write (nout, *) 'Eigenvalues'
          Write (nout, 110)(i, w(i), i=1, n)
        End If
      End If

100   Format (1X, A, I4)
110   Format (1X, I4, 2X, ' (', F7.4, ',', F7.4, ')', :)
    End Program
