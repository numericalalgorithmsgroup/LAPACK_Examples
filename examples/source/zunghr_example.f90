    Program zunghr_example

!     ZUNGHR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: zgemm
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgehrd, zhseqr, zlange, zunghr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: alpha, beta
      Real (Kind=dp) :: norm
      Integer :: i, ifail, info, lda, ldc, ldd, ldz, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), c(:, :), d(:, :), tau(:), &
        w(:), work(:), z(:, :)
      Real (Kind=dp), Allocatable :: rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: cmplx, epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZUNGHR Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldc = n
      ldd = n
      ldz = n
      lwork = 64*(n-1)
      Allocate (a(lda,n), c(ldc,n), d(ldd,n), rwork(lda), tau(n), w(n), &
        work(lwork), z(ldz,n))

!     Read A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     Store A in D
      d(1:ldd, 1:n) = a(1:lda, 1:n)

!     Print matrix A
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.4', 'Matrix A', 'Integer', rlabs, 'Integer', &
        clabs, 80, 0, ifail)
      Write (nout, *)
      Flush (nout)

!     Reduce A to upper Hessenberg form H = (Q**H)*A*Q

      Call zgehrd(n, 1, n, a, lda, tau, work, lwork, info)

!     Copy A into Z
      z(1:n, 1:n) = a(1:n, 1:n)

!     Form Q explicitly, storing the result in Z
      Call zunghr(n, 1, n, z, ldz, tau, work, lwork, info)

!     Calculate the Schur factorization of H = Y*T*(Y**H) and form
!     Q*Y explicitly, storing the result in Z

!     Note that A = Z*T*(Z**H), where Z = Q*Y

      Call zhseqr('Schur form', 'Vectors', n, 1, n, a, lda, w, z, ldz, work, &
        lwork, info)

!     Compute A - Z*T*Z^H from Schur factorization of A, and store in matrix D
      alpha = cmplx(1, kind=dp)
      beta = cmplx(0, kind=dp)
      Call zgemm('N', 'N', n, n, n, alpha, z, ldz, a, lda, beta, c, ldc)
      alpha = cmplx(-1, kind=dp)
      beta = cmplx(1, kind=dp)
      Call zgemm('N', 'C', n, n, n, alpha, c, ldc, z, ldz, beta, d, ldd)

!     Find norm of matrix D and print warning if it is too large
      norm = zlange('O', ldd, n, d, ldd, rwork)
      If (norm>epsilon(1.0E0_dp)**0.5_dp) Then
        Write (nout, *) 'Norm of A-(Z*T*Z^H) is much greater than 0.'
        Write (nout, *) 'Schur factorization has failed.'
      Else
!       Print eigenvalues.
        Write (nout, *) 'Eigenvalues'
        Write (nout, 100)(w(i), i=1, n)
      End If

100   Format ((3X,4(' (',F7.4,',',F7.4,')',:)))

    End Program
