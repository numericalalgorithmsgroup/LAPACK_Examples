    Program dorghr_example

!     DORGHR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dgemm
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgehrd, dhseqr, dlange, dorghr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: alpha, beta, norm
      Integer :: i, ifail, info, lda, ldc, ldd, ldz, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), c(:, :), d(:, :), tau(:), wi(:), &
        work(:), wr(:), z(:, :)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'DORGHR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldz = n
      ldc = n
      ldd = n
      lwork = 64*(n-1)
      Allocate (a(lda,n), c(ldc,n), d(ldd,n), tau(n), wi(n), work(lwork), &
        wr(n), z(ldz,n))

!     Read A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     Copy A into D.
      d(1:n, 1:n) = a(1:n, 1:n)

      Write (nout, *)
      Flush (nout)

!     Print Matrix A
!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Matrix A', ifail)

      Write (nout, *)
      Flush (nout)

!     Reduce A to upper Hessenberg form H = (Q**T)*A*Q
      Call dgehrd(n, 1, n, a, lda, tau, work, lwork, info)

!     Copy A into Z
      z(1:n, 1:n) = a(1:n, 1:n)

!     Form Q explicitly, storing the result in Z
      Call dorghr(n, 1, n, z, ldz, tau, work, lwork, info)

!     Calculate the Schur factorization of H = Y*T*(Y**T) and form
!     Q*Y explicitly, storing the result in Z

!     Note that A = Z*T*(Z**T), where Z = Q*Y
      Call dhseqr('Schur form', 'Vectors', n, 1, n, a, lda, wr, wi, z, ldz, &
        work, lwork, info)

!     Compute A - Z*T*Z^T from the factorization of A and store in matrix D.
      alpha = 1.0_dp
      beta = 0.0_dp
      Call dgemm('N', 'N', n, n, n, alpha, z, ldz, a, lda, beta, c, ldc)
      alpha = -1.0_dp
      beta = 1.0_dp
      Call dgemm('N', 'T', n, n, n, alpha, c, ldc, z, ldz, beta, d, ldd)

!     Find norm of difference matrix D and warn if it is too large;
      norm = dlange('O', ldd, n, d, ldd, work)
      If (norm>epsilon(1.0E0_dp)**0.8_dp) Then
        Write (nout, *) 'Norm of A-(Z*T*Z^T) is much greater than 0.'
        Write (nout, *) 'Schur factorization has failed.'
      Else
!       Print eigenvalues.
        Write (nout, *) 'Eigenvalues'
        Write (nout, 100)(' (', wr(i), ',', wi(i), ')', i=1, n)
      End If

100   Format (1X, A, F8.4, A, F8.4, A)

    End Program
