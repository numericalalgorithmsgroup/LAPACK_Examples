    Program zhseqr_example

!     ZHSEQR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: zgemm
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zhseqr, zlange
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: alpha, beta
      Real (Kind=dp) :: norm
      Integer :: i, ifail, info, ldc, ldd, ldh, ldz, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: c(:, :), d(:, :), h(:, :), w(:), &
        work(:), z(:, :)
      Real (Kind=dp) :: rwork(1)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: cmplx, epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZHSEQR Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldc = n
      ldd = n
      ldh = n
      ldz = n
      lwork = n
      Allocate (c(ldc,n), d(ldd,n), h(ldh,n), w(n), work(lwork), z(ldz,n))

!     Read H from data file

      Read (nin, *)(h(i,1:n), i=1, n)

!     Store H in D
      d(1:ldd, 1:n) = h(1:ldh, 1:n)

!     Print matrix H
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, h, &
        ldh, 'Bracketed', 'F7.4', 'Matrix H', 'Integer', rlabs, 'Integer', &
        clabs, 80, 0, ifail)

!     Calculate the eigenvalues and Schur factorization of H

      Call zhseqr('Schur form', 'Initialize Z', n, 1, n, h, ldh, w, z, ldz, &
        work, lwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Compute A - Z*T*Z^H from Schur factorization of A, and store in matrix
!       D
        alpha = cmplx(1, kind=dp)
        beta = cmplx(0, kind=dp)
        Call zgemm('N', 'N', n, n, n, alpha, z, ldz, h, ldh, beta, c, ldc)
        alpha = cmplx(-1, kind=dp)
        beta = cmplx(1, kind=dp)
        Call zgemm('N', 'C', n, n, n, alpha, c, ldc, z, ldz, beta, d, ldd)

!       Find norm of matrix D and print warning if it is too large
        norm = zlange('O', ldd, n, d, ldd, rwork)

        If (norm>epsilon(1.0E0_dp)**0.5_dp) Then
          Write (nout, *) 'Norm of A-(Z*T*Z^H) is much greater than 0.'
          Write (nout, *) 'Schur factorization has failed.'
        Else
!         Print eigenvalues
          Write (nout, *) 'Eigenvalues'
          Write (nout, 100)(w(i), i=1, n)
        End If

      End If

100   Format ((3X,4(' (',F7.4,',',F7.4,')',:)))
    End Program
