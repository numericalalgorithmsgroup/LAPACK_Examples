    Program dhseqr_example

!     DHSEQR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dgemm
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dhseqr, dlange
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: alpha, beta, norm
      Integer :: i, ifail, info, ldc, ldd, ldh, ldz, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: c(:, :), d(:, :), h(:, :), wi(:), &
        work(:), wr(:), z(:, :)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'DHSEQR Example Program Results'
      Flush (nout)

!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldc = n
      ldd = n
      ldh = n
      ldz = n
      lwork = n
      Allocate (c(ldc,n), d(ldd,n), h(ldh,n), wi(n), work(lwork), wr(n), &
        z(ldz,n))

!     Read H from data file
      Read (nin, *)(h(i,1:n), i=1, n)

!     Copy H into D
      d(1:n, 1:n) = h(1:n, 1:n)

!     Print Matrix H
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, h, ldh, &
        'Matrix H', ifail)

!     Calculate the eigenvalues and Schur factorization of H

      Call dhseqr('Schur form', 'Initialize Z', n, 1, n, h, ldh, wr, wi, z, &
        ldz, work, lwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Compute H - Z*T*Z^T from the factorization of A and store in matrix D
        alpha = 1.0_dp
        beta = 0.0_dp
        Call dgemm('N', 'N', n, n, n, alpha, z, ldz, h, ldh, beta, c, ldc)
        alpha = -1.0_dp
        beta = 1.0_dp
        Call dgemm('N', 'T', n, n, n, alpha, c, ldc, z, ldz, beta, d, ldd)

!       Find norm of matrix D and print warning if it is too large
        norm = dlange('O', ldd, n, d, ldd, work)
        If (norm>epsilon(1.0E0_dp)**0.8_dp) Then
          Write (nout, *) 'Norm of H-(Z*T*Z^T) is much greater than 0.'
          Write (nout, *) 'Schur factorization has failed.'
        Else
!         Print eigenvalues
          Write (nout, *) 'Eigenvalues'
          Write (nout, 100)(' (', wr(i), ',', wi(i), ')', i=1, n)
        End If
      End If

100   Format (1X, A, F8.4, A, F8.4, A)
    End Program
