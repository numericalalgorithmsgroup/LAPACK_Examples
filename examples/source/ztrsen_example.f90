    Program ztrsen_example

!     ZTRSEN Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: zgemm
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zlange, ztrsen
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: alpha, beta
      Real (Kind=dp) :: norm, s, sep
      Integer :: i, ifail, info, lda, ldc, ldq, ldt, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), c(:, :), q(:, :), t(:, :), &
        w(:), work(:)
      Real (Kind=dp) :: rwork(1)
      Logical, Allocatable :: select(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: cmplx, epsilon
!     .. Executable Statements ..
      Write (nout, *) 'ZTRSEN Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldc = n
      lda = n
      ldq = n
      ldt = n
      lwork = (n*n)/2
      Allocate (a(lda,n), c(ldc,n), q(ldq,n), t(ldt,n), w(n), work(lwork), &
        select(n))

!     Read T, Q and the logical array SELECT from data file

      Read (nin, *)(t(i,1:n), i=1, n)
      Read (nin, *)
      Read (nin, *)(q(i,1:n), i=1, n)
      Read (nin, *)
      Read (nin, *) select(1:n)

!     Compute Q * T * Q**T to find  A
      alpha = cmplx(1, kind=dp)
      beta = cmplx(0, kind=dp)
      Call zgemm('N', 'N', n, n, n, alpha, q, ldq, t, ldt, beta, c, ldc)
      Call zgemm('N', 'C', n, n, n, alpha, c, ldc, q, ldq, beta, a, lda)

!     Print Matrix A, as computed from Q * T * Q**T
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.4', 'Matrix A created from Q*T*Q^T', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

!     Reorder the Schur factor T and update the matrix Q to obtain TT and QT

      Call ztrsen('Both', 'Vectors', select, n, t, ldt, q, ldq, w, m, s, sep, &
        work, lwork, info)

!     Compute (Q * T * Q^H) - (QT * TT * QT^H) and store in A,
!     i.e. the difference between reconstructed A using Schur and reordered
!          Schur decompositions.
      alpha = cmplx(1, kind=dp)
      beta = cmplx(0, kind=dp)
      Call zgemm('N', 'N', n, n, n, alpha, q, ldq, t, ldt, beta, c, ldc)
      alpha = cmplx(-1, kind=dp)
      beta = cmplx(1, kind=dp)
      Call zgemm('N', 'C', n, n, n, alpha, c, ldc, q, ldq, beta, a, lda)

!     Find norm of difference matrix and print warning if it is too large
      norm = zlange('O', lda, n, a, lda, rwork)
      If (norm>epsilon(1.0E0_dp)**0.5_dp) Then
        Write (nout, *) 'Norm of A - (QT * TT * QT^H) is much greater than 0.'
        Write (nout, *) 'Schur factorization has failed.'
      Else
!       Print condition estimates
        Write (nout, 100) 'Condition number estimate', &
          ' of the selected cluster of eigenvalues = ', 1.0_dp/s
        Write (nout, *)
        Write (nout, 100) 'Condition number estimate of the specified ', &
          'invariant subspace    = ', 1.0_dp/sep
      End If

100   Format (1X, A, A, 1P, E10.2)
    End Program
