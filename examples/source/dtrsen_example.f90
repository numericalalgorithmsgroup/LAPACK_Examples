    Program dtrsen_example

!     DTRSEN Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dgemm
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dlange, dtrsen
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: alpha, beta, norm, s, sep
      Integer :: i, ifail, info, lda, ldc, ldq, ldt, liwork, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), c(:, :), q(:, :), t(:, :), &
        wi(:), work(:), wr(:)
      Integer, Allocatable :: iwork(:)
      Logical, Allocatable :: select(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon
!     .. Executable Statements ..
      Write (nout, *) 'DTRSEN Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldc = n
      ldq = n
      ldt = n
      liwork = (n*n)/4
      lwork = (n*n)/2
      Allocate (a(lda,n), c(ldc,n), q(ldq,n), t(ldt,n), wi(n), work(lwork), &
        wr(n), iwork(liwork), select(n))

!     Read T, Q and the logical array SELECT from data file

      Read (nin, *)(t(i,1:n), i=1, n)
      Read (nin, *)(q(i,1:n), i=1, n)

      Read (nin, *) select(1:n)

!     Compute Q * T * Q**T to find  A
      alpha = 1._dp
      beta = 0._dp
      Call dgemm('N', 'N', n, n, n, alpha, q, ldq, t, ldt, beta, c, ldc)
      Call dgemm('N', 'T', n, n, n, alpha, c, ldc, q, ldq, beta, a, lda)

!     Print Matrix A, as computed from Q * T * Q**T
!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Matrix A computed from Q*T*Q^T', ifail)

      Write (nout, *)
      Flush (nout)

!     Reorder the Schur factor T and update the matrix Q to obtain TT and QT

      Call dtrsen('Both', 'Vectors', select, n, t, ldt, q, ldq, wr, wi, m, s, &
        sep, work, lwork, iwork, liwork, info)

!     Compute (Q * T * Q^T) - (QT * TT * QT^T) and store in A,
!     i.e. the difference between reconstructed A using Schur and reordered
!          Schur decompositions.
      alpha = 1._dp
      beta = 0._dp
      Call dgemm('N', 'N', n, n, n, alpha, q, ldq, t, ldt, beta, c, ldc)
      alpha = -1._dp
      beta = 1._dp
      Call dgemm('N', 'T', n, n, n, alpha, c, ldc, q, ldq, beta, a, lda)

!     Find norm of difference matrix and print warning if it is too large
      norm = dlange('O', lda, n, a, lda, work)
      If (norm>epsilon(1.0E0_dp)**0.8_dp) Then
        Write (nout, *) 'Norm of A - (QT * TT * QT^T) is much greater than 0.'
        Write (nout, *) 'Schur factorization has failed.'
      Else
!       Print Result
        Write (nout, 100) 'Condition number estimate', &
          ' of the selected cluster of eigenvalues = ', 1.0_dp/s
        Write (nout, *)
        Write (nout, 100) 'Condition number estimate of the spec', &
          'ified invariant subspace    = ', 1.0_dp/sep
      End If

100   Format (1X, A, A, 1P, E10.2)
    End Program
