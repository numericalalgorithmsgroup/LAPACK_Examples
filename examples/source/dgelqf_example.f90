    Program dgelqf_example

!     DGELQF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dtrsm
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgelqf, dormlq
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0E0_dp
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldb, lwork, m, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), tau(:), work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGELQF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n, nrhs
      lda = m
      ldb = n
      lwork = 64*n
      Allocate (a(lda,n), b(ldb,nrhs), tau(n), work(lwork))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, m)
      Read (nin, *)(b(i,1:nrhs), i=1, m)

!     Compute the LQ factorization of A
      Call dgelqf(m, n, a, lda, tau, work, lwork, info)

!     Solve L*Y = B, storing the result in B
      Call dtrsm('Left', 'Lower', 'No transpose', 'Non-Unit', m, nrhs, one, a, &
        lda, b, ldb)

!     Set rows (M+1) to N of B to zero

      If (m<n) Then
        b(m+1:n, 1:nrhs) = zero
      End If

!     Compute minimum-norm solution X = (Q**T)*B in B
      Call dormlq('Left', 'Transpose', n, nrhs, m, a, lda, tau, b, ldb, work, &
        lwork, info)

!     Print minimum-norm solution(s)

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, b, ldb, &
        'Minimum-norm solution(s)', ifail)

    End Program
