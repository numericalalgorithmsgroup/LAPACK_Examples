    Program zgelqf_example

!     ZGELQF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: ztrsm
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgelqf, zunmlq
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: one = (1.0_dp, 0.0_dp)
      Complex (Kind=dp), Parameter :: zero = (0.0_dp, 0.0_dp)
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldb, lwork, m, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), tau(:), work(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZGELQF Example Program Results'
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
      Call zgelqf(m, n, a, lda, tau, work, lwork, info)

!     Solve L*Y = B, storing the result in B
      Call ztrsm('Left', 'Lower', 'No transpose', 'Non-Unit', m, nrhs, one, a, &
        lda, b, ldb)

!     Set rows (M+1) to N of B to zero

      If (m<n) Then
        b(m+1:n, 1:nrhs) = zero
      End If

!     Compute minimum-norm solution X = (Q**H)*B in B
      Call zunmlq('Left', 'Conjugate transpose', n, nrhs, m, a, lda, tau, b, &
        ldb, work, lwork, info)

!     Print minimum-norm solution(s)

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, b, &
        ldb, 'Bracketed', 'F7.4', 'Minimum-norm solution(s)', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

    End Program
