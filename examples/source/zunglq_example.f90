    Program zunglq_example

!     ZUNGLQ Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgelqf, zunglq
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, lwork, m, n
      Character (30) :: title
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), tau(:), work(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZUNGLQ Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = 64*m
      Allocate (a(lda,n), tau(n), work(lwork))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Compute the LQ factorization of A
      Call zgelqf(m, n, a, lda, tau, work, lwork, info)

!     Form the leading M rows of Q explicitly
      Call zunglq(m, n, m, a, lda, tau, work, lwork, info)

!     Print the leading M rows of Q only

      Write (nout, *)
      Write (title, 100) m
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, a, &
        lda, 'Bracketed', 'F7.4', title, 'Integer', rlabs, 'Integer', clabs, &
        80, 0, ifail)

100   Format ('The leading ', I2, ' rows of Q')
    End Program
