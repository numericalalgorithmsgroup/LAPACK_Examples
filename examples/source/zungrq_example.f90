    Program zungrq_example

!     ZUNGRQ Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgerqf, zungrq
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, lwork, m, n
      Character (26) :: title
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), tau(:), work(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZUNGRQ Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = nb*m
      Allocate (a(lda,n), tau(n), work(lwork))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Compute the RQ factorization of A
      Call zgerqf(m, n, a, lda, tau, work, lwork, info)

!     Form the leading M rows of Q explicitly
      Call zungrq(m, n, m, a, lda, tau, work, lwork, info)

!     Form the heading for NAGF_FILE_PRINT_MATRIX_COMPLEX_GEN_COMP

      Write (title, 100) m
      Flush (nout)

!     Print the leading M rows of Q

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, a, &
        lda, 'Bracketed', 'F7.4', title, 'Integer', rlabs, 'Integer', clabs, &
        80, 0, ifail)

100   Format ('The leading ', I4, ' rows of Q')
    End Program
