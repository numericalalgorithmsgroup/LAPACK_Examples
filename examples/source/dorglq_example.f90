    Program dorglq_example

!     DORGLQ Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgelqf, dorglq
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, lwork, m, n
      Character (30) :: title
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), tau(:), work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DORGLQ Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = 64*m
      Allocate (a(lda,n), tau(n), work(lwork))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Compute the LQ factorization of A
      Call dgelqf(m, n, a, lda, tau, work, lwork, info)

!     Form the leading M rows of Q explicitly
      Call dorglq(m, n, m, a, lda, tau, work, lwork, info)

!     Print the leading M rows of Q only

      Write (nout, *)
      Write (title, 100) m
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', m, n, a, lda, &
        title, ifail)

100   Format ('The leading ', I2, ' rows of Q')
    End Program
