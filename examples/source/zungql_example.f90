    Program zungql_example

!     ZUNGQL Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgeqlf, zungql
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, lwork, m, n
      Character (30) :: title
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), tau(:), work(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZUNGQL Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = nb*n
      Allocate (a(lda,n), tau(n), work(lwork))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Compute the QL factorization of A
      Call zgeqlf(m, n, a, lda, tau, work, lwork, info)

!     Form the leading N columns of Q explicitly
      Call zungql(m, n, n, a, lda, tau, work, lwork, info)

!     Form the heading for NAGF_FILE_PRINT_MATRIX_COMPLEX_GEN_COMP

      Write (title, 100) n
      Flush (nout)

!     Print the leading N columns of Q

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, a, &
        lda, 'Bracketed', 'F7.4', title, 'Integer', rlabs, 'Integer', clabs, &
        80, 0, ifail)

100   Format ('The leading ', I4, ' columns of Q')
    End Program
