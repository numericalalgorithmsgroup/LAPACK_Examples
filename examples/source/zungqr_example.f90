    Program zungqr_example

!     ZUNGQR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgeqrf, zungqr
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
      Write (nout, *) 'ZUNGQR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      lwork = 64*n
      Allocate (a(lda,n), tau(n), work(lwork))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Compute the QR factorization of A
      Call zgeqrf(m, n, a, lda, tau, work, lwork, info)

!     Form the leading N columns of Q explicitly
      Call zungqr(m, n, n, a, lda, tau, work, lwork, info)

!     Print the leading N columns of Q only

      Write (nout, *)
      Write (title, 100) n
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, a, &
        lda, 'Bracketed', 'F7.4', title, 'Integer', rlabs, 'Integer', clabs, &
        80, 0, ifail)

100   Format ('The leading ', I2, ' columns of Q')
    End Program
