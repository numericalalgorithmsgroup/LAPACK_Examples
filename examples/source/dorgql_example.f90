    Program dorgql_example

!     DORGQL Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgeqlf, dorgql
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, lwork, m, n
      Character (30) :: title
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), tau(:), work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DORGQL Example Program Results'
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

      Call dgeqlf(m, n, a, lda, tau, work, lwork, info)

!     Form the leading N columns of Q explicitly

      Call dorgql(m, n, n, a, lda, tau, work, lwork, info)

!     Form the heading for NAGF_FILE_PRINT_MATRIX_REAL_GEN

      Write (title, 100) n
      Flush (nout)

!     Print the leading N columns of Q

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', m, n, a, lda, &
        title, ifail)

100   Format ('The leading ', I4, ' columns of Q')
    End Program
