    Program dposv_example

!     DPOSV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dposv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:)
!     .. Executable Statements ..
      Write (nout, *) 'DPOSV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      Allocate (a(lda,n), b(n))

!     Read the upper triangular part of A from data file

      Read (nin, *)(a(i,i:n), i=1, n)

!     Read b from data file

      Read (nin, *) b(1:n)

!     Solve the equations Ax = b for x
      Call dposv('Upper', n, 1, a, lda, b, n, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100) b(1:n)

!       Print details of factorization

        Write (nout, *)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('Upper', 'Non-unit diagonal', n, &
          n, a, lda, 'Cholesky factor U', ifail)

      Else
        Write (nout, 110) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format ((3X,7F11.4))
110   Format (1X, A, I3, A)
    End Program
