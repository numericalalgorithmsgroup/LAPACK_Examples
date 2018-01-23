    Program dgetrs_example

!     DGETRS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgetrf, dgetrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldb, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :)
      Integer, Allocatable :: ipiv(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGETRS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      lda = n
      ldb = n
      Allocate (a(lda,n), b(ldb,nrhs), ipiv(n))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Factorize A

      Call dgetrf(n, n, a, lda, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution

        Call dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)

!       Print solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, b, ldb, &
          'Solution(s)', ifail)
      Else
        Write (nout, *) 'The factor U is singular'
      End If

    End Program
