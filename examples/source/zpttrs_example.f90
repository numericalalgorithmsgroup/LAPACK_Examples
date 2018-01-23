    Program zpttrs_example

!     ZPTTRS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zpttrf, zpttrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Integer :: i, ifail, info, ldb, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: b(:, :), e(:)
      Real (Kind=dp), Allocatable :: d(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZPTTRS Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      Allocate (b(ldb,nrhs), e(n-1), d(n))

!     Read the upper bidiagonal part of the tridiagonal matrix A from
!     data file

      Read (nin, *) e(1:n-1)
      Read (nin, *) d(1:n)

!     Read the right hand matrix B

      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Factorize the tridiagonal matrix A
      Call zpttrf(n, d, e, info)

      If (info==0) Then

!       Solve the equations AX = B
        Call zpttrs(uplo, n, nrhs, d, e, b, ldb, info)

!       Print the solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, &
          b, ldb, 'Bracketed', ' ', 'Solution(s)', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      Else
        Write (nout, 100) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format (1X, A, I3, A)
    End Program
