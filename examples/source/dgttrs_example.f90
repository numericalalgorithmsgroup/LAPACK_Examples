    Program dgttrs_example

!     DGTTRS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgttrf, dgttrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, ldb, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: b(:, :), d(:), dl(:), du(:), du2(:)
      Integer, Allocatable :: ipiv(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGTTRS Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      Allocate (b(ldb,nrhs), d(n), dl(n-1), du(n-1), du2(n-2), ipiv(n))

!     Read the tridiagonal matrix A from data file

      Read (nin, *) du(1:n-1)
      Read (nin, *) d(1:n)
      Read (nin, *) dl(1:n-1)

!     Read the right hand matrix B
      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Factorize the tridiagonal matrix A
      Call dgttrf(n, dl, d, du, du2, ipiv, info)

      If (info==0) Then

!       Solve the equations AX = B
        Call dgttrs('No transpose', n, nrhs, dl, d, du, du2, ipiv, b, ldb, &
          info)

!       Print the solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, b, ldb, &
          'Solution(s)', ifail)

      Else
        Write (nout, 100) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format (1X, A, I3, A, I3, A, A)
    End Program
