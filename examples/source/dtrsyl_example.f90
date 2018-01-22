    Program dtrsyl_example

!     DTRSYL Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dtrsyl
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: scale
      Integer :: i, ifail, info, lda, ldb, ldc, m, n, sign
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), c(:, :)
!     .. Executable Statements ..
      Write (nout, *) 'DTRSYL Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldb = n
      ldc = m
      sign = 1
      Allocate (a(lda,m), b(ldb,n), c(ldc,n))

!     Read A, B and C from data file

      Read (nin, *)(a(i,1:m), i=1, m)
      Read (nin, *)(b(i,1:n), i=1, n)
      Read (nin, *)(c(i,1:n), i=1, m)

!     Solve the Sylvester equation A*X + X*B = C for X
      Call dtrsyl('No transpose', 'No transpose', sign, m, n, a, lda, b, ldb, &
        c, ldc, scale, info)
      If (info==1) Then
        Write (nout, 100)
        Write (nout, *)
      End If

      Flush (nout)

!     Print X
!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', m, n, c, ldc, &
        'Solution Matrix', ifail)

100   Format (/, ' A and B have common or very close eigenvalues.', /, ' Pe', &
        'rturbed values were used to solve the equations')
    End Program
