    Program ztrsyl_example

!     ZTRSYL Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: ztrsyl
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: scale
      Integer :: i, ifail, info, lda, ldb, ldc, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), c(:, :)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZTRSYL Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      ldb = n
      ldc = m
      Allocate (a(lda,m), b(ldb,n), c(ldc,n))

!     Read A, B and C from data file

      Read (nin, *)(a(i,1:m), i=1, m)
      Read (nin, *)(b(i,1:n), i=1, n)
      Read (nin, *)(c(i,1:n), i=1, m)

!     Solve the Sylvester equation A*X + X*B = C for X
      Call ztrsyl('No transpose', 'No transpose', 1, m, n, a, lda, b, ldb, c, &
        ldc, scale, info)
      If (info>=1) Then
        Write (nout, 100)
        Write (nout, *)
        Flush (nout)
      End If

!     Print X
!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, c, &
        ldc, 'Bracketed', 'F8.4', 'Solution Matrix', 'I', rlabs, 'I', clabs, &
        80, 0, ifail)

100   Format (/, ' A and B have common or very close eigenvalues.', /, ' Pe', &
        'rturbed values were used to solve the equations')
    End Program
