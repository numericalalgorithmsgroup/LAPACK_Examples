    Program dbdsqr_example

!     DBDSQR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dbdsqr, dlaset
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0_dp
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: ifail, info, ldc, ldu, ldvt, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: c(:, :), d(:), e(:), u(:, :), vt(:, :), &
        work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DBDSQR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldc = 1
      ldu = n
      ldvt = n
      Allocate (c(ldc,1), d(n), e(n-1), u(ldu,n), vt(ldvt,n), work(4*n))

!     Read B from data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

      Read (nin, *) uplo

!     Initialize U and VT to be the unit matrix
      Call dlaset('General', n, n, zero, one, u, ldu)
      Call dlaset('General', n, n, zero, one, vt, ldvt)

!     Calculate the SVD of B
      Call dbdsqr(uplo, n, n, n, 0, d, e, vt, ldvt, u, ldu, c, ldc, work, &
        info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print singular values, left & right singular vectors

        Write (nout, *) 'Singular values'
        Write (nout, 100) d(1:n)
        Write (nout, *)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, vt, ldvt, &
          'Right singular vectors, by row', ifail)

        Write (nout, *)
        Flush (nout)

        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, u, ldu, &
          'Left singular vectors, by column', ifail)

      End If

100   Format (3X, (8F8.4))
    End Program
