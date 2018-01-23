    Program dposvx_example

!     DPOSVX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dposvx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, ifail, info, lda, ldaf, ldb, ldx, n, nrhs
      Character (1) :: equed
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), af(:, :), b(:, :), berr(:), &
        ferr(:), s(:), work(:), x(:, :)
      Integer, Allocatable :: iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DPOSVX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      lda = n
      ldaf = n
      ldb = n
      ldx = n
      Allocate (a(lda,n), af(ldaf,n), b(ldb,nrhs), berr(nrhs), ferr(nrhs), &
        s(n), work(3*n), x(ldx,nrhs), iwork(n))

!     Read the upper triangular part of A from data file

      Read (nin, *)(a(i,i:n), i=1, n)

!     Read B from data file

      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Solve the equations AX = B for X
      Call dposvx('Equilibration', 'Upper', n, nrhs, a, lda, af, ldaf, equed, &
        s, b, ldb, x, ldx, rcond, ferr, berr, work, iwork, info)

      If ((info==0) .Or. (info==n+1)) Then

!       Print solution, error bounds, condition number and the form
!       of equilibration

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, x, ldx, &
          'Solution(s)', ifail)

        Write (nout, *)
        Write (nout, *) 'Backward errors (machine-dependent)'
        Write (nout, 100) berr(1:nrhs)
        Write (nout, *)
        Write (nout, *) 'Estimated forward error bounds (machine-dependent)'
        Write (nout, 100) ferr(1:nrhs)
        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal condition number'
        Write (nout, 100) rcond
        Write (nout, *)
        If (equed=='N') Then
          Write (nout, *) 'A has not been equilibrated'
        Else If (equed=='Y') Then
          Write (nout, *) &
            'A has been row and column scaled as diag(S)*A*diag(S)'
        End If

        If (info==n+1) Then
          Write (nout, *)
          Write (nout, *) 'The matrix A is singular to working precision'
        End If
      Else
        Write (nout, 110) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format ((3X,1P,7E11.1))
110   Format (1X, A, I3, A)
    End Program
