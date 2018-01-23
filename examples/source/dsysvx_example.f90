    Program dsysvx_example

!     DSYSVX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dsysvx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, ifail, info, lda, ldaf, ldb, ldx, lwork, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), af(:, :), b(:, :), berr(:), &
        ferr(:), work(:), x(:, :)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSYSVX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      lda = n
      ldaf = n
      ldb = n
      ldx = n
      lwork = nb*n
      Allocate (a(lda,n), af(ldaf,n), b(ldb,nrhs), berr(nrhs), ferr(nrhs), &
        work(lwork), x(ldx,nrhs), ipiv(n), iwork(n))

!     Read the upper triangular part of A from data file

      Read (nin, *)(a(i,i:n), i=1, n)

!     Read B from data file

      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Solve the equations AX = B for X
      Call dsysvx('Not factored', 'Upper', n, nrhs, a, lda, af, ldaf, ipiv, b, &
        ldb, x, ldx, rcond, ferr, berr, work, lwork, iwork, info)

      If ((info==0) .Or. (info==n+1)) Then

!       Print solution, error bounds and condition number

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

        If (info==n+1) Then
          Write (nout, *)
          Write (nout, *) 'The matrix A is singular to working precision'
        End If
      Else
        Write (nout, 110) 'The diagonal block ', info, ' of D is zero'
      End If

100   Format ((3X,1P,7E11.1))
110   Format (1X, A, I3, A)
    End Program
