    Program zgtsvx_example

!     ZGTSVX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgtsvx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, ifail, info, ldb, ldx, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: b(:, :), d(:), df(:), dl(:), dlf(:), &
        du(:), du2(:), duf(:), work(:), x(:, :)
      Real (Kind=dp), Allocatable :: berr(:), ferr(:), rwork(:)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZGTSVX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      ldx = n
      Allocate (b(ldb,nrhs), d(n), df(n), dl(n-1), dlf(n-1), du(n-1), &
        du2(n-2), duf(n-1), work(2*n), x(ldx,nrhs), berr(nrhs), ferr(nrhs), &
        rwork(n), ipiv(n))

!     Read the tridiagonal matrix A from data file

      Read (nin, *) du(1:n-1)
      Read (nin, *) d(1:n)
      Read (nin, *) dl(1:n-1)

!     Read the right hand matrix B

      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Solve the equations AX = B
      Call zgtsvx('No factors', 'No transpose', n, nrhs, dl, d, du, dlf, df, &
        duf, du2, ipiv, b, ldb, x, ldx, rcond, ferr, berr, work, rwork, info)

      If ((info==0) .Or. (info==n+1)) Then

!       Print solution, error bounds and condition number

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, &
          x, ldx, 'Bracketed', 'F7.4', 'Solution(s)', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

        Write (nout, *)
        Write (nout, *) 'Backward errors (machine-dependent)'
        Write (nout, 100) berr(1:nrhs)
        Write (nout, *)
        Write (nout, *) 'Estimated forward error bounds (machine-dependent)'
        Write (nout, 100) ferr(1:nrhs)
        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal condition number'
        Write (nout, 100) rcond

        If (info==n+1) Then
          Write (nout, *)
          Write (nout, *) 'The matrix A is singular to working precision'
        End If
      Else
        Write (nout, 110) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format ((3X,1P,7E11.1))
110   Format (1X, A, I3, A, I3, A, A)
    End Program
