    Program zptrfs_example

!     ZPTRFS Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zptrfs, zpttrf, zpttrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, ldb, ldx, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: b(:, :), e(:), ef(:), work(:), x(:, :)
      Real (Kind=dp), Allocatable :: berr(:), d(:), df(:), ferr(:), rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZPTRFS Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      ldx = n
      Allocate (b(ldb,nrhs), e(n-1), ef(n-1), work(n), x(ldx,nrhs), &
        berr(nrhs), d(n), df(n), ferr(nrhs), rwork(n))

!     Read the lower bidiagonal part of the tridiagonal matrix A from
!     data file

      Read (nin, *) d(1:n)
      Read (nin, *) e(1:n-1)

!     Read the right hand matrix B

      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Copy A into DF and EF, and copy B into X
      df(1:n) = d(1:n)
      ef(1:n-1) = e(1:n-1)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize the copy of the tridiagonal matrix A
      Call zpttrf(n, df, ef, info)

      If (info==0) Then

!       Solve the equations AX = B
        Call zpttrs('Lower', n, nrhs, df, ef, x, ldx, info)

!       Improve the solution and compute error estimates
        Call zptrfs('Lower', n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr, &
          berr, work, rwork, info)

!       Print the solution and the forward and backward error estimates

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
      Else
        Write (nout, 110) 'The leading minor of order ', info, &
          ' is not positive definite'
      End If

100   Format ((3X,1P,7E11.1))
110   Format (1X, A, I3, A)
    End Program
