    Program dgtrfs_example

!     DGTRFS Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgtrfs, dgttrf, dgttrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, ldb, ldx, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: b(:, :), berr(:), d(:), df(:), dl(:), &
        dlf(:), du(:), du2(:), duf(:), ferr(:), work(:), x(:, :)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGTRFS Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      ldx = n
      Allocate (b(ldb,nrhs), berr(nrhs), d(n), df(n), dl(n-1), dlf(n-1), &
        du(n-1), du2(n-2), duf(n-1), ferr(nrhs), work(3*n), x(ldx,nrhs), &
        ipiv(n), iwork(n))

!     Read the tridiagonal matrix A from data file

      Read (nin, *) du(1:n-1)
      Read (nin, *) d(1:n)
      Read (nin, *) dl(1:n-1)

!     Read the right hand matrix B

      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Copy A into DUF, DF and DLF, and copy B into X

      duf(1:n-1) = du(1:n-1)
      df(1:n) = d(1:n)
      dlf(1:n-1) = dl(1:n-1)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize the copy of the tridiagonal matrix A
      Call dgttrf(n, dlf, df, duf, du2, ipiv, info)

      If (info==0) Then

!       Solve the equations AX = B
        Call dgttrs('No transpose', n, nrhs, dlf, df, duf, du2, ipiv, x, ldx, &
          info)

!       Improve the solution and compute error estimates
        Call dgtrfs('No transpose', n, nrhs, dl, d, du, dlf, df, duf, du2, &
          ipiv, b, ldb, x, ldx, ferr, berr, work, iwork, info)

!       Print the solution and the forward and backward error
!       estimates

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
      Else
        Write (nout, 110) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format ((3X,1P,7E11.1))
110   Format (1X, A, I3, A, I3, A, A)
    End Program
