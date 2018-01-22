    Program ztprfs_example

!     ZTPRFS Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: ztprfs, ztptrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: diag = 'N', trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, ldb, ldx, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), b(:, :), work(:), x(:, :)
      Real (Kind=dp), Allocatable :: berr(:), ferr(:), rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZTPRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      ldx = n
      Allocate (ap(n*(n+1)/2), b(ldb,nrhs), work(2*n), x(ldx,n), berr(nrhs), &
        ferr(nrhs), rwork(n))

!     Read A and B from data file, and copy B to X

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If
      Read (nin, *)(b(i,1:nrhs), i=1, n)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Compute solution in the array X
      Call ztptrs(uplo, trans, diag, n, nrhs, ap, x, ldx, info)

!     Compute backward errors and estimated bounds on the
!     forward errors

      Call ztprfs(uplo, trans, diag, n, nrhs, ap, b, ldb, x, ldx, ferr, berr, &
        work, rwork, info)

!     Print solution

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, x, &
        ldx, 'Bracketed', 'F7.4', 'Solution(s)', 'Integer', rlabs, 'Integer', &
        clabs, 80, 0, ifail)

      Write (nout, *)
      Write (nout, *) 'Backward errors (machine-dependent)'
      Write (nout, 100) berr(1:nrhs)
      Write (nout, *) 'Estimated forward error bounds (machine-dependent)'
      Write (nout, 100) ferr(1:nrhs)

100   Format ((5X,1P,4(E11.1,7X)))
    End Program
