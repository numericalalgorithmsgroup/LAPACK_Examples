    Program dpprfs_example

!     DPPRFS Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dpprfs, dpptrf, dpptrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: aplen, i, ifail, info, j, ldb, ldx, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: afp(:), ap(:), b(:, :), berr(:), ferr(:), &
        work(:), x(:, :)
      Integer, Allocatable :: iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DPPRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      ldx = n
      aplen = n*(n+1)/2
      Allocate (afp(aplen), ap(aplen), b(ldb,nrhs), berr(nrhs), ferr(nrhs), &
        work(3*n), x(ldx,n), iwork(n))

!     Read A and B from data file, and copy A to AFP and B to X

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If
      Read (nin, *)(b(i,1:nrhs), i=1, n)

      afp(1:aplen) = ap(1:aplen)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize A in the array AFP
      Call dpptrf(uplo, n, afp, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution in the array X
        Call dpptrs(uplo, n, nrhs, afp, x, ldx, info)

!       Improve solution, and compute backward errors and
!       estimated bounds on the forward errors

        Call dpprfs(uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr, work, &
          iwork, info)

!       Print solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, x, ldx, &
          'Solution(s)', ifail)

        Write (nout, *)
        Write (nout, *) 'Backward errors (machine-dependent)'
        Write (nout, 100) berr(1:nrhs)
        Write (nout, *) 'Estimated forward error bounds (machine-dependent)'
        Write (nout, 100) ferr(1:nrhs)
      Else
        Write (nout, *) 'A is not positive definite'
      End If

100   Format ((3X,1P,7E11.1))
    End Program
