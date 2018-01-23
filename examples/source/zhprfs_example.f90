    Program zhprfs_example

!     ZHPRFS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zhprfs, zhptrf, zhptrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: aplen, i, ifail, info, j, ldb, ldx, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: afp(:), ap(:), b(:, :), work(:), &
        x(:, :)
      Real (Kind=dp), Allocatable :: berr(:), ferr(:), rwork(:)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZHPRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      ldb = n
      ldx = n
      aplen = n*(n+1)/2
      Allocate (afp(aplen), ap(aplen), b(ldb,nrhs), work(2*n), x(ldx,n), &
        berr(nrhs), ferr(nrhs), rwork(n), ipiv(n))

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
      Call zhptrf(uplo, n, afp, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution in the array X
        Call zhptrs(uplo, n, nrhs, afp, ipiv, x, ldx, info)

!       Improve solution, and compute backward errors and
!       estimated bounds on the forward errors

        Call zhprfs(uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr, berr, &
          work, rwork, info)

!       Print solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, &
          x, ldx, 'Bracketed', 'F7.4', 'Solution(s)', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

        Write (nout, *)
        Write (nout, *) 'Backward errors (machine-dependent)'
        Write (nout, 100) berr(1:nrhs)
        Write (nout, *) 'Estimated forward error bounds (machine-dependent)'
        Write (nout, 100) ferr(1:nrhs)
      Else
        Write (nout, *) 'The factor D is singular'
      End If

100   Format ((5X,1P,4(E11.1,7X)))
    End Program
