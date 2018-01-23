    Program dtrrfs_example

!     DTRRFS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dtrrfs, dtrtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: diag = 'N', trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldb, ldx, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), berr(:), ferr(:), &
        work(:), x(:, :)
      Integer, Allocatable :: iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DTRRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      lda = n
      ldb = n
      ldx = n
      Allocate (a(lda,n), b(ldb,nrhs), berr(nrhs), ferr(nrhs), work(3*n), &
        x(ldx,n), iwork(n))

!     Read A and B from data file, and copy B to X

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If
      Read (nin, *)(b(i,1:nrhs), i=1, n)

      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Compute solution in the array X
      Call dtrtrs(uplo, trans, diag, n, nrhs, a, lda, x, ldx, info)

!     Compute backward errors and estimated bounds on the
!     forward errors

      Call dtrrfs(uplo, trans, diag, n, nrhs, a, lda, b, ldb, x, ldx, ferr, &
        berr, work, iwork, info)

!     Print solution

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, x, ldx, &
        'Solution(s)', ifail)

      Write (nout, *)
      Write (nout, *) 'Backward errors (machine-dependent)'
      Write (nout, 100) berr(1:nrhs)
      Write (nout, *) 'Estimated forward error bounds (machine-dependent)'
      Write (nout, 100) ferr(1:nrhs)

100   Format ((3X,1P,7E11.1))
    End Program
