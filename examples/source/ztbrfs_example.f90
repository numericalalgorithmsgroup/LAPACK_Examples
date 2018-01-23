    Program ztbrfs_example

!     ZTBRFS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: ztbrfs, ztbtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: diag = 'N', trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, ldb, ldx, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), b(:, :), work(:), x(:, :)
      Real (Kind=dp), Allocatable :: berr(:), ferr(:), rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZTBRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd, nrhs
      ldab = kd + 1
      ldb = n
      ldx = n
      Allocate (ab(ldab,n), b(ldb,nrhs), work(2*n), x(ldx,n), berr(nrhs), &
        ferr(nrhs), rwork(n))

!     Read A and B from data file, and copy B to X

      Read (nin, *) uplo
      If (uplo=='U') Then
        Do i = 1, n
          Read (nin, *)(ab(kd+1+i-j,j), j=i, min(n,i+kd))
        End Do
      Else If (uplo=='L') Then
        Do i = 1, n
          Read (nin, *)(ab(1+i-j,j), j=max(1,i-kd), i)
        End Do
      End If
      Read (nin, *)(b(i,1:nrhs), i=1, n)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Compute solution in the array X
      Call ztbtrs(uplo, trans, diag, n, kd, nrhs, ab, ldab, x, ldx, info)

!     Compute backward errors and estimated bounds on the
!     forward errors

      Call ztbrfs(uplo, trans, diag, n, kd, nrhs, ab, ldab, b, ldb, x, ldx, &
        ferr, berr, work, rwork, info)

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
