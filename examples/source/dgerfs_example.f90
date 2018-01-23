    Program dgerfs_example

!     DGERFS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgerfs, dgetrf, dgetrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldaf, ldb, ldx, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), af(:, :), b(:, :), berr(:), &
        ferr(:), work(:), x(:, :)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Executable Statements ..
      Write (nout, *) 'DGERFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      lda = n
      ldaf = n
      ldb = n
      ldx = n
      Allocate (a(lda,n), af(ldaf,n), b(ldb,nrhs), berr(nrhs), ferr(nrhs), &
        work(3*n), x(ldx,n), ipiv(n), iwork(n))

!     Read A and B from data file, and copy A to AF and B to X

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:nrhs), i=1, n)

      af(1:n, 1:n) = a(1:n, 1:n)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize A in the array AF

      Call dgetrf(n, n, af, ldaf, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution in the array X

        Call dgetrs(trans, n, nrhs, af, ldaf, ipiv, x, ldx, info)

!       Improve solution, and compute backward errors and
!       estimated bounds on the forward errors

        Call dgerfs(trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, ldx, &
          ferr, berr, work, iwork, info)

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
        Write (nout, *) 'The factor U is singular'
      End If

100   Format ((3X,1P,7E11.1))
    End Program
