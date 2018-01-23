    Program zgerfs_example

!     ZGERFS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgerfs, zgetrf, zgetrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldaf, ldb, ldx, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), af(:, :), b(:, :), work(:), &
        x(:, :)
      Real (Kind=dp), Allocatable :: berr(:), ferr(:), rwork(:)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZGERFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs
      lda = n
      ldaf = n
      ldb = n
      ldx = n
      Allocate (a(lda,n), af(ldaf,n), b(ldb,nrhs), work(2*n), x(ldx,n), &
        berr(nrhs), ferr(nrhs), rwork(n), ipiv(n))

!     Read A and B from data file, and copy A to AF and B to X

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:nrhs), i=1, n)

      af(1:n, 1:n) = a(1:n, 1:n)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize A in the array AF
      Call zgetrf(n, n, af, ldaf, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution in the array X

        Call zgetrs(trans, n, nrhs, af, ldaf, ipiv, x, ldx, info)

!       Improve solution, and compute backward errors and
!       estimated bounds on the forward errors

        Call zgerfs(trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, ldx, &
          ferr, berr, work, rwork, info)

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
        Write (nout, *) 'The factor U is singular'
      End If

100   Format ((5X,1P,4(E11.1,7X)))
    End Program
