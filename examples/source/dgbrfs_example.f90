    Program dgbrfs_example

!     DGBRFS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgbrfs, dgbtrf, dgbtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, k, kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), afb(:, :), b(:, :), berr(:), &
        ferr(:), work(:), x(:, :)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DGBRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs, kl, ku
      ldab = kl + ku + 1
      ldafb = 2*kl + ku + 1
      ldb = n
      ldx = n
      Allocate (ab(ldab,n), afb(ldafb,n), b(ldb,nrhs), berr(nrhs), ferr(nrhs), &
        work(3*n), x(ldx,n), ipiv(n), iwork(n))

!     Set A to zero to avoid referencing uninitialized elements

      ab(1:kl+ku+1, 1:n) = zero

!     Read A and B from data file, and copy A to AFB and B to X

      k = ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, n)
      Read (nin, *)(b(i,1:nrhs), i=1, n)

      afb(kl+1:2*kl+ku+1, 1:n) = ab(1:kl+ku+1, 1:n)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize A in the array AFB
      Call dgbtrf(n, n, kl, ku, afb, ldafb, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution in the array X
        Call dgbtrs(trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx, info)

!       Improve solution, and compute backward errors and
!       estimated bounds on the forward errors

        Call dgbrfs(trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv, b, &
          ldb, x, ldx, ferr, berr, work, iwork, info)

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
