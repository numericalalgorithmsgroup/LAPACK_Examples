    Program zgbrfs_example

!     ZGBRFS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgbrfs, zgbtrf, zgbtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: zero = (0.0_dp, 0.0_dp)
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, k, kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), afb(:, :), b(:, :), work(:), &
        x(:, :)
      Real (Kind=dp), Allocatable :: berr(:), ferr(:), rwork(:)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZGBRFS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs, kl, ku
      ldb = n
      ldx = n
      ldab = kl + ku + 1
      ldafb = 2*kl + ku + 1
      Allocate (ab(ldab,n), afb(ldafb,n), b(ldb,nrhs), work(2*n), x(ldx,n), &
        berr(nrhs), ferr(nrhs), rwork(n), ipiv(n))

!     Set A to zero to avoid referencing uninitialized elements

      ab(1:kl+ku+1, 1:n) = zero

!     Read A and B from data file, and copy A to AFB and B to X

      k = ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, n)
      Read (nin, *)(b(i,1:nrhs), i=1, n)

      afb(kl+1:2*kl+ku+1, 1:n) = ab(1:kl+ku+1, 1:n)
      x(1:n, 1:nrhs) = b(1:n, 1:nrhs)

!     Factorize A in the array AFB
      Call zgbtrf(n, n, kl, ku, afb, ldafb, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution in the array X
        Call zgbtrs(trans, n, kl, ku, nrhs, afb, ldafb, ipiv, x, ldx, info)

!       Improve solution, and compute backward errors and
!       estimated bounds on the forward errors
        Call zgbrfs(trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv, b, &
          ldb, x, ldx, ferr, berr, work, rwork, info)

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
