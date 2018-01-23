    Program dgbsvx_example

!     DGBSVX Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgbsvx
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: rcond
      Integer :: i, ifail, info, j, k, kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
      Character (1) :: equed
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), afb(:, :), b(:, :), berr(:), &
        c(:), ferr(:), r(:), work(:), x(:, :)
      Integer, Allocatable :: ipiv(:), iwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DGBSVX Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs, kl, ku
      ldb = n
      ldx = n
      ldab = kl + ku + 1
      ldafb = ldab + kl
      Allocate (ab(ldab,n), afb(ldafb,n), b(ldb,nrhs), berr(nrhs), c(n), &
        ferr(nrhs), r(n), work(3*n), x(ldx,nrhs), ipiv(n), iwork(n))

!     Read the band matrix A and B from data file

      k = ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, n)
      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Solve the equations AX = B for X
      Call dgbsvx('Equilibration', 'No transpose', n, kl, ku, nrhs, ab, ldab, &
        afb, ldafb, ipiv, equed, r, c, b, ldb, x, ldx, rcond, ferr, berr, &
        work, iwork, info)

      If ((info==0) .Or. (info==n+1)) Then

!       Print solution, error bounds, condition number, the form
!       of equilibration and the pivot growth factor

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
        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal condition number'
        Write (nout, 100) rcond
        Write (nout, *)
        If (equed=='N') Then
          Write (nout, *) 'A has not been equilibrated'
        Else If (equed=='R') Then
          Write (nout, *) 'A has been row scaled as diag(R)*A'
        Else If (equed=='C') Then
          Write (nout, *) 'A has been column scaled as A*diag(C)'
        Else If (equed=='B') Then
          Write (nout, *) &
            'A has been row and column scaled as diag(R)*A*diag(C)'
        End If
        Write (nout, *)
        Write (nout, *) 'Estimate of reciprocal pivot growth factor'
        Write (nout, 100) work(1)

        If (info==n+1) Then
          Write (nout, *)
          Write (nout, *) 'The matrix A is singular to working precision'
        End If
      Else
        Write (nout, 110) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format ((3X,1P,7E11.1))
110   Format (1X, A, I3, A, I3, A, A)
    End Program
