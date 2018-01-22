    Program dgbtrs_example

!     DGBTRS Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgbtrf, dgbtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, k, kl, ku, ldab, ldb, n, nrhs
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), b(:, :)
      Integer, Allocatable :: ipiv(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DGBTRS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs, kl, ku
      ldab = 2*kl + ku + 1
      ldb = n
      Allocate (ab(ldab,n), b(ldb,nrhs), ipiv(n))

!     Read A and B from data file

      k = kl + ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, n)
      Read (nin, *)(b(i,1:nrhs), i=1, n)

!     Factorize A
      Call dgbtrf(n, n, kl, ku, ab, ldab, ipiv, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution
        Call dgbtrs(trans, n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)

!       Print solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, b, ldb, &
          'Solution(s)', ifail)

      Else
        Write (nout, *) 'The factor U is singular'
      End If

    End Program
