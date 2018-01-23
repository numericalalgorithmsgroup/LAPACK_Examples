    Program zpbtrs_example

!     ZPBTRS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zpbtrf, zpbtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, ldb, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), b(:, :)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZPBTRS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd, nrhs
      ldab = kd + 1
      ldb = n
      Allocate (ab(ldab,n), b(ldb,nrhs))

!     Read A and B from data file

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

!     Factorize A
      Call zpbtrf(uplo, n, kd, ab, ldab, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution
        Call zpbtrs(uplo, n, kd, nrhs, ab, ldab, b, ldb, info)

!       Print solution

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, nrhs, &
          b, ldb, 'Bracketed', 'F7.4', 'Solution(s)', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      Else
        Write (nout, *) 'A is not positive definite'
      End If

    End Program
