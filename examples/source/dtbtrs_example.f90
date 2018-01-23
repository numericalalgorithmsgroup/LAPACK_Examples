    Program dtbtrs_example

!     DTBTRS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dtbtrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: diag = 'N', trans = 'N'
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, ldb, n, nrhs
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), b(:, :)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DTBTRS Example Program Results'
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

!     Compute solution
      Call dtbtrs(uplo, trans, diag, n, kd, nrhs, ab, ldab, b, ldb, info)

!     Print solution

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, b, ldb, &
          'Solution(s)', ifail)

      Else
        Write (nout, *) 'A is singular'
      End If

    End Program
