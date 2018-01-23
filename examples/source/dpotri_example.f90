    Program dpotri_example

!     DPOTRI Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dpotrf, dpotri
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :)
!     .. Executable Statements ..
      Write (nout, *) 'DPOTRI Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      Allocate (a(lda,n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If

!     Factorize A
      Call dpotrf(uplo, n, a, lda, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute inverse of A
        Call dpotri(uplo, n, a, lda, info)

!       Print inverse

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen(uplo, 'Nonunit', n, n, a, lda, &
          'Inverse', ifail)

      Else
        Write (nout, *) 'A is not positive definite'
      End If

    End Program
