    Program dpbtrf_example

!     DPBTRF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_band
      Use lapack_interfaces, Only: dpbtrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'DPBTRF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      Allocate (ab(ldab,n))

!     Read A from data file

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

!     Factorize A
      Call dpbtrf(uplo, n, kd, ab, ldab, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Print factor

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0

        If (uplo=='U') Then

          Call nagf_file_print_matrix_real_band(n, n, 0, kd, ab, ldab, &
            'Factor', ifail)

        Else If (uplo=='L') Then

          Call nagf_file_print_matrix_real_band(n, n, kd, 0, ab, ldab, &
            'Factor', ifail)

        End If

      Else
        Write (nout, *) 'A is not positive definite'
      End If

    End Program
