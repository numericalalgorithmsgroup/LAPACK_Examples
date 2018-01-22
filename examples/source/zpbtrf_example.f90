    Program zpbtrf_example

!     ZPBTRF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_band_comp
      Use lapack_interfaces, Only: zpbtrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, kd, ldab, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZPBTRF Example Program Results'
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
      Call zpbtrf(uplo, n, kd, ab, ldab, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Print factor

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        If (uplo=='U') Then

          Call nagf_file_print_matrix_complex_band_comp(n, n, 0, kd, ab, ldab, &
            'Bracketed', 'F7.4', 'Factor', 'Integer', rlabs, 'Integer', clabs, &
            80, 0, ifail)

        Else If (uplo=='L') Then

          Call nagf_file_print_matrix_complex_band_comp(n, n, kd, 0, ab, ldab, &
            'Bracketed', 'F7.4', 'Factor', 'Integer', rlabs, 'Integer', clabs, &
            80, 0, ifail)

        End If

      Else
        Write (nout, *) 'A is not positive definite'
      End If

    End Program
