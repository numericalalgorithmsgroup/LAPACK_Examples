    Program dpbequ_example

!     DPBEQU Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dscal
      Use lapack_example_aux, Only: nagf_blas_ddscl, &
        nagf_file_print_matrix_real_band
      Use lapack_interfaces, Only: dpbequ
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0_dp
      Real (Kind=dp), Parameter :: thresh = 0.1_dp
      Integer, Parameter :: nin = 5, nout = 6
      Character (1), Parameter :: uplo = 'U'
!     .. Local Scalars ..
      Real (Kind=dp) :: amax, big, scond, small
      Integer :: i, i0, i1, ifail, ilen, info, j, kd, ldab, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ab(:, :), s(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, max, min, radix, real, tiny
!     .. Executable Statements ..
      Write (nout, *) 'DPBEQU Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kd
      ldab = kd + 1
      Allocate (ab(ldab,n), s(n))

!     Read the upper or lower triangular part of the band matrix A
!     from data file

      If (uplo=='U') Then
        Do i = 1, n
          Read (nin, *)(ab(kd+1+i-j,j), j=i, min(n,i+kd))
        End Do
      Else If (uplo=='L') Then
        Do i = 1, n
          Read (nin, *)(ab(1+i-j,j), j=max(1,i-kd), i)
        End Do
      End If

!     Print the matrix A

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      If (uplo=='U') Then
        Call nagf_file_print_matrix_real_band(n, n, 0, kd, ab, ldab, &
          'Matrix A', ifail)
      Else If (uplo=='L') Then
        Call nagf_file_print_matrix_real_band(n, n, kd, 0, ab, ldab, &
          'Matrix A', ifail)
      End If

      Write (nout, *)

!     Compute diagonal scaling factors
      Call dpbequ(uplo, n, kd, ab, ldab, s, scond, amax, info)

      If (info>0) Then
        Write (nout, 100) 'Diagonal element', info, ' of A is non positive'
      Else

!       Print SCOND, AMAX and the scale factors

        Write (nout, 110) 'SCOND =', scond, ', AMAX =', amax
        Write (nout, *)
        Write (nout, *) 'Diagonal scaling factors'
        Write (nout, 120) s(1:n)
        Write (nout, *)
        Flush (nout)

!       Compute values close to underflow and overflow

        small = tiny(1.0E0_dp)/(epsilon(1.0E0_dp)*real(radix(1.0E0_dp),kind=dp &
          ))
        big = one/small
        If ((scond<thresh) .Or. (amax<small) .Or. (amax>big)) Then

!         Scale A
          If (uplo=='U') Then

            Do j = 1, n
              i0 = max(1, j-kd)
              i1 = 1 + i0 - (j-kd)
              ilen = j - i0 + 1
              Call dscal(ilen, s(j), ab(i1,j), 1)
              Call nagf_blas_ddscl(ilen, s(i0), 1, ab(i1,j), 1)
            End Do

          Else If (uplo=='L') Then
            Do j = 1, n
              i1 = 1
              ilen = min(n, j+kd) - j + 1
              Call dscal(ilen, s(j), ab(i1,j), 1)
              Call nagf_blas_ddscl(ilen, s(j), 1, ab(i1,j), 1)
            End Do
          End If

!         Print the scaled matrix

          ifail = 0
          If (uplo=='U') Then
            Call nagf_file_print_matrix_real_band(n, n, 0, kd, ab, ldab, &
              'Scaled matrix', ifail)
          Else If (uplo=='L') Then
            Call nagf_file_print_matrix_real_band(n, n, kd, 0, ab, ldab, &
              'Scaled matrix', ifail)
          End If
        End If
      End If

100   Format (1X, A, I4, A)
110   Format (1X, 2(A,1P,E8.1))
120   Format ((1X,1P,7E11.1))
    End Program
