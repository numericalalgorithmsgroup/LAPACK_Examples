    Program dppequ_example

!     DPPEQU Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dscal
      Use lapack_example_aux, Only: nagf_blas_ddscl, &
        nagf_file_print_matrix_real_packed
      Use lapack_interfaces, Only: dppequ
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
      Integer :: i, ifail, info, j, jinc, jj, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ap(:), s(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, radix, real, tiny
!     .. Executable Statements ..
      Write (nout, *) 'DPPEQU Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap((n*(n+1))/2), s(n))

!     Read the upper or lower triangular part of the matrix A from
!     data file

      If (uplo=='U') Then
        Read (nin, *)((ap(i+(j*(j-1))/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+((2*n-j)*(j-1))/2),j=1,i), i=1, n)
      End If

!     Print the matrix A

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_packed(uplo, 'Non-unit diagonal', n, &
        ap, 'Matrix A', ifail)

      Write (nout, *)

!     Compute diagonal scaling factors
      Call dppequ(uplo, n, ap, s, scond, amax, info)

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
            jj = 1
            Do j = 1, n
              Call dscal(j, s(j), ap(jj), 1)
              Call nagf_blas_ddscl(j, s, 1, ap(jj), 1)
              jj = jj + j
            End Do
          Else If (uplo=='L') Then
            jj = 1
            jinc = n
            Do j = 1, n
              Call dscal(jinc, s(j), ap(jj), 1)
              Call nagf_blas_ddscl(jinc, s(j), 1, ap(jj), 1)
              jj = jj + jinc
              jinc = jinc - 1
            End Do
          End If

!         Print the scaled matrix

          ifail = 0
          Call nagf_file_print_matrix_real_packed(uplo, 'Non-unit diagonal', &
            n, ap, 'Scaled matrix', ifail)

        End If
      End If

100   Format (1X, A, I4, A)
110   Format (1X, 2(A,1P,E8.1))
120   Format ((1X,1P,7E11.1))
    End Program
