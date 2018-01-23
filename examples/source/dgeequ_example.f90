    Program dgeequ_example

!     DGEEQU Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dscal
      Use lapack_example_aux, Only: nagf_blas_ddscl, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgeequ
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: one = 1.0_dp
      Real (Kind=dp), Parameter :: thresh = 0.1_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: amax, big, colcnd, rowcnd, small
      Integer :: i, ifail, info, j, lda, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), c(:), r(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: epsilon, radix, real, tiny
!     .. Executable Statements ..
      Write (nout, *) 'DGEEQU Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      Allocate (a(lda,n), c(n), r(n))

!     Read the N by N matrix A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Print the matrix A

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Matrix A', ifail)
      Write (nout, *)

!     Compute row and column scaling factors

      Call dgeequ(n, n, a, lda, r, c, rowcnd, colcnd, amax, info)

      If (info>0) Then
        If (info<=n) Then
          Write (nout, 100) 'Row ', info, ' of A is exactly zero'
        Else
          Write (nout, 100) 'Column ', info - n, ' of A is exactly zero'
        End If
      Else

!       Print ROWCND, COLCND, AMAX and the scale factors

        Write (nout, 110) 'ROWCND =', rowcnd, ', COLCND =', colcnd, &
          ', AMAX =', amax
        Write (nout, *)
        Write (nout, *) 'Row scale factors'
        Write (nout, 120) r(1:n)
        Write (nout, *)
        Write (nout, *) 'Column scale factors'
        Write (nout, 120) c(1:n)
        Write (nout, *)
        Flush (nout)

!       Compute values close to underflow and overflow

        small = tiny(1.0E0_dp)/(epsilon(1.0E0_dp)*real(radix(1.0E0_dp),kind=dp &
          ))
        big = one/small
        If ((rowcnd>=thresh) .And. (amax>=small) .And. (amax<=big)) Then
          If (colcnd<thresh) Then

!           Just column scale A
            Do j = 1, n
              Call dscal(n, c(j), a(1,j), 1)
            End Do

          End If
        Else If (colcnd>=thresh) Then

!         Just row scale A
          Do j = 1, n
            Call nagf_blas_ddscl(n, r, 1, a(1,j), 1)
          End Do

        Else

!         Row and column scale A
          Do j = 1, n
            Call dscal(n, c(j), a(1,j), 1)
            Call nagf_blas_ddscl(n, r, 1, a(1,j), 1)
          End Do

        End If

!       Print the scaled matrix
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
          'Scaled matrix', ifail)

      End If

100   Format (1X, A, I4, A)
110   Format (1X, 3(A,1P,E8.1))
120   Format ((1X,1P,7E11.2))
    End Program
