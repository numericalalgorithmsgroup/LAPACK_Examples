    Program zpteqr_example

!     ZPTEQR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zhetrd, zlacpy, zpteqr, zungtr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, info, k, lda, ldz, lwork, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), tau(:), work(:), z(:, :)
      Real (Kind=dp), Allocatable :: d(:), e(:), rwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, conjg, maxloc
!     .. Executable Statements ..
      Write (nout, *) 'ZPTEQR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldz = n
      lwork = 64*n
      Allocate (a(lda,n), tau(n), work(lwork), z(ldz,n), d(n), e(n), &
        rwork(4*n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If

!     Reduce A to tridiagonal form T = (Q**H)*A*Q

      Call zhetrd(uplo, n, a, lda, d, e, tau, work, lwork, info)

!     Copy A into Z

      Call zlacpy(uplo, n, n, a, lda, z, ldz)

!     Form Q explicitly, storing the result in Z

      Call zungtr(uplo, n, z, ldz, tau, work, lwork, info)

!     Calculate all the eigenvalues and eigenvectors of A

      Call zpteqr('V', n, d, e, z, ldz, rwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else

!       Print eigenvalues and eigenvectors

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) d(1:n)
        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors, largest element real
        Do i = 1, n
          rwork(1:n) = abs(z(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(z(k,i))/abs(z(k,i))/dznrm2(n, z(1,i), 1)
          z(1:n, i) = z(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, z, &
          ldz, 'Bracketed', 'F7.4', 'Eigenvectors', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      End If

100   Format (8X, 4(F7.4,11X,:))
    End Program
