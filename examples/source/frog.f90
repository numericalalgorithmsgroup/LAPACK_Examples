    Program zupmtr_example

!     ZUPMTR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: dstebz, zhptrd, zstein, zupmtr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Real (Kind=dp) :: vl, vu
      Integer :: i, ifail, info, j, k, ldc, m, n, nsplit
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ap(:), c(:, :), tau(:), work(:)
      Real (Kind=dp), Allocatable :: d(:), e(:), rwork(:), w(:)
      Integer, Allocatable :: iblock(:), ifailv(:), isplit(:), iwork(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, conjg, maxloc
!     .. Executable Statements ..
      Write (nout, *) 'ZUPMTR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldc = n
      Allocate (ap(n*(n+1)/2), c(ldc,n), tau(n), work(n), d(n), e(n), rwork(5* &
        n), w(n), iblock(n), ifailv(n), isplit(n), iwork(3*n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

!     Reduce A to tridiagonal form T = (Q**H)*A*Q
      Call zhptrd(uplo, n, ap, d, e, tau, info)

!     Calculate the two smallest eigenvalues of T (same as A)

      Call dstebz('I', 'B', n, vl, vu, 1, 2, zero, d, e, m, nsplit, w, iblock, &
        isplit, rwork, iwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else
        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:m)

!       Calculate the eigenvectors of T, storing the result in C
        Call zstein(n, d, e, m, w, iblock, isplit, c, ldc, rwork, iwork, &
          ifailv, info)

        If (info>0) Then
          Write (nout, *) 'Failure to converge.'
        Else

!         Calculate the eigenvectors of A = Q * (eigenvectors of T)
          Call zupmtr('Left', uplo, 'No transpose', n, m, ap, tau, c, ldc, &
            work, info)

!         Print eigenvectors

          Write (nout, *)
          Flush (nout)

!         Normalize the eigenvectors, largest element real
          Do i = 1, m
            rwork(1:n) = abs(c(1:n,i))
            k = maxloc(rwork(1:n), 1)
            scal = conjg(c(k,i))/abs(c(k,i))/dznrm2(n, c(1,i), 1)
            c(1:n, i) = c(1:n, i)*scal
          End Do

!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
          ifail = 0
          Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, m, &
            c, ldc, 'Bracketed', 'F7.4', 'Eigenvectors', 'Integer', rlabs, &
            'Integer', clabs, 80, 0, ifail)

        End If
      End If

100   Format (8X, 4(F7.4,11X,:))
    End Program
