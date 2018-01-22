    Program dormhr_example

!     DORMHR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgehrd, dhsein, dhseqr, dormhr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: eig, eig1
      Real (Kind=dp) :: r, thresh
      Integer :: i, ifail, info, j, k, l, lda, ldc, ldh, ldvl, ldz, lwork, m, &
        n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), c(:, :), h(:, :), tau(:), &
        vl(:, :), wi(:), work(:), wr(:), z(:, :)
      Integer, Allocatable :: ifaill(:), ifailr(:)
      Logical, Allocatable :: select(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: aimag, cmplx, conjg, maxloc, real, sqrt, sum
!     .. Executable Statements ..
      Write (nout, *) 'DORMHR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = 1
      lda = n
      ldc = n
      ldh = n
      ldvl = n
      lwork = 64*n
      Allocate (a(lda,n), c(ldc,n), h(ldh,n), tau(n), vl(ldvl,n), wi(n), &
        work(lwork), wr(n), z(ldz,1), ifaill(n), ifailr(n), select(n))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

      Read (nin, *) thresh

!     Reduce A to upper Hessenberg form H = (Q**T)*A*Q
      Call dgehrd(n, 1, n, a, lda, tau, work, lwork, info)

!     Copy A to H
      h(1:n, 1:n) = a(1:n, 1:n)

!     Calculate the eigenvalues of H (same as A)
      Call dhseqr('Eigenvalues', 'No vectors', n, 1, n, h, ldh, wr, wi, z, &
        ldz, work, lwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else
        Write (nout, *) 'Eigenvalues'
        Write (nout, 100)(' (', wr(i), ',', wi(i), ')', i=1, n)

        Do i = 1, n
          select(i) = wr(i) < thresh
        End Do

!       Calculate the eigenvectors of H (as specified by SELECT),
!       storing the result in C
        Call dhsein('Right', 'QR', 'No initial vectors', select, n, a, lda, &
          wr, wi, vl, ldvl, c, ldc, n, m, work, ifaill, ifailr, info)

!       Calculate the eigenvectors of A = Q * (eigenvectors of H)
        Call dormhr('Left', 'No transpose', n, m, 1, n, a, lda, tau, c, ldc, &
          work, lwork, info)

!       Print eigenvectors

        Write (nout, *)
        Flush (nout)

!       Normalize selected eigenvectors
        j = 0
        k = 1
        Do While (k<=n)
          If (select(k)) Then
            j = j + 1
            If (wi(k)==0.0_dp) Then
!             Normalize real eigenvector by making largest positive
              Call nagf_blas_damax_val(n, c(1,j), 1, l, r)
              r = dnrm2(n, c(1,j), 1)
              c(1:n, j) = c(1:n, j)/r
              If (c(l,j)<zero) Then
                c(1:n, j) = -c(1:n, j)
              End If
            Else
!             Normalize complex eigenvectors making largest element real
              work(1:n) = c(1:n, j)**2 + c(1:n, j+1)**2
              l = maxloc(work(1:n), 1)
              eig1 = cmplx(c(l,j), c(l,j+1), kind=dp)
              eig1 = conjg(eig1)/sqrt(work(l)*sum(work(1:n)))
              Do i = 1, n
                eig = cmplx(c(i,j), c(i,j+1), kind=dp)
                eig = eig*eig1
                c(i, j) = real(eig)
                c(i, j+1) = aimag(eig)
              End Do
              c(l, j+1) = 0.0_dp
              j = j + 1
              k = k + 1
            End If
          End If
          k = k + 1
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, m, c, ldc, &
          'Contents of array C', ifail)

      End If

100   Format (1X, A, F8.4, A, F8.4, A)
    End Program
