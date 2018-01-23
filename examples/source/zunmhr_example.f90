    Program zunmhr_example

!     ZUNMHR Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgehrd, zhsein, zhseqr, zunmhr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Real (Kind=dp) :: thresh
      Integer :: i, ifail, info, k, lda, ldc, ldh, ldvl, ldz, lwork, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), c(:, :), h(:, :), tau(:), &
        vl(:, :), w(:), work(:), z(:, :)
      Real (Kind=dp), Allocatable :: rwork(:)
      Integer, Allocatable :: ifaill(:), ifailr(:)
      Logical, Allocatable :: select(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, aimag, conjg, maxloc, real
!     .. Executable Statements ..
      Write (nout, *) 'ZUNMHR Example Program Results'
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldz = 1
      lda = n
      ldc = n
      ldh = n
      ldvl = n
      lwork = 64*n
      Allocate (a(lda,n), c(ldc,n), h(ldh,n), tau(n), vl(ldvl,n), w(n), &
        work(lwork), z(ldz,1), rwork(n), ifaill(n), ifailr(n), select(n))

!     Read A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

      Read (nin, *) thresh

!     Reduce A to upper Hessenberg form H = (Q**H)*A*Q
      Call zgehrd(n, 1, n, a, lda, tau, work, lwork, info)

!     Copy A to H
      h(1:n, 1:n) = a(1:n, 1:n)

!     Calculate the eigenvalues of H (same as A)
      Call zhseqr('Eigenvalues', 'No vectors', n, 1, n, h, ldh, w, z, ldz, &
        work, lwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else
        Write (nout, *) 'Eigenvalues'
        Write (nout, 100)(' (', real(w(i)), ',', aimag(w(i)), ')', i=1, n)
        Flush (nout)

        Do i = 1, n
          select(i) = real(w(i)) < thresh
        End Do

!       Calculate the eigenvectors of H (as specified by SELECT),
!       storing the result in C
        Call zhsein('Right', 'QR', 'No initial vectors', select, n, a, lda, w, &
          vl, ldvl, c, ldc, n, m, work, rwork, ifaill, ifailr, info)

!       Calculate the eigenvectors of A = Q * (eigenvectors of H)
        Call zunmhr('Left', 'No transpose', n, m, 1, n, a, lda, tau, c, ldc, &
          work, lwork, info)

!       Print eigenvectors

        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors, largest element real
        Do i = 1, m
          rwork(1:n) = abs(c(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(c(k,i))/abs(c(k,i))/dznrm2(n, c(1,i), 1)
          c(1:n, i) = c(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, m, c, &
          ldc, 'Bracketed', 'F7.4', 'Contents of array C', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      End If

100   Format ((3X,4(A,F7.4,A,F7.4,A,:)))
    End Program
