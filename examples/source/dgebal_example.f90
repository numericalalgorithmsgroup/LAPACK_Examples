    Program dgebal_example

!     DGEBAL Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dnrm2
      Use lapack_example_aux, Only: nagf_blas_damax_val, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgebak, dgebal, dgehrd, dhseqr, dorghr, &
        dtrevc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: r
      Integer :: i, ifail, ihi, ilo, info, k, lda, ldh, ldvl, ldvr, lwork, m, &
        n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), h(:, :), scale(:), tau(:), &
        vl(:, :), vr(:, :), wi(:), work(:), wr(:)
      Logical :: select(1)
!     .. Executable Statements ..
      Write (nout, *) 'DGEBAL Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldvl = 1
      lda = n
      ldh = n
      ldvr = n
      lwork = 64*n
      Allocate (a(lda,n), h(ldh,n), scale(n), tau(n), vl(ldvl,1), vr(ldvr,n), &
        wi(n), work(lwork), wr(n))

!     Read A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     Balance A
      Call dgebal('Both', n, a, lda, ilo, ihi, scale, info)

!     Reduce A to upper Hessenberg form H = (Q**T)*A*Q
      Call dgehrd(n, ilo, ihi, a, lda, tau, work, lwork, info)

!     Copy A to H and VR
      h(1:n, 1:n) = a(1:n, 1:n)
      vr(1:n, 1:n) = a(1:n, 1:n)

!     Form Q explicitly, storing the result in VR
      Call dorghr(n, 1, n, vr, ldvr, tau, work, lwork, info)

!     Calculate the eigenvalues and Schur factorization of A
      Call dhseqr('Schur form', 'Vectors', n, ilo, ihi, h, ldh, wr, wi, vr, &
        ldvr, work, lwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else
        Write (nout, *) 'Eigenvalues'
        Write (nout, 100)(' (', wr(i), ',', wi(i), ')', i=1, n)

!       Calculate the eigenvectors of A, storing the result in VR

        Call dtrevc('Right', 'Backtransform', select, n, h, ldh, vl, ldvl, vr, &
          ldvr, n, m, work, info)

        Call dgebak('Both', 'Right', n, ilo, ihi, scale, m, vr, ldvr, info)

!       Print eigenvectors

        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors, largest positive
        Do i = 1, m
          Call nagf_blas_damax_val(n, vr(1,i), 1, k, r)
          If (vr(k,i)<zero) Then
            vr(1:n, i) = -vr(1:n, i)
          End If
          r = dnrm2(n, vr(1,i), 1)
          vr(1:n, i) = vr(1:n, i)/r
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, m, vr, ldvr, &
          'Contents of array VR', ifail)

      End If

100   Format (1X, A, F8.4, A, F8.4, A)
    End Program
