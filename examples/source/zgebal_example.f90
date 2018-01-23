    Program zgebal_example

!     ZGEBAL Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use blas_interfaces, Only: dznrm2
      Use lapack_example_aux, Only: nagf_sort_realvec_rank, &
        nagf_file_print_matrix_complex_gen_comp, &
        nagf_sort_cmplxvec_rank_rearrange
      Use lapack_interfaces, Only: zgebak, zgebal, zgehrd, zhseqr, ztrevc, &
        zunghr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Complex (Kind=dp) :: scal
      Integer :: i, ifail, ihi, ilo, info, k, lda, ldh, ldvl, ldvr, lwork, m, &
        n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), h(:, :), tau(:), vl(:, :), &
        vr(:, :), vr_row(:), w(:), work(:)
      Real (Kind=dp), Allocatable :: rwork(:), scale(:)
      Integer, Allocatable :: irank(:)
      Logical :: select(1)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, aimag, conjg, maxloc, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGEBAL Example Program Results'
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldvl = 1
      lda = n
      ldh = n
      ldvr = n
      lwork = 64*n
      Allocate (a(lda,n), h(ldh,n), tau(n), vl(ldvl,1), vr(ldvr,n), w(n), &
        work(lwork), rwork(n), scale(n), irank(n))

!     Read A from data file
      Read (nin, *)(a(i,1:n), i=1, n)

!     Balance A
      Call zgebal('Both', n, a, lda, ilo, ihi, scale, info)

!     Reduce A to upper Hessenberg form H = (Q**H)*A*Q
      Call zgehrd(n, ilo, ihi, a, lda, tau, work, lwork, info)

!     Copy A to H and VR
      h(1:n, 1:n) = a(1:n, 1:n)
      vr(1:n, 1:n) = a(1:n, 1:n)

!     Form Q explicitly, storing the result in VR
      Call zunghr(n, 1, n, vr, ldvr, tau, work, lwork, info)

!     Calculate the eigenvalues and Schur factorization of A
      Call zhseqr('Schur form', 'Vectors', n, ilo, ihi, h, ldh, w, vr, ldvr, &
        work, lwork, info)

      Write (nout, *)
      If (info>0) Then
        Write (nout, *) 'Failure to converge.'
      Else
!       Sort eigenvalues into descending absolute value
        rwork(1:n) = abs(w(1:n))
!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_sort_realvec_rank(rwork, 1, n, 'Descending', irank, ifail)
!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_sort_cmplxvec_rank_rearrange(w, 1, n, irank, ifail)

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100)(' (', real(w(i)), ',', aimag(w(i)), ')', i=1, n)
        Flush (nout)

!       Calculate the eigenvectors of A, storing the result in VR
        Call ztrevc('Right', 'Backtransform', select, n, h, ldh, vl, ldvl, vr, &
          ldvr, n, m, work, rwork, info)

        Call zgebak('Both', 'Right', n, ilo, ihi, scale, m, vr, ldvr, info)

!       Reorder eigenvectors using irank
        Allocate (vr_row(n))
        Do i = 1, n
          vr_row(1:n) = vr(i, 1:n)
!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
          ifail = 0
          Call nagf_sort_cmplxvec_rank_rearrange(vr_row, 1, n, irank, ifail)
          vr(i, 1:n) = vr_row(1:n)
        End Do
        Deallocate (vr_row)

!       Print eigenvectors

        Write (nout, *)
        Flush (nout)

!       Normalize the eigenvectors, largest element real
        Do i = 1, m
          rwork(1:n) = abs(vr(1:n,i))
          k = maxloc(rwork(1:n), 1)
          scal = conjg(vr(k,i))/abs(vr(k,i))/dznrm2(n, vr(1,i), 1)
          vr(1:n, i) = vr(1:n, i)*scal
        End Do

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, m, vr, &
          ldvr, 'Bracketed', 'F7.4', 'Contents of array VR', 'Integer', rlabs, &
          'Integer', clabs, 80, 0, ifail)

      End If

100   Format ((3X,4(A,F7.4,A,F7.4,A,:)))
    End Program
