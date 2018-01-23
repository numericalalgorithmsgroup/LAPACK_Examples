    Program zgeev_example

!     ZGEEV Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen
      Use lapack_interfaces, Only: zgeev
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nb = 64, nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, ldvr, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), vr(:, :), w(:), work(:)
      Complex (Kind=dp) :: dummy(1, 1)
      Real (Kind=dp), Allocatable :: rwork(:)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, nint, real
!     .. Executable Statements ..
      Write (nout, *) 'ZGEEV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldvr = n
      Allocate (a(lda,n), vr(ldvr,n), w(n), rwork(2*n))

!     Use routine workspace query to get optimal workspace.
      lwork = -1
      Call zgeev('No left vectors', 'Vectors (right)', n, a, lda, w, dummy, 1, &
        vr, ldvr, dummy, lwork, rwork, info)

!     Make sure that there is enough workspace for block size nb.
      lwork = max((nb+1)*n, nint(real(dummy(1,1))))
      Allocate (work(lwork))

!     Read the matrix A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Compute the eigenvalues and right eigenvectors of A

      Call zgeev('No left vectors', 'Vectors (right)', n, a, lda, w, dummy, 1, &
        vr, ldvr, work, lwork, rwork, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Eigenvalues'
        Write (nout, 100) w(1:n)

        Write (nout, *)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_gen('General', ' ', n, n, vr, &
          ldvr, 'Eigenvectors', ifail)

      Else
        Write (nout, *)
        Write (nout, 110) 'Failure in ZGEEV.  INFO = ', info
      End If

100   Format ((3X,4(' (',F7.4,',',F7.4,')',:)))
110   Format (1X, A, I4)
    End Program
