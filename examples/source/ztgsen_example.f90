    Program ztgsen_example

!     ZTGSEN Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: ztgsen
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: pl, pr
      Integer :: i, ifail, ijob, info, lda, ldb, ldq, ldz, liwork, lwork, m, n
      Logical :: wantq, wantz
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), alpha(:), b(:, :), beta(:), &
        q(:, :), work(:), z(:, :)
      Real (Kind=dp) :: dif(2)
      Integer, Allocatable :: iwork(:)
      Logical, Allocatable :: select(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZTGSEN Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      ldb = n
      ldq = n
      ldz = n
      liwork = (n*n)/2 + 2
      lwork = n*n
      Allocate (a(lda,n), alpha(n), b(ldb,n), beta(n), q(ldq,n), work(lwork), &
        z(ldz,n), iwork(liwork), select(n))

!     Read A, B, Q, Z and the logical array SELECT from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)
      Read (nin, *)(q(i,1:n), i=1, n)
      Read (nin, *)(z(i,1:n), i=1, n)

      Read (nin, *) select(1:n)

!     Set ijob, wantq and wantz
      ijob = 4
      wantq = .True.
      wantz = .True.

!     Reorder the Schur factors A and B and update the matrices
!     Q and Z

      Call ztgsen(ijob, wantq, wantz, select, n, a, lda, b, ldb, alpha, beta, &
        q, ldq, z, ldz, m, pl, pr, dif, work, lwork, iwork, liwork, info)

      If (info/=0) Then
        Write (nout, 100) info
        Write (nout, *)
        Flush (nout)
      End If

!     Print reordered generalized Schur form

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.4', 'Reordered Schur matrix A', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, b, &
        ldb, 'Bracketed', 'F7.4', 'Reordered Schur matrix B', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

!     Print deflating subspaces

      Write (nout, *)
      Flush (nout)

      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, m, q, &
        ldq, 'Bracketed', 'F7.4', 'Basis of left deflating invariant subspace' &
        , 'Integer', rlabs, 'Integer', clabs, 80, 0, ifail)

      Write (nout, *)
      Flush (nout)

      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, m, z, &
        ldz, 'Bracketed', 'F7.4', &
        'Basis of right deflating invariant subspace', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

!     Print norm estimates and F-norm upper bounds

      Write (nout, *)
      Write (nout, 110) 'Norm estimate of projection onto', &
        ' left  eigenspace for selected cluster', 1.0E0_dp/pl
      Write (nout, *)
      Write (nout, 110) 'Norm estimate of projection onto', &
        ' right eigenspace for selected cluster', 1.0E0_dp/pr
      Write (nout, *)
      Write (nout, 110) 'F-norm based upper bound on', ' Difu', dif(1)
      Write (nout, *)
      Write (nout, 110) 'F-norm based upper bound on', ' Difl', dif(2)

100   Format (' Reordering could not be completed. INFO = ', I3)
110   Format (1X, 2A, /, 1X, 1P, E10.2)
    End Program
