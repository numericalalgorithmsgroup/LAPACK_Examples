    Program ztgexc_example

!     ZTGEXC Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: ztgexc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Logical, Parameter :: wantq = .False., wantz = .False.
!     .. Local Scalars ..
      Integer :: i, ifail, ifst, ilst, info, lda, ldb, ldq, ldz, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), b(:, :), q(:, :), z(:, :)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZTGEXC Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldq = 1
      ldz = 1
      lda = n
      ldb = n
      Allocate (a(lda,n), b(ldb,n), q(ldq,1), z(ldz,1))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Read the row indices

      Read (nin, *) ifst, ilst

!     Reorder the A and B

      Call ztgexc(wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz, ifst, ilst, &
        info)

      If (info/=0) Then
        Write (nout, 100) info, ilst
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

100   Format (' Reordering could not be completed. INFO = ', I3, ' ILST = ', &
        I5)
    End Program
