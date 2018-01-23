    Program dtgexc_example

!     DTGEXC Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dtgexc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
      Logical, Parameter :: wantq = .False., wantz = .False.
!     .. Local Scalars ..
      Integer :: i, ifail, ifst, ilst, info, lda, ldb, ldq, ldz, lwork, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), b(:, :), q(:, :), work(:), &
        z(:, :)
!     .. Executable Statements ..
      Write (nout, *) 'DTGEXC Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldq = 1
      ldz = 1
      lda = n
      ldb = n
      lwork = 4*n + 16
      Allocate (a(lda,n), b(ldb,n), q(ldq,1), work(lwork), z(ldz,1))

!     Read A and B from data file

      Read (nin, *)(a(i,1:n), i=1, n)
      Read (nin, *)(b(i,1:n), i=1, n)

!     Read the row indices

      Read (nin, *) ifst, ilst

!     Reorder A and B

      Call dtgexc(wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz, ifst, ilst, &
        work, lwork, info)

      If (info/=0) Then
        Write (nout, 100) info, ilst
        Write (nout, *)
        Flush (nout)
      End If

!     The resulting reordered Schur matrices can differ by +- signs by
!     multiplying rows and columns of Q and Z by -1. We will normalize here by
!     making the diagonals and last column of B positive.
      Call normalize(a, b)

!     Print reordered generalized Schur form

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, a, lda, &
        'Reordered Schur matrix A', ifail)

      Write (nout, *)
      Flush (nout)

      ifail = 0
      Call nagf_file_print_matrix_real_gen('General', ' ', n, n, b, ldb, &
        'Reordered Schur matrix B', ifail)

100   Format (' Reordering could not be completed. INFO = ', I3, ' ILST = ', &
        I5)

    Contains
      Subroutine normalize(a, b)

!       .. Array Arguments ..
        Real (Kind=dp), Intent (Inout) :: a(lda, n), b(ldb, n)
!       .. Local Scalars ..
        Integer :: i, j
!       .. Intrinsic Procedures ..
        Intrinsic :: max
!       .. Executable Statements ..

!       Last column of B positive
        Do i = 1, n
          j = max(1, i-1)
          If (b(i,n)<0.0_dp) Then
            a(i, j:n) = -a(i, j:n)
            b(i, i:n) = -b(i, i:n)
          End If
        End Do

!       Diagonals of B positive
        Do i = 1, n - 1
          If (b(i,i)<0.0_dp) Then
            a(1:i+1, i) = -a(1:i+1, i)
            b(1:i, i) = -b(1:i, i)
          End If
        End Do
      End Subroutine
    End Program
