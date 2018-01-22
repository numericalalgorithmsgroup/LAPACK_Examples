    Subroutine x04cafn(matrix, diag, m, n, a, lda, title, nout, errbuf, ifail)
!     Mark 14 Release. NAG Copyright 1989.
!     Prints a general real matrix.
!     Easy to use driver for X04CBFN.

!     .. Use Statements ..
      Use lapack_example_aux, Only: p01abf, x04cbfn
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Character (6), Parameter :: srname = 'X04CAF'
!     .. Scalar Arguments ..
      Integer :: ifail, lda, m, n, nout
      Character (1) :: diag, matrix
      Character (200) :: errbuf
      Character (*) :: title
!     .. Array Arguments ..
      Real (Kind=dp) :: a(lda, *)
!     .. Local Scalars ..
      Integer :: ierr, ifcbf, nrec
      Logical :: generl, lower, upper
!     .. Local Arrays ..
      Character (1) :: clabs(1), rlabs(1)
      Character (80) :: rec(2)
!     .. Intrinsic Procedures ..
      Intrinsic :: char
!     .. Executable Statements ..
      Continue
      errbuf(1:1) = char(0)
!
!     Check for incorrect arguments.
!
      ierr = 0
      nrec = 0
      generl = matrix == 'G' .Or. matrix == 'g'
      lower = matrix == 'L' .Or. matrix == 'l'
      upper = matrix == 'U' .Or. matrix == 'u'
!
      If (.Not. generl .And. .Not. lower .And. .Not. upper) Then
        ierr = 1
        nrec = 1
        Write (rec, Fmt=110) matrix
        Write (errbuf, Fmt=120) srname, 1, 99999, 1, matrix
      Else If (m>lda) Then
        ierr = 3
        nrec = 1
        Write (rec, Fmt=130) m, lda
        Write (errbuf, Fmt=140) srname, 3, 99998, 2, m, lda
      Else If (.Not. generl) Then
        If (diag/='U' .And. diag/='u' .And. diag/='N' .And. diag/='n' .And. &
          diag/='B' .And. diag/='b') Then
          ierr = 2
          nrec = 1
          Write (rec, Fmt=150) matrix, diag
          Write (errbuf, Fmt=160) srname, 2, 99997, 2, matrix, diag
        End If
!
      End If
!
      If (ierr/=0) Go To 100
!
!     End of argument checking.
!
      ifcbf = 0
      Call x04cbfn(matrix, diag, m, n, a, lda, ' ', title, 'I', rlabs, 'I', &
        clabs, 80, 0, nout, errbuf, ifcbf)
!
100   Continue
      errbuf(200:200) = char(0)
      ifail = p01abf(ifail, ierr, srname, nrec, rec)
!
      Return
!
110   Format (' ** On entry, MATRIX is not valid: MATRIX = ''', A, '''.')
120   Format (A, 3(1X,I5), 1X, A)
130   Format (' ** On entry, M .gt. LDA: M = ', I16, ', LDA = ', I16, '.')
140   Format (A, 3(1X,I5), 1X, I16, 1X, I16)
150   Format (' ** On entry, MATRIX = ''', A, ''', but DIAG is not val', &
        'id: DIAG = ''', A, '''.')
160   Format (A, 3(1X,I5), 1X, A, 1X, A)
    End Subroutine
