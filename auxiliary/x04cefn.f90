    Subroutine x04cefn(m, n, kl, ku, a, lda, title, errbuf, ifail)
!     Mark 14 Release. NAG Copyright 1989.
!     Prints a real banded matrix stored in packed form.
!     Easy to use driver for X04CFFN.

!     .. Use Statements ..
      Use lapack_example_aux, Only: p01abf, x04cffn
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Character (6), Parameter :: srname = 'X04CEF'
!     .. Scalar Arguments ..
      Integer :: ifail, kl, ku, lda, m, n
      Character (200) :: errbuf
      Character (*) :: title
!     .. Array Arguments ..
      Real (Kind=dp) :: a(lda, *)
!     .. Local Scalars ..
      Integer :: ierr, ifcff, nrec
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
      If (kl<0) Then
        ierr = 1
        nrec = 1
        Write (rec, Fmt=110) kl
        Write (errbuf, Fmt=120) srname, 1, 99999, 1, kl
      Else If (ku<0) Then
        ierr = 2
        nrec = 1
        Write (rec, Fmt=130) ku
        Write (errbuf, Fmt=140) srname, 2, 99998, 1, ku
      Else If (lda<kl+ku+1) Then
        ierr = 3
        nrec = 2
        Write (rec, Fmt=150) lda, kl, ku
        Write (errbuf, Fmt=160) srname, 3, 99997, 3, lda, kl, ku
      End If
!
      If (ierr/=0) Go To 100
!
!     End of argument checking.
!
      ifcff = 1
      Call x04cffn(m, n, kl, ku, a, lda, ' ', title, 'I', rlabs, 'I', clabs, &
        80, 0, errbuf, ifcff)
!
100   Continue
      errbuf(200:200) = char(0)
      ifail = p01abf(ifail, ierr, srname, nrec, rec)
!
      Return
!
110   Format (' ** On entry, KL .lt. 0: KL =', I16, '.')
120   Format (A, 3(1X,I5), 1X, I16)
130   Format (' ** On entry, KU .lt. 0: KU =', I16, '.')
140   Format (A, 3(1X,I5), 1X, I16)
150   Format (' ** On entry, LDA .lt. KL+KU+1: LDA =', I16, ',', /, 4X, &
        'KL =', I16, ', KU =', I16, '.')
160   Format (A, 3(1X,I5), 1X, I16, 1X, I16, 1X, I16)
    End Subroutine
