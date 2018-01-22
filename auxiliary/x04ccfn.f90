    Subroutine x04ccfn(uplo, diag, n, a, title, errbuf, ifail)
!     Mark 14 Release. NAG Copyright 1989.
!     Prints a real triangular matrix stored in packed vector form.
!     Easy to use driver for X04CDFN.

!     .. Use Statements ..
      Use lapack_example_aux, Only: p01abf, x04cdfn
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Character (6), Parameter :: srname = 'X04CCF'
!     .. Scalar Arguments ..
      Integer :: ifail, n
      Character (1) :: diag, uplo
      Character (200) :: errbuf
      Character (*) :: title
!     .. Array Arguments ..
      Real (Kind=dp) :: a(*)
!     .. Local Scalars ..
      Integer :: ierr, ifcdf, nrec
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
!
      If (uplo/='L' .And. uplo/='l' .And. uplo/='U' .And. uplo/='u') Then
        ierr = 1
        nrec = 1
        Write (rec, Fmt=110) uplo
        Write (errbuf, Fmt=120) srname, 1, 99999, 1, uplo
      Else If (diag/='U' .And. diag/='u' .And. diag/='N' .And. diag/='n' .And. &
          diag/='B' .And. diag/='b') Then
        ierr = 2
        nrec = 1
        Write (rec, Fmt=130) diag
        Write (errbuf, Fmt=140) srname, 2, 99998, 1, diag
      End If
!
      If (ierr/=0) Go To 100
!
!     End of argument checking.
!
      ifcdf = 1
      Call x04cdfn(uplo, diag, n, a, ' ', title, 'I', rlabs, 'I', clabs, 80, &
        0, errbuf, ifcdf)
!
100   Continue
      errbuf(200:200) = char(0)
      ifail = p01abf(ifail, ierr, srname, nrec, rec)
!
      Return
!
110   Format (' ** On entry, UPLO is not valid: UPLO = ''', A, '''.')
120   Format (A, 3(1X,I5), 1X, A)
130   Format (' ** On entry, DIAG is not valid: DIAG = ''', A, '''.')
140   Format (A, 3(1X,I5), 1X, A)
    End Subroutine
