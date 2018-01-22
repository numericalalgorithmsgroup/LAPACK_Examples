    Subroutine x04bafn(nout, rec)
!     Mark 11.5(F77) Release. NAG Copyright 1986.
!
!     X04BAFN writes the contents of REC to the unit defined by NOUT.
!
!     Trailing blanks are not output, except that if REC is entirely
!     blank, a single blank character is output.
!     If NOUT.lt.0, i.e. if NOUT is not a valid Fortran unit identifier,
!     then no output occurs.
!

!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: nout
      Character (*) :: rec
!     .. Local Scalars ..
      Integer :: i
!     .. Intrinsic Procedures ..
      Intrinsic :: len
!     .. Executable Statements ..
      Continue
!
!     No argument checking.
!
      If (nout>=0) Then
!       Remove trailing blanks
        Do i = len(rec), 2, -1
          If (rec(i:i)/=' ') Go To 100
        End Do
!       Write record to external file
100     Write (nout, Fmt=110) rec(1:i)
      End If
      Return
!
110   Format (A)
    End Subroutine
