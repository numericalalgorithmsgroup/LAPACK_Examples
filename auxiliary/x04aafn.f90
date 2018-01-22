    Subroutine x04aafn(iflag, nerr)
!     Mark 7 Release. NAG Copyright 1978
!     Mark 7C Revised IER-190 (May 1979)
!     Mark 11.5(F77) Revised. (Sept 1985.)
!     Mark 14 Revised. IER-829 (Dec 1989).
!     If IFLAG = 0, sets NERR to current error message unit number
!     (stored in NERR1).
!     If IFLAG = 1, changes current error message unit number to
!     value specified by NERR.
!
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: iflag, nerr
!     .. Local Scalars ..
      Integer, Save :: nerr1
!     .. Data Statements ..
      Data nerr1/6/
!     .. Executable Statements ..
      Continue
!
!     No argument checking.
!
      If (iflag==0) nerr = nerr1
      If (iflag==1) nerr1 = nerr
      Return
    End Subroutine
