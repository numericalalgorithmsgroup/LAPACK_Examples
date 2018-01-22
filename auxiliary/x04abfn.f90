    Subroutine x04abfn(iflag, nadv)
!     Mark 7 Release. NAG Copyright 1978
!     Mark 7c Revised IER-190 (May 1979)
!     Mark 11.5(F77) Revised. (Sept 1985.)
!     Mark 14 Revised. IER-830 (Dec 1989).
!      If IFLAG = 0, sets NADV to current advisory message unit number
!     (stored in NADV1).
!     If IFLAG = 1, changes current advisory message unit number to
!     value specified by NADV.
!
!     .. Implicit None Statement ..
      Implicit None
!     .. Scalar Arguments ..
      Integer :: iflag, nadv
!     .. Local Scalars ..
      Integer, Save :: nadv1
!     .. Data Statements ..
      Data nadv1/6/
!     .. Executable Statements ..
      Continue
!
!     No argument checking.
!
      If (iflag==0) nadv = nadv1
      If (iflag==1) nadv1 = nadv
      Return
    End Subroutine
