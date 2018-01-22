    Function p01abf(ifail, ierror, srname, nrec, rec)
!     Mark 11.5(F77) Release. NAG Copyright 1986.
!     Mark 13 Revised. IER-621 (Apr 1988).
!     Mark 13B Revised. IER-668 (Aug 1988).
!
!     P01ABF is the error-handling routine for the NAG Library.
!
!     P01ABF either returns the value of IERROR through the routine
!     name (soft failure), or terminates execution of the program
!     (hard failure). Diagnostic messages may be output.
!
!     If IERROR = 0 (successful exit from the calling routine),
!     the value 0 is returned through the routine name, and no
!     message is output
!
!     If IERROR is non-zero (abnormal exit from the calling routine),
!     the action taken depends on the value of IFAIL.
!
!     IFAIL =  1: soft failure, silent exit (i.e. no messages are
!                 output)
!     IFAIL = -1: soft failure, noisy exit (i.e. messages are output)
!     IFAIL =-13: soft failure, noisy exit but standard messages from
!                 P01ABF are suppressed
!     IFAIL =  0: hard failure, noisy exit
!
!     For compatibility with certain routines included before Mark 12
!     P01ABF also allows an alternative specification of IFAIL in which
!     it is regarded as a decimal integer with least significant digits
!     cba. Then
!
!     a = 0: hard failure  a = 1: soft failure
!     b = 0: silent exit   b = 1: noisy exit
!
!     except that hard failure now always implies a noisy exit.
!
!     S.Hammarling, M.P.Hooper and J.J.du Croz, NAG Central Office.
!

!     .. Implicit None Statement ..
      Implicit None
!     .. Function Return Value ..
      Integer :: p01abf
!     .. Scalar Arguments ..
      Integer :: ierror, ifail, nrec
      Character (*) :: srname
!     .. Array Arguments ..
      Character (*) :: rec(*)
!     .. Local Scalars ..
      Integer :: i, nerr
      Character (72) :: mess
!     .. External Procedures ..
      External :: x04aafn, x04bafn
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, mod
!     .. Executable Statements ..
      Continue
!
!     No argument checking.
!
      If (ierror/=0) Then
!
!       Abnormal exit from calling routine
!
        If (ifail==-1 .Or. ifail==0 .Or. ifail==-13 .Or. (ifail>0 .And. mod( &
          ifail/10,10)/=0)) Then
!
!         Noisy exit
!
          Call x04aafn(0, nerr)
          Do i = 1, nrec
            Call x04bafn(nerr, rec(i))
          End Do
          If (ifail/=-13) Then
            Write (mess, Fmt=100) srname, ierror
            Call x04bafn(nerr, mess)
            If (abs(mod(ifail,10))/=1) Then
!
!             Hard failure
!
              Call x04bafn(nerr, ' ** NAG hard failure - execution terminated' &
                )
              Stop
            Else
!
!             Soft failure
!
              Call x04bafn(nerr, ' ** NAG soft failure - control returned')
            End If
          End If
        End If
      End If
!
      p01abf = ierror
!
      Return
!
100   Format (' ** ABNORMAL EXIT from NAG Library routine ', A, ': IFAIL', &
        ' =', I6)
    End Function
