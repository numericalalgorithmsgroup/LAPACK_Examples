    Subroutine x04dbfn(matrix, diag, m, n, a, lda, usefrm, form, title, &
      labrow, rlabs, labcol, clabs, ncols, indent, nout, errbuf, ifail)
!     Mark 14 Release. NAG Copyright 1989.
!     Mark 16a Revised. IER-1049 (Jun 1993).
!     Prints a general complex matrix.

!     .. Use Statements ..
      Use lapack_example_aux, Only: p01abf, x04bafn, x04cbzn
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: czero = (0.0_dp, 0.0_dp)
      Real (Kind=dp), Parameter :: frnglw = 1.0E-3_dp
      Real (Kind=dp), Parameter :: frngup = 9.99999991E+3_dp
      Real (Kind=dp), Parameter :: one = 1.0_dp
      Real (Kind=dp), Parameter :: zero = 0.0_dp
      Character (6), Parameter :: srname = 'X04DBF'
!     .. Scalar Arguments ..
      Integer :: ifail, indent, lda, m, n, ncols, nout
      Character (1) :: diag, labcol, labrow, matrix, usefrm
      Character (200) :: errbuf
      Character (*) :: form, title
!     .. Array Arguments ..
      Complex (Kind=dp) :: a(lda, *)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Complex (Kind=dp) :: diagel
      Real (Kind=dp) :: aa, base, lbase, maxel, minel, rndigs
      Integer :: cl, clbwid, cleft, clshft, cright, finis2, finish, i, ierr, &
        incols, indnt, j, k, lwid, nd, ndigs1, ndigs2, nelems, nels, nrec, &
        nsets, ntitle, numwid, offset, rlbwid, start
      Logical :: above, brack, generl, lower, prdiag
      Character (132) :: blanks, tilter
      Character (189) :: fmat
      Character (81) :: formt
      Character (10) :: icform, irform
      Character (133) :: infbot, infile
      Character (266) :: infil2
!     .. Local Arrays ..
      Character (80) :: rec(2)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, aimag, char, digits, int, len, log10, max, &
        maxexponent, min, minexponent, radix, real
!     .. Executable Statements ..
      Continue
      errbuf(1:1) = char(0)
!
      ierr = 0
      generl = matrix == 'G' .Or. matrix == 'g'
      lower = matrix == 'L' .Or. matrix == 'l'
      above = usefrm == 'A' .Or. usefrm == 'a'
      brack = usefrm == 'B' .Or. usefrm == 'b'
      If (ncols<=0 .Or. ncols>132) Then
        incols = 80
      Else
        incols = ncols
      End If
      If (indent<0 .Or. indent>=incols) Then
        indnt = 0
      Else
        indnt = indent
      End If
      incols = incols - indnt
      blanks = ' '
!
!     Check for incorrect arguments.
      If (.Not. generl .And. .Not. lower .And. matrix/='U' .And. matrix/='u') &
        Then
        ierr = 1
        nrec = 1
        Write (rec, Fmt=280) matrix
        Write (errbuf, Fmt=290) srname, 1, 99999, 1, matrix
      Else If ((labrow/='N' .And. labrow/='n' .And. labrow/='I' .And. &
          labrow/='i' .And. labrow/='C' .And. labrow/='c') .Or. &
          (labcol/='N' .And. labcol/='n' .And. labcol/='I' .And. &
          labcol/='i' .And. labcol/='C' .And. labcol/='c')) Then
        ierr = 7
        nrec = 2
        Write (rec, Fmt=340) labrow, labcol
        Write (errbuf, Fmt=350) srname, 7, 99996, 2, labrow, labcol
      Else If (.Not. above .And. .Not. brack .And. usefrm/='D' .And. &
          usefrm/='d') Then
        ierr = 4
        nrec = 1
        Write (rec, Fmt=360) usefrm
        Write (errbuf, Fmt=370) srname, 4, 99995, 1, usefrm
      Else If (m>lda) Then
        ierr = 3
        nrec = 1
        Write (rec, Fmt=300) m, lda
        Write (errbuf, Fmt=310) srname, 3, 99998, 2, m, lda
      Else If (.Not. generl) Then
        If (diag/='U' .And. diag/='u' .And. diag/='N' .And. diag/='n' .And. &
          diag/='B' .And. diag/='b') Then
          ierr = 2
          nrec = 1
          Write (rec, Fmt=320) matrix, diag
          Write (errbuf, Fmt=330) srname, 2, 99997, 2, matrix, diag
        End If
      End If
      If (ierr/=0) Go To 270
!
!     End of argument checking.
!
!
      formt = form
      If (formt=='*') Then
!       Construct an E FORMAT that is wide enough to distinguish
!       between adjacent machine numbers.
        base = radix(zero)
        lbase = log10(base)
        rndigs = digits(zero)*lbase
        ndigs1 = int(rndigs)
        If (ndigs1<rndigs) ndigs1 = ndigs1 + 1
!       NDIGS1 is the number of significant decimal digits required
!       for the mantissa.
        rndigs = log10(max(-minexponent(zero),maxexponent(zero))*lbase) + 2
        ndigs2 = int(rndigs)
        If (ndigs2<rndigs) ndigs2 = ndigs2 + 1
!       NDIGS2 is the number of decimal places required for the
!       exponent, including letter 'E' and sign.
        formt = '1P,E   .   '
        Write (formt(5:7), Fmt='(I3)') ndigs1 + ndigs2 + 3
        Write (formt(9:11), Fmt='(I3)') ndigs1 - 1
      Else If (formt==' ') Then
!       Construct either a fixed point FORMAT, if the elements to be
!       printed are a reasonable size, i.e. all lie inside the range
!       0.001 to 9999.9999, or a floating-point FORMAT otherwise,
!       printing to 5 significant digits.
!       First find the largest and smallest elements to be printed,
!       ignoring zeros.
        maxel = one
        minel = one
        If (generl) Then
          Do j = 1, n
            Do i = 1, m
              aa = max(abs(real(a(i,j))), abs(aimag(a(i,j))))
              If (aa>maxel) maxel = aa
              If (aa/=zero .And. aa<minel) minel = aa
            End Do
          End Do
        Else
          If (diag=='N' .Or. diag=='n') Then
            Do j = 1, min(m, n)
              aa = max(abs(real(a(j,j))), abs(aimag(a(j,j))))
              If (aa>maxel) maxel = aa
              If (aa/=zero .And. aa<minel) minel = aa
            End Do
          End If
          Do j = 1, n
            If (lower) Then
              Do i = j + 1, m
                aa = max(abs(real(a(i,j))), abs(aimag(a(i,j))))
                If (aa>maxel) maxel = aa
                If (aa/=zero .And. aa<minel) minel = aa
              End Do
            Else
              Do i = 1, j - 1
                aa = max(abs(real(a(i,j))), abs(aimag(a(i,j))))
                If (aa>maxel) maxel = aa
                If (aa/=zero .And. aa<minel) minel = aa
              End Do
            End If
          End Do
        End If
!
        If (minel>=frnglw .And. maxel<frngup) Then
!         If all elements to be printed are moderately sized,
!         use a fixed point FORMAT ...
          If (maxel<=one) Then
            formt = 'F8.4'
          Else
            formt = 'F11.4'
          End If
        Else
!         ... otherwise use a floating-point FORMAT.
          formt = '1PE13.4'
        End If
      End If
!
!     Construct the format statement to be used internally.
      Call x04cbzn(formt, start, finish)
      If (finish-start+1>80) Then
!       The length of FORM is too great.
        ierr = 5
        nrec = 2
        Write (rec, Fmt=380) form(start:start+74)
        Write (errbuf, Fmt=390) srname, 5, 99994, 1, form(start:start+74)
        Go To 270
      End If
!
      If (brack) Then
!       Insert brackets and comma into the edit descriptor.
        fmat = '(999(:,'' ('',' // formt(start:finish) // ','','',' // &
          formt(start:finish) // ','')''))'
      Else
        fmat = '(999(:,' // formt(start:finish) // '))'
      End If
!
!     Decide how wide each column of numbers is going to be,
!     by writing the number ZERO to an internal file and measuring
!     the width needed. Since the width may include trailing blanks,
!     we also write ZERO twice with the same format, and compute
!     the required field width from the two widths. Note that if
!     FORM has more than one edit descriptors in it, with different
!     field widths, for example FORM = 'E12.3,E13.4', then it may
!     not be possible to compute a sensible value, in which case
!     the columns of the output matrix will be a little skew.
      If (above) Then
        Write (infile, Fmt=fmat, Err=100) zero
        Write (infil2, Fmt=fmat, Err=100) zero, zero
      Else
        Write (infile, Fmt=fmat, Err=100) czero
        Write (infil2, Fmt=fmat, Err=100) czero, czero
      End If
      Call x04cbzn(infile, start, finish)
      Call x04cbzn(infil2, start, finis2)
      numwid = finis2 - finish
!     NUMWID is the width of a number as printed using FORMT.
      Go To 110
100   Continue
!     The format in FMAT caused an error when used to print a number.
      ierr = 6
      nrec = 2
      Write (rec, Fmt=400) form(1:min(75,len(form)))
      Write (errbuf, Fmt=410) srname, 6, 99993, 2, form(1:min(75,len(form)))
      Go To 270
110   Continue
!
!     What kind of row labelling is required?
      If (labrow=='N' .Or. labrow=='n') Then
!       No row labelling.
        rlbwid = 1
      Else If (labrow=='I' .Or. labrow=='i') Then
!       Numeric row labelling.
        Write (infile, Fmt='(I16)') m
        Call x04cbzn(infile, start, finish)
        rlbwid = finish - start + 2
        irform = '(I    )'
        Write (irform(3:6), Fmt='(I4)') rlbwid
      Else
!       User supplied row labelling.
        rlbwid = 1
        Do i = 1, m
          Call x04cbzn(rlabs(i), start, finish)
          rlbwid = max(rlbwid, finish-start+2)
        End Do
      End If
!
!     What kind of column labelling is required?
      If (labcol=='I' .Or. labcol=='i') Then
!       Numeric column labelling.
        Write (infile, Fmt='(I16)') n
        Call x04cbzn(infile, start, finish)
        clbwid = finish - start + 2
        icform = '(999I    )'
        Write (icform(6:9), Fmt='(I4)') numwid
      Else If (labcol/='N' .And. labcol/='n') Then
!       User supplied column labelling.
        clbwid = len(clabs(1))
      End If
!
      nelems = (incols-1-rlbwid)/numwid
      If (nelems<1) Then
        ierr = 8
        nrec = 2
        Write (rec, Fmt=420) incols + indnt, indnt
        Write (errbuf, Fmt=430) srname, 8, 99992, 2, incols + indnt, indnt
        Go To 270
      End If
!     NELEMS is the number of elements that can fit into INCOLS columns.
!
      nsets = (n-1)/nelems + 1
!     NSETS is the number of pieces that the matrix must be split into.
!
!     Print the title, splitting it up if more than INCOLS-1 characters.
      Call x04cbzn(title, start, finish)
      If (finish/=0) Then
        ntitle = (finish-1)/(incols-1) + 1
        Do i = 1, ntitle - 1
          tilter = blanks(1:indnt+1) // title((i-1)*(incols-1)+1:i*(incols-1))
          Call x04bafn(nout, tilter)
        End Do
        tilter = blanks(1:indnt+1) // title((ntitle-1)*(incols-1)+1:finish)
        Call x04bafn(nout, tilter)
      End If
!
!     Exit after printing the title if M or N is less than 1.
      If (m<1 .Or. n<1) Go To 270
!
!     Print the matrix, with row and column labels if requested.
      clshft = 0
!     CLSHFT is the offset into the current set of columns, when
!     the matrix cannot be printed in one go but has to be split.
      Do i = 1, nsets
        If (i==nsets) Then
          nels = n - (nsets-1)*nelems
        Else
          nels = nelems
        End If
        If (labcol=='I' .Or. labcol=='i') Then
!         Construct the numeric column labels.
          infile = ' '
          Write (infile(rlbwid+2+indnt:), Fmt=icform)(cl, cl=clshft+1, &
            clshft+nels)
        Else If (labcol/='N' .And. labcol/='n') Then
!         Process the user-supplied column labels.
          infile = ' '
          lwid = min(clbwid, numwid)
          offset = rlbwid + 1 + indnt
          Do k = clshft + 1, clshft + nels
            Call x04cbzn(clabs(k)(1:lwid), start, finish)
            If (start==0) Then
              start = 1
              finish = 1
            End If
            infile(offset+numwid-finish+start:offset+numwid+finish-start) &
              = clabs(k)(start:finish)
            offset = offset + numwid
          End Do
        End If
!       Output the column labels.
        If (labcol/='N' .And. labcol/='n') Then
          Call x04bafn(nout, infile(1:ncols))
        End If
!
!       Now print each row in turn.
        Do j = 1, m
          infile = ' '
!
!         Insert the row label.
          If (labrow=='I' .Or. labrow=='i') Then
            Write (infile(indnt+1:indnt+rlbwid), Fmt=irform) j
          Else If (labrow/='N' .And. labrow/='n') Then
            Call x04cbzn(rlabs(j), start, finish)
            If (start==0) Then
              start = 1
              finish = 1
            End If
            infile(indnt+rlbwid-finish+start:indnt+rlbwid) = rlabs(j) &
              (start:finish)
          End If
!
          If (generl) Then
!           General rectangular matrix.
            If (above) Then
              Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=120)(real(a(j, &
                cl)), cl=clshft+1, clshft+nels)
120           Continue
              infbot(1:indnt+rlbwid+1) = ' '
              Write (infbot(indnt+rlbwid+2:), Fmt=fmat, Err=130)(aimag(a(j, &
                cl)), cl=clshft+1, clshft+nels)
130           Continue
            Else
              Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=140)(a(j,cl), &
                cl=clshft+1, clshft+nels)
140           Continue
            End If
          Else
!           Upper or lower triangular matrix.
            nd = max(0, j-(i-1)*nelems)
!           ND is the position of the Jth row diagonal element.
            If (lower) Then
              cleft = clshft + 1
              cright = clshft + min(nd-1, nels)
            Else
              cleft = clshft + nd + 1
              cright = clshft + nels
            End If
!           CLEFT and CRIGHT are the leftmost and rightmost elements
!           of the current row to be printed, excluding the diagonal.
            prdiag = diag /= 'B' .And. diag /= 'b' .And. nd > 0 .And. &
              nd <= nels
!           PRDIAG is true if a diagonal element appears in the
!           current matrix row section, and it is to be printed.
            If (prdiag) Then
              If (diag=='U' .Or. diag=='u') Then
                diagel = one
              Else
                diagel = a(j, j)
              End If
            End If
!
            If (lower) Then
!             Lower triangular matrix.
              If (prdiag) Then
                If (above) Then
                  If (cleft<=cright) Then
!                   Optimization bug on Alpha forces us to check
!                   CLEFT.LE.CRIGHT; DIAGEL didn't get printed
!                   out when CLEFT.GT.CRIGHT.
                    Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=150)(real(a( &
                      j,cl)), cl=cleft, cright), real(diagel)
                  Else
                    Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=150) &
                      real(diagel)
                  End If
150               Continue
                  infbot(1:indnt+rlbwid+1) = ' '
                  If (cleft<=cright) Then
                    Write (infbot(indnt+rlbwid+2:), Fmt=fmat, Err=160) &
                      (aimag(a(j,cl)), cl=cleft, cright), aimag(diagel)
                  Else
                    Write (infbot(indnt+rlbwid+2:), Fmt=fmat, Err=160) &
                      aimag(diagel)
                  End If
160               Continue
                Else
                  If (cleft<=cright) Then
                    Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=170) &
                      (a(j,cl), cl=cleft, cright), diagel
                  Else
                    Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=170) diagel
                  End If
170               Continue
                End If
              Else
                If (above) Then
                  Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=180)(real(a(j, &
                    cl)), cl=cleft, cright)
180               Continue
                  infbot(1:indnt+rlbwid+1) = ' '
                  Write (infbot(indnt+rlbwid+2:), Fmt=fmat, Err=190)(aimag(a( &
                    j,cl)), cl=cleft, cright)
190               Continue
                Else
                  Write (infile(indnt+rlbwid+2:), Fmt=fmat, Err=200)(a(j,cl), &
                    cl=cleft, cright)
200               Continue
                End If
              End If
            Else
!             Upper triangular matrix.
              If (prdiag) Then
                If (above) Then
                  Write (infile(indnt+rlbwid+2+numwid*(nd- &
                    1):), Fmt=fmat, Err=210) real(diagel), &
                    (real(a(j,cl)), cl=cleft, cright)
210               Continue
                  infbot = ' '
                  Write (infbot(indnt+rlbwid+2+numwid*(nd- &
                    1):), Fmt=fmat, Err=220) aimag(diagel), &
                    (aimag(a(j,cl)), cl=cleft, cright)
220               Continue
                Else
                  Write (infile(indnt+rlbwid+2+numwid*(nd- &
                    1):), Fmt=fmat, Err=230) diagel, &
                    (a(j,cl), cl=cleft, cright)
230               Continue
                End If
              Else
                If (cleft<=cright) Then
!                 Have to do the check on CLEFT and CRIGHT to
!                 avoid INDNT+RLBWID+2+NUMWID*ND possibly being
!                 out of range.
                  If (above) Then
                    Write (infile(indnt+rlbwid+2+numwid*nd:), Fmt=fmat, &
                      Err=240)(real(a(j,cl)), cl=cleft, cright)
240                 Continue
                    infbot = ' '
                    Write (infbot(indnt+rlbwid+2+numwid*nd:), Fmt=fmat, &
                      Err=250)(aimag(a(j,cl)), cl=cleft, cright)
250                 Continue
                  Else
                    Write (infile(indnt+rlbwid+2+numwid*nd:), Fmt=fmat, &
                      Err=260)(a(j,cl), cl=cleft, cright)
260                 Continue
                  End If
                End If
              End If
            End If
          End If
!
!         Output the (partial) matrix row.
          Call x04bafn(nout, infile(1:ncols))
          If (above) Then
            Call x04bafn(nout, infbot(1:ncols))
            If (j<m) Call x04bafn(nout, ' ')
          End If
!
        End Do
!
        clshft = clshft + nelems
        If (i/=nsets) Then
          Call x04bafn(nout, ' ')
        End If
      End Do
!
270   Continue
      errbuf(200:200) = char(0)
      ifail = p01abf(ifail, ierr, srname, nrec, rec)
!
      Return
!
280   Format (' ** On entry, MATRIX is not valid: MATRIX = ''', A, '''.')
290   Format (A, 3(1X,I5), 1X, A)
300   Format (' ** On entry, M .gt .LDA: M = ', I16, ', LDA = ', I16, '.')
310   Format (A, 3(1X,I5), 1X, I16, 1X, I16)
320   Format (' ** On entry, MATRIX = ''', A, ''', but DIAG is not val', &
        'id: DIAG = ''', A, '''.')
330   Format (A, 3(1X,I5), 1X, A, 1X, A)
340   Format (' ** On entry, either LABROW or LABCOL is not valid', /, 4X, &
        'LABROW = ''', A, ''', LABCOL = ''', A, '''.')
350   Format (A, 3(1X,I5), 1X, A, 1X, A)
360   Format (' ** On entry, invalid USEFRM: USEFRM = ''', A, '''.')
370   Format (A, 3(1X,I5), 1X, A)
380   Format (' ** On entry, FORM has more than 80 characters: the', &
        ' first 75 are', /, 4X, A, '.')
390   Format (A, 3(1X,I5), 1X, A)
400   Format (' ** The format specifier in FORM cannot be used to ', &
        'print a number: FORM =', /, 4X, A, '.')
410   Format (A, 3(1X,I5), 1X, A)
420   Format (' ** On entry, NCOLS-INDENT is not wide enough to hold', &
        ' at least one matrix', /, 4X, 'column: NCOLS = ', I16, ', INDE', &
        'NT =', I16, '.')
430   Format (A, 3(1X,I5), 1X, I16, 1X, I16)
    End Subroutine
