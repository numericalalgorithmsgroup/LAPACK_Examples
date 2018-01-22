    Subroutine x04ebfn(matrix, diag, m, n, a, lda, form, title, labrow, rlabs, &
      labcol, clabs, ncols, indent, errbuf, ifail)
!     Mark 14 Release. NAG Copyright 1989.
!     Mark 16a Revised. IER-1050 (Jun 1993).
!     Prints a general integer matrix.

!     .. Use Statements ..
      Use lapack_example_aux, Only: p01abf, x04abfn, x04bafn, x04cbzn
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: one = 1, zero = 0
      Character (6), Parameter :: srname = 'X04EBF'
!     .. Scalar Arguments ..
      Integer :: ifail, indent, lda, m, n, ncols
      Character (1) :: diag, labcol, labrow, matrix
      Character (200) :: errbuf
      Character (*) :: form, title
!     .. Array Arguments ..
      Integer :: a(lda, *)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Integer :: aa, cl, clbwid, cleft, clshft, cright, diagel, finis2, &
        finish, i, ierr, incols, indnt, j, k, lwid, maxel, minel, nd, nelems, &
        nels, nout, nrec, nsets, ntitle, numwid, offset, rlbwid, start, start2
      Logical :: generl, lower, prdiag
      Character (132) :: blanks, tilter
      Character (87) :: fmat
      Character (81) :: formt
      Character (10) :: icform, irform
      Character (266) :: infil2
      Character (133) :: infile
!     .. Local Arrays ..
      Character (80) :: rec(2)
!     .. Intrinsic Procedures ..
      Intrinsic :: char, len, max, min
!     .. Executable Statements ..

!-INT INTENT(IN) :: MATRIX, DIAG, M, N, A, LDA, FORM, TITLE, LABROW
!-INT INTENT(IN) :: RLABS, LABCOL, CLABS, NCOLS, INDENT
!-INT INTENT(INOUT) :: IFAIL
!-INT INTENT(OUT) :: ERRBUF
!     .. Executable Statements ..
      Continue
      errbuf(1:1) = char(0)
!
      ierr = 0
      generl = matrix == 'G' .Or. matrix == 'g'
      lower = matrix == 'L' .Or. matrix == 'l'
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
        Write (rec, Fmt=130) matrix
        Write (errbuf, Fmt=140) srname, 1, 99999, 1, matrix
      Else If ((labrow/='N' .And. labrow/='n' .And. labrow/='I' .And. &
          labrow/='i' .And. labrow/='C' .And. labrow/='c') .Or. &
          (labcol/='N' .And. labcol/='n' .And. labcol/='I' .And. &
          labcol/='i' .And. labcol/='C' .And. labcol/='c')) Then
        ierr = 6
        nrec = 2
        Write (rec, Fmt=190) labrow, labcol
        Write (errbuf, Fmt=200) srname, 6, 99996, 2, labrow, labcol
      Else If (m>lda) Then
        ierr = 3
        nrec = 1
        Write (rec, Fmt=150) m, lda
        Write (errbuf, Fmt=160) srname, 3, 99998, 2, m, lda
      Else If (.Not. generl) Then
        If (diag/='U' .And. diag/='u' .And. diag/='N' .And. diag/='n' .And. &
          diag/='B' .And. diag/='b') Then
          ierr = 2
          nrec = 1
          Write (rec, Fmt=170) matrix, diag
          Write (errbuf, Fmt=180) srname, 2, 99997, 2, matrix, diag
        End If
      End If
      If (ierr/=0) Go To 120
!
!     End of argument checking.
!
!     Get the advisory message unit number.
      Call x04abfn(0, nout)
!
      formt = form
      If (formt==' ') Then
!       Construct the FORMAT code of smallest field width that is
!       large enough to print all matrix elements.
!       First find the largest and smallest elements to be printed.
        maxel = zero
        minel = zero
        If (generl) Then
          Do j = 1, n
            Do i = 1, m
              aa = a(i, j)
              If (aa>maxel) maxel = aa
              If (aa<minel) minel = aa
            End Do
          End Do
        Else
          If (diag=='N' .Or. diag=='n') Then
            Do j = 1, min(m, n)
              aa = a(j, j)
              If (aa>maxel) maxel = aa
              If (aa<minel) minel = aa
            End Do
          End If
          Do j = 1, n
            If (lower) Then
              Do i = j + 1, m
                aa = a(i, j)
                If (aa>maxel) maxel = aa
                If (aa<minel) minel = aa
              End Do
            Else
              Do i = 1, j - 1
                aa = a(i, j)
                If (aa>maxel) maxel = aa
                If (aa<minel) minel = aa
              End Do
            End If
          End Do
        End If
!
        Write (infile, Fmt='(I40)') maxel
        Call x04cbzn(infile, start, finish)
        Write (infile, Fmt='(I40)') minel
        Call x04cbzn(infile, start2, finis2)
        k = max(finish-start, finis2-start2) + 2
        formt = 'I  '
        Write (formt(2:), Fmt='(I2)') k
      End If
!
!     Construct the format statement to be used internally.
      Call x04cbzn(formt, start, finish)
      If (finish-start+1>80) Then
!       The length of FORM is too great.
        ierr = 4
        nrec = 2
        Write (rec, Fmt=210) form(start:start+74)
        Write (errbuf, Fmt=220) srname, 4, 99995, 1, form(start:start+74)
        Go To 120
      End If
!
      fmat = '(999(' // formt(start:finish) // '))'
!
!     Decide how wide each column of numbers is going to be,
!     by writing the number 0 to an internal file and measuring
!     the width needed. Since the width may include trailing blanks,
!     we also write 0 twice with the same format, and compute
!     the required field width from the two widths. Note that if
!     FORM has more than one edit descriptor in it, with different
!     field widths, for example FORM = 'I12,I13', then it is
!     not possible to compute a sensible value, in which case
!     the columns of the output matrix will be a little skew.
      Write (infile, Fmt=fmat, Err=100) zero
      Call x04cbzn(infile, start, finish)
      Write (infil2, Fmt=fmat, Err=100) zero, zero
      Call x04cbzn(infil2, start, finis2)
      numwid = finis2 - finish
!     NUMWID is the width of a number as printed using FORMT.
      Go To 110
100   Continue
!     The format in FMAT caused an error when used to print a number.
      ierr = 5
      nrec = 2
      Write (rec, Fmt=230) form(1:min(75,len(form)))
      Write (errbuf, Fmt=240) srname, 5, 99994, 2, form(1:min(75,len(form)))
      Go To 120
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
        ierr = 7
        nrec = 2
        Write (rec, Fmt=250) incols + indnt, indnt
        Write (errbuf, Fmt=260) srname, 7, 99993, 2, incols + indnt, indnt
        Go To 120
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
      If (m<1 .Or. n<1) Go To 120
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
            Write (infile(indnt+rlbwid+2:), Fmt=fmat)(a(j,cl), cl=clshft+1, &
              clshft+nels)
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
                If (cleft<=cright) Then
!                 Optimization bug on Alpha forces us to check
!                 CLEFT.LE.CRIGHT; DIAGEL didn't get printed
!                 out when CLEFT.GT.CRIGHT.
                  Write (infile(indnt+rlbwid+2:), Fmt=fmat)(a(j,cl), cl=cleft, &
                    cright), diagel
                Else
                  Write (infile(indnt+rlbwid+2:), Fmt=fmat) diagel
                End If
              Else
                Write (infile(indnt+rlbwid+2:), Fmt=fmat)(a(j,cl), cl=cleft, &
                  cright)
              End If
            Else
!             Upper triangular matrix.
              If (prdiag) Then
                Write (infile(indnt+rlbwid+2+numwid*(nd- &
                  1):), Fmt=fmat) diagel, (a(j,cl), cl=cleft, cright)
              Else
                If (cleft<=cright) Then
!                 Have to do the check on CLEFT and CRIGHT to
!                 avoid INDNT+RLBWID+2+NUMWID*ND possibly being
!                 out of range.
                  Write (infile(indnt+rlbwid+2+numwid*nd:), Fmt=fmat)(a(j,cl), &
                    cl=cleft, cright)
                End If
              End If
            End If
          End If
!
!         Output the (partial) matrix row.
          Call x04bafn(nout, infile(1:ncols))
!
        End Do
!
        clshft = clshft + nelems
        If (i/=nsets) Then
          Call x04bafn(nout, ' ')
        End If
      End Do
!
120   Continue
      errbuf(200:200) = char(0)
      ifail = p01abf(ifail, ierr, srname, nrec, rec)
!
      Return
!
130   Format (' ** On entry, MATRIX is not valid: MATRIX = ''', A, '''.')
140   Format (A, 3(1X,I5), 1X, A)
150   Format (' ** On entry, M .lt. LDA: M = ', I16, ', LDA = ', I16, '.')
160   Format (A, 3(1X,I5), 1X, I16, 1X, I16)
170   Format (' ** On entry, MATRIX = ''', A, ''', but DIAG is not val', &
        'id: DIAG = ''', A, '''.')
180   Format (A, 3(1X,I5), 1X, A, 1X, A)
190   Format (' ** On entry, either LABROW or LABCOL is not valid', /, 4X, &
        'LABROW = ''', A, ''', LABCOL = ''', A, '''.')
200   Format (A, 3(1X,I5), 1X, A, 1X, A)
210   Format (' ** On entry, FORM has more than 80 characters: the', &
        ' first 75 are', /, 4X, A, '.')
220   Format (A, 3(1X,I5), 1X, A)
230   Format (' ** The format specifier in FORM cannot be used to ', &
        'print a number: FORM =', /, 4X, A, '.')
240   Format (A, 3(1X,I5), 1X, A)
250   Format (' ** On entry, NCOLS-INDENT is not wide enough to hold', &
        ' at least one matrix', /, 4X, 'column: NCOLS = ', I16, ', INDE', &
        'NT =', I16, '.')
260   Format (A, 3(1X,I5), 1X, I16, 1X, I16)
    End Subroutine
