    Subroutine x04cffn(m, n, kl, ku, a, lda, form, title, labrow, rlabs, &
      labcol, clabs, ncols, indent, errbuf, ifail)
!     Mark 14 Release. NAG Copyright 1989.
!     Prints a real banded matrix stored in packed form.

!     .. Use Statements ..
      Use lapack_example_aux, Only: p01abf, x04abfn, x04bafn, x04cbzn
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: frnglw = 1.0E-3_dp
      Real (Kind=dp), Parameter :: frngup = 9.99999991E+3_dp
      Real (Kind=dp), Parameter :: one = 1.0E+0_dp
      Real (Kind=dp), Parameter :: zero = 0.0E+0_dp
      Character (6), Parameter :: srname = 'X04CFF'
!     .. Scalar Arguments ..
      Integer :: ifail, indent, kl, ku, lda, m, n, ncols
      Character (200) :: errbuf
      Character (*) :: form, title
      Character (1) :: labcol, labrow
!     .. Array Arguments ..
      Real (Kind=dp) :: a(lda, *)
      Character (*) :: clabs(*), rlabs(*)
!     .. Local Scalars ..
      Real (Kind=dp) :: aa, base, lbase, maxel, minel, rndigs
      Integer :: cl, clbwid, clshft, finis2, finish, i, ierr, incols, indnt, &
        j, k, lwid, ndigs1, ndigs2, nelems, nels, nout, nrec, nsets, ntitle, &
        numwid, offset, rlbwid, start
      Character (132) :: blanks, tilter
      Character (89) :: fmat
      Character (81) :: formt
      Character (10) :: icform, irform
      Character (266) :: infil2
      Character (133) :: infile
!     .. Local Arrays ..
      Character (80) :: rec(2)
!     .. Intrinsic Procedures ..
      Intrinsic :: abs, char, digits, int, len, log10, max, maxexponent, min, &
        minexponent, radix
!     .. Executable Statements ..
      Continue
      errbuf(1:1) = char(0)
!
      ierr = 0
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
      If (kl<0) Then
        ierr = 1
        nrec = 1
        Write (rec, Fmt=140) kl
        Write (errbuf, Fmt=150) srname, 1, 99999, 1, kl
      Else If (ku<0) Then
        ierr = 2
        nrec = 1
        Write (rec, Fmt=160) ku
        Write (errbuf, Fmt=170) srname, 2, 99998, 1, ku
      Else If ((labrow/='N' .And. labrow/='n' .And. labrow/='I' .And. &
          labrow/='i' .And. labrow/='C' .And. labrow/='c') .Or. &
          (labcol/='N' .And. labcol/='n' .And. labcol/='I' .And. &
          labcol/='i' .And. labcol/='C' .And. labcol/='c')) Then
        ierr = 6
        nrec = 2
        Write (rec, Fmt=180) labrow, labcol
        Write (errbuf, Fmt=190) srname, 6, 99997, 2, labrow, labcol
      Else If (lda<kl+ku+1) Then
        ierr = 3
        nrec = 2
        Write (rec, Fmt=200) lda, kl, ku
        Write (errbuf, Fmt=210) srname, 3, 99996, 3, lda, kl, ku
      End If
      If (ierr/=0) Go To 130
!
!     End of argument checking.
!
!     Get the advisory message unit number.
      Call x04abfn(0, nout)
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
        Do j = 1, n
          k = ku + 1 - j
          Do i = max(1, j-ku), min(m, j+kl)
            aa = abs(a(k+i,j))
            If (aa>maxel) maxel = aa
            If (aa/=zero .And. aa<minel) minel = aa
          End Do
        End Do
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
        ierr = 4
        nrec = 2
        Write (rec, Fmt=220) form(start:start+74)
        Write (errbuf, Fmt=230) srname, 4, 99995, 1, form(start:start+74)
        Go To 130
      End If
!
      fmat = '(999(:,' // formt(start:finish) // '))'
!
!     Decide how wide each column of numbers is going to be,
!     by writing the number 0.0 to an internal file and measuring
!     the width needed. Since the width may include trailing blanks,
!     we also write 0.0 twice with the same format, and compute
!     the required field width from the two widths. Note that if
!     FORM has more than one edit descriptor in it, with different
!     field widths, for example FORM = 'E12.3,E13.4', then it is
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
      Write (rec, Fmt=240) form(1:min(75,len(form)))
      Write (errbuf, Fmt=250) srname, 5, 99994, 2, form(1:min(75,len(form)))
      Go To 130
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
        Write (rec, Fmt=260) incols + indnt, indnt
        Write (errbuf, Fmt=270) srname, 7, 99993, 2, incols + indnt, indnt
        Go To 130
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
      If (m<1 .Or. n<1) Go To 130
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
          Write (infile(indnt+rlbwid+2:), Fmt=icform)(cl, cl=clshft+1, &
            clshft+nels)
        Else If (labcol/='N' .And. labcol/='n') Then
!         Process the user-supplied column labels.
          infile = ' '
          lwid = min(clbwid, numwid)
          offset = indnt + rlbwid + 1
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
          offset = indnt + rlbwid + 1
          Do k = clshft + 1, min(clshft+nels, j+ku)
            If (j-kl<=k) Then
!             If we are in the non-zero band.
              Write (infile(offset+1:offset+numwid), Fmt=fmat, Err=120) &
                a(ku+j-k+1, k)
120           Continue
            End If
            offset = offset + numwid
          End Do
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
130   Continue
      errbuf(200:200) = char(0)
      ifail = p01abf(ifail, ierr, srname, nrec, rec)
!
      Return
!
140   Format (' ** On entry, KL .lt. 0: KL =', I16, '.')
150   Format (A, 3(1X,I5), 1X, I16)
160   Format (' ** On entry, KU .lt. 0: KU =', I16, '.')
170   Format (A, 3(1X,I5), 1X, I16)
180   Format (' ** On entry, either LABROW or LABCOL is not valid:', /, 4X, &
        'LABROW = ''', A, ''', LABCOL = ''', A, '''.')
190   Format (A, 3(1X,I5), 1X, A, 1X, A)
200   Format (' ** On entry, LDA .lt. KL+KU+1: LDA =', I16, ',', /, 4X, &
        'KL =', I16, ', KU =', I16, '.')
210   Format (A, 3(1X,I5), 1X, I16, 1X, I16, 1X, I16)
220   Format (' ** On entry, FORM has more than 80 characters: the', &
        ' first 75 are', /, 4X, A, '.')
230   Format (A, 3(1X,I5), 1X, A)
240   Format (' ** The format specifier in FORM cannot be used to ', &
        'print a number: FORM =', /, 4X, A, '.')
250   Format (A, 3(1X,I5), 1X, A)
260   Format (' ** On entry, NCOLS-INDENT is not wide enough to hold', &
        ' at least one matrix', /, 4X, 'column: NCOLS = ', I16, ', INDE', &
        'NT =', I16, '.')
270   Format (A, 3(1X,I5), 1X, I16, 1X, I16)
    End Subroutine
