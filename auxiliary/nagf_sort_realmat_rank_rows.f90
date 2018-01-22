    Subroutine nagf_sort_realmat_rank_rows(rm, ldm, m1, m2, n1, n2, order, &
      irank, ifail)
!     Mark 12 Release. NAG Copyright 1986.
!
!     NAGF_SORT_REALMAT_RANK_ROWS ranks the rows of a matrix of real numbers
!     in ascending or descending order.
!
!     NAGF_SORT_REALMAT_RANK_ROWS ranks rows m1 to m2 of a matrix, using
!     the data in columns N1 to N2 of those rows. the ordering is
!     determined by first ranking the data in column N1,
!     then ranking any tied rows according to the data in
!     column N1+1, and so on up to column N2.
!
!     NAGF_SORT_REALMAT_RANK_ROWS uses a variant of list-merging, as
!     described by Knuth. The routine takes advantage of natural ordering
!     in the data, and uses a simple list insertion in a
!     preparatory pass to generate ordered lists of length at
!     least 10. The ranking is stable: equal rows preserve
!     their ordering in the input data.
!
!     The minimum length of the lists at the end of the
!     preparatory pass is defined by the variable MAXINS.
!
!     Written by N.M.Maclaren, University of Cambridge.
!     Revised by NAG Central Office.
!

!     .. Use Statements ..
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: maxins = 10
      Character (6), Parameter :: srname = 'M01DEF'
!     .. Scalar Arguments ..
      Integer :: ifail, ldm, m1, m2, n1, n2
      Character (1) :: order
!     .. Array Arguments ..
      Real (Kind=dp) :: rm(ldm, n2)
      Integer :: irank(m2)
!     .. Local Scalars ..
      Real (Kind=dp) :: a, b, c
      Integer :: i, i1, i2, ierr, ih, ilist, j, j1, k, k1, ktemp, l, list1, &
        list2, nlast, nprev, nrec
!     .. Local Arrays ..
      Character (80) :: p01rec(3)
!     .. External Procedures ..
      Integer, External :: p01abf
!     .. Executable Statements ..
!
!       Check the arguments and deal with the trivial case.
!
      If (m2<1 .Or. n2<1 .Or. m1<1 .Or. m1>m2 .Or. n1<1 .Or. n1>n2 .Or. &
        ldm<m2) Then
        ierr = 1
        Write (p01rec, Fmt=370) m1, m2, ldm, n1, n2
        nrec = 3
      Else If (order/='A' .And. order/='a' .And. order/='D' .And. order/='d') &
          Then
        ierr = 2
        Write (p01rec, Fmt=380) order
        nrec = 1
      Else If (m1==m2) Then
        irank(m2) = m2
        ierr = 0
      Else
        ierr = 0
!
!       Initialise, using natural runs in both directions and
!       straight list insertion for small lists.
!
!       I points to the smallest element in current list
!       J points to the largest  element in current list
!       B is the value of the smallest element in current list
!       C is the value of the largest  element in current list
!
        ilist = -1
        k = m1
        i = k
        j = k
        l = k + maxins
        b = rm(k, n1)
        c = b
        Do k = m1 + 1, m2
!
!         Deal with additions at either end.
!
          a = rm(k, n1)
          If (a>=c) Then
            If (a==c) Then
              Do ih = n1 + 1, n2
                If (rm(k,ih)>rm(j,ih)) Go To 100
                If (rm(k,ih)<rm(j,ih)) Go To 110
              End Do
            End If
100         irank(j) = k
            j = k
            c = a
            Go To 160
          End If
110       If (a<=b) Then
            If (a==b) Then
              Do ih = n1 + 1, n2
                If (rm(k,ih)<rm(i,ih)) Go To 120
                If (rm(k,ih)>rm(i,ih)) Go To 130
              End Do
              Go To 130
            End If
120         irank(k) = i
            i = k
            b = a
            Go To 160
          End If
!
!         Do an ascending list insertion.
!
130       If (k<l) Then
            i2 = i
140         i1 = i2
            i2 = irank(i1)
            If (a>rm(i2,n1)) Go To 140
            If (a==rm(i2,n1)) Then
              Do ih = n1 + 1, n2
                If (rm(k,ih)<rm(i2,ih)) Go To 150
                If (rm(k,ih)>rm(i2,ih)) Go To 140
              End Do
              Go To 140
            End If
150         Continue
            irank(i1) = k
            irank(k) = i2
          Else
!
!           Add the current list on to the others.
!
            If (ilist<0) Then
              list1 = -i
              ilist = 0
            Else If (ilist==0) Then
              list2 = -i
              ilist = 1
              nprev = nlast
            Else
              irank(nprev) = -i
              nprev = nlast
            End If
!
            nlast = j
            i = k
            j = k
            l = k + maxins
            b = rm(k, n1)
            c = b
          End If
160     End Do
!
!       Tidy up at the end.
!
        irank(j) = 0
        If (ilist<0) Then
          list1 = -i
          Go To 320
        Else If (ilist==0) Then
          list2 = -i
        Else
          irank(nprev) = -i
        End If
        irank(nlast) = 0
!
!       At this point:
!       LIST1  = -(index of least element in first list)
!       LIST2  = -(index of least element in second list)
!       For each K, IRANK(K) = index of next element in current list
!       except that, if there is no such element, IRANK(K) =
!       -(index of least element in next list but 1)  or 0 if
!       there is no such list
!
!       Start merging lists by pairs.
!
170     ilist = -1
        i = -list1
        j = -list2
180     k = i
        If (rm(i,n1)>rm(j,n1)) k = j
        If (rm(i,n1)==rm(j,n1)) Then
          Do ih = n1 + 1, n2
            If (rm(i,ih)>rm(j,ih)) Go To 190
            If (rm(i,ih)<rm(j,ih)) Go To 200
          End Do
          Go To 200
190       k = j
        End If
200     If (ilist<0) Then
          list1 = -k
          ilist = 0
        Else If (ilist==0) Then
          list2 = -k
          ilist = 1
          nlast = l
        Else
          irank(nlast) = -k
          nlast = l
        End If
!
!       Move Along The Lists Until One Finishes.
!
!       I  points to the current element in the first list
!       J  points to the current element in the second list
!       K  points to the current element in the merged list
!
        If (k/=i) Go To 240
210     a = rm(j, n1)
        k = i
220     i = k
        k = irank(i)
        If (k<=0) Go To 270
        If (a>rm(k,n1)) Go To 220
        If (a==rm(k,n1)) Then
          Do ih = n1 + 1, n2
            If (rm(j,ih)<rm(k,ih)) Go To 230
            If (rm(j,ih)>rm(k,ih)) Go To 220
          End Do
          Go To 220
        End If
230     irank(i) = j
        i = k
240     a = rm(i, n1)
        k = j
250     j = k
        k = irank(j)
        If (k<=0) Go To 280
        If (a>rm(k,n1)) Go To 250
        If (a==rm(k,n1)) Then
          Do ih = n1 + 1, n2
            If (rm(i,ih)<rm(k,ih)) Go To 260
            If (rm(i,ih)>rm(k,ih)) Go To 250
          End Do
        End If
260     irank(j) = i
        j = k
        Go To 210
!
!       Add the remains of one list to the other.
!
270     ktemp = 1
        Go To 290
280     ktemp = 2
290     If (ktemp==1) Then
!
!         First list is exhausted
!
          irank(i) = j
          i = -k
          j1 = j
300       j = j1
          j1 = irank(j)
          If (j1>0) Go To 300
          l = j
          j = -j1
        Else
!
!         Second list is exhausted
!
          irank(j) = i
          j = -k
          i1 = i
310       i = i1
          i1 = irank(i)
          If (i1>0) Go To 310
          l = i
          i = -i1
        End If
!
!       Tidy up and carry on if not finished.
!
        If ((i/=0) .And. (j/=0)) Go To 180
        irank(l) = 0
        k = i + j
        If (ilist>0) Then
          irank(nlast) = -k
          Go To 170
        Else If (k/=0) Then
          list2 = -k
          Go To 170
        End If
!
!       If descending, reverse all pointers between equality
!       blocks.
!
320     If (order=='D' .Or. order=='d') Then
          i = 0
          j = -list1
330       k = j
          k1 = k
          a = rm(k, n1)
          ktemp = k
340       k = k1
          k1 = irank(k)
          If (k1/=0) Then
            If (a==rm(k1,n1)) Then
              Do ih = n1 + 1, n2
                If (rm(ktemp,ih)/=rm(k1,ih)) Go To 350
              End Do
              Go To 340
            End If
          End If
350       irank(k) = i
          i = j
          j = k1
          If (j/=0) Go To 330
          list1 = -i
        End If
!
!       Convert the list form to ranks and return.
!
        k = m1
        i = -list1
360     i1 = irank(i)
        irank(i) = k
        k = k + 1
        i = i1
        If (i>0) Go To 360
!
      End If
      If (ierr/=0) Then
        ifail = p01abf(ifail, ierr, srname, nrec, p01rec)
      Else
        ifail = 0
      End If
      Return
!
370   Format (' ** On entry, one or more of the following parameter va', &
        'lues is illegal', /, '    M1 =', I16, '  M2 =', I16, '  LDM =', I16, &
        /, '    N1 =', I16, '  N2 =', I16)
380   Format (' ** On entry, ORDER has an illegal value: ORDER = ', A1)
    End Subroutine
