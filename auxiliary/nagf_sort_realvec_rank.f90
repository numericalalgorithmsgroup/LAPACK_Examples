    Subroutine nagf_sort_realvec_rank(rv, m1, m2, order, irank, ifail)
!     Mark 12 Release. NAG Copyright 1986.
!
!     NAGF_SORT_REALVEC_RANK ranks a vector of real numbers in ascending
!     or descending order.
!
!     NAGF_SORT_REALVEC_RANK uses a variant of list-merging, as described
!     by Knuth. The routine takes advantage of natural
!     ordering in the data, and uses a simple list insertion
!     in a preparatory pass to generate ordered lists of
!     length at least 10. The ranking is stable: equal elements
!     preserve their ordering in the input data.
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
      Character (6), Parameter :: srname = 'M01DAF'
!     .. Scalar Arguments ..
      Integer :: ifail, m1, m2
      Character (1) :: order
!     .. Array Arguments ..
      Real (Kind=dp) :: rv(m2)
      Integer :: irank(m2)
!     .. Local Scalars ..
      Real (Kind=dp) :: a, b, c
      Integer :: i, i1, i2, ierr, ilist, j, j1, j2, k, k1, k2, l, list1, &
        list2, nlast, nprev, nrec
!     .. Local Arrays ..
      Character (80) :: p01rec(2)
!     .. External Procedures ..
      Integer, External :: p01abf
!     .. Executable Statements ..
!
!     Check the arguments and deal with the trivial case.
!
      If (m2<1 .Or. m1<1 .Or. m1>m2) Then
        ierr = 1
        Write (p01rec, Fmt=260) m1, m2
        nrec = 2
      Else If (order/='A' .And. order/='a' .And. order/='D' .And. order/='d') &
          Then
        ierr = 2
        Write (p01rec, Fmt=270) order
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
!       I  points to the smallest element in the current list
!       J  points to the largest  element in the current list
!       B  is the value of the smallest element in current list
!       C  is the value of the largest  element in current list
!
        ilist = -1
        k = m1
        i = k
        j = k
        l = k + maxins
        b = rv(k)
        c = b
        Do k = m1 + 1, m2
!
!         Deal with additions at either end.
!
          a = rv(k)
          If (a>=c) Then
            irank(j) = k
            j = k
            c = a
          Else If (a<b) Then
            irank(k) = i
            i = k
            b = a
          Else
!
!           Do an ascending list insertion.
!
            If (k<l) Then
              i2 = i
100           i1 = i2
              i2 = irank(i1)
              If (a>=rv(i2)) Go To 100
              irank(i1) = k
              irank(k) = i2
            Else
!
!             Add the current list on to the others.
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
              b = rv(k)
              c = b
            End If
          End If
        End Do
!
!       Tidy up at the end.
!
        irank(j) = 0
        If (ilist<0) Then
          list1 = -i
          Go To 220
        Else If (ilist==0) Then
          list2 = -i
        Else
          irank(nprev) = -i
        End If
        irank(nlast) = 0
!
!       At this point:
!       LIST1 = - (index of least element in the first list)
!       LIST2 = - (index of least element in the second list)
!       For each K, IRANK(K) is the index of the next element in the
!       current list, except that, if there is no such element,
!       IRANK(K) is - (index of the least element in the next list
!       but 1)  or 0 if there is no such list.
!
!       Start merging lists by pairs.
!
110     ilist = -1
        i = -list1
        j = -list2
120     k = i
        If (rv(i)>rv(j)) k = j
        If (ilist<0) Then
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
!       Move along the lists until one finishes.
!
!       New variables I2, J2 and K2 are used instead of I, J and K
!       within the innermost block to encourage optimising compilers to
!       store them in registers.
!        I2 points to the current element in the first list
!        J2 points to the current element in the second list
!        K2 points to the current element in the merged list
!
        i2 = i
        j2 = j
        If (k/=i2) Go To 150
130     a = rv(j2)
        k2 = i2
140     i2 = k2
        k2 = irank(i2)
        If (k2<=0) Go To 170
        If (a>=rv(k2)) Go To 140
        irank(i2) = j2
        i2 = k2
150     a = rv(i2)
        k2 = j2
160     j2 = k2
        k2 = irank(j2)
        If (k2<=0) Go To 180
        If (a>rv(k2)) Go To 160
        irank(j2) = i2
        j2 = k2
        Go To 130
!
!       Add the remains of one list to the other.
!
170     k = 1
        i1 = k2
        Go To 190
180     k = 2
        j1 = k2
190     i = i2
        j = j2
        If (k==1) Then
!
!         First list is exhausted
!
          irank(i) = j
          i = -i1
          j1 = j
200       j = j1
          j1 = irank(j)
          If (j1>0) Go To 200
          l = j
          j = -j1
        Else
!
!         Second list is exhausted
!
          irank(j) = i
          j = -j1
          i1 = i
210       i = i1
          i1 = irank(i)
          If (i1>0) Go To 210
          l = i
          i = -i1
        End If
!
!       Tidy up and carry on if not finished.
!
        If ((i/=0) .And. (j/=0)) Go To 120
        irank(l) = 0
        k = i + j
        If (ilist>0) Then
          irank(nlast) = -k
          Go To 110
        Else If (k/=0) Then
          list2 = -k
          Go To 110
        End If
!
!       If descending, reverse all pointers between equality
!       blocks.
!
220     If ((order=='D') .Or. (order=='d')) Then
          i = 0
          j = -list1
230       k = j
          k1 = k
          a = rv(k)
240       k = k1
          k1 = irank(k)
          If (k1/=0) Then
            If (a==rv(k1)) Go To 240
          End If
          irank(k) = i
          i = j
          j = k1
          If (j/=0) Go To 230
          list1 = -i
        End If
!
!       Convert the list form to ranks and return.
!
        k = m1
        i = -list1
250     i1 = irank(i)
        irank(i) = k
        k = k + 1
        i = i1
        If (i>0) Go To 250
!
      End If
!
      If (ierr/=0) Then
        ifail = p01abf(ifail, ierr, srname, nrec, p01rec)
      Else
        ifail = 0
      End If
      Return
!
260   Format (' ** On entry, one or more of the following parameter va', &
        'lues is illegal', /, '    M1 =', I16, '  M2 =', I16)
270   Format (' ** On entry, ORDER has an illegal value: ORDER = ', A1)
    End Subroutine
