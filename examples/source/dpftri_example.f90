    Program dpftri_example

!     DPFTRI Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dpftrf, dpftri, dtfttr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, k, lar1, lda, lenar, n, q
      Character (1) :: transr, uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), ar(:)
!     .. Executable Statements ..
      Write (nout, *) 'DPFTRI Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, uplo, transr

      lenar = n*(n+1)/2
      lda = n
      Allocate (ar(lenar), a(lda,n))

!     Setup notional dimensions of RFP matrix AR
      k = n/2
      q = n - k
      If (transr=='N' .Or. transr=='n') Then
        lar1 = 2*k + 1
      Else
        lar1 = q
      End If

!     Read an RFP matrix into array AR
      Do i = 1, lar1
        Read (nin, *) ar(i:lenar:lar1)
      End Do

!     Factorize A
      Call dpftrf(transr, uplo, n, ar, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute inverse of A
        Call dpftri(transr, uplo, n, ar, info)

!       Convert inverse to full array form, and print it
        Call dtfttr(transr, uplo, n, ar, a, lda, info)
        ifail = 0
        Call nagf_file_print_matrix_real_gen(uplo, 'Nonunit', n, n, a, lda, &
          'Inverse', ifail)

      Else
        Write (nout, *) 'A is not positive definite'
      End If

    End Program
