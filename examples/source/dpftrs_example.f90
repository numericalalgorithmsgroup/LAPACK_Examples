    Program dpftrs_example

!     DPFTRS Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dpftrf, dpftrs
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, k, lar1, ldb, lenar, n, nrhs, q
      Character (1) :: transr, uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ar(:), b(:, :)
!     .. Executable Statements ..
      Write (nout, *) 'DPFTRS Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, nrhs, uplo, transr

      lenar = n*(n+1)/2
      ldb = n
      Allocate (ar(lenar), b(ldb,nrhs))

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

!     Read RHS matrix B
      Do i = 1, n
        Read (nin, *) b(i, 1:nrhs)
      End Do

!     Factorize A
      Call dpftrf(transr, uplo, n, ar, info)

      Write (nout, *)
      Flush (nout)
      If (info==0) Then

!       Compute solution
        Call dpftrs(transr, uplo, n, nrhs, ar, b, ldb, info)

!       Print solution
        ifail = 0
        Call nagf_file_print_matrix_real_gen('General', ' ', n, nrhs, b, ldb, &
          'Solution(s)', ifail)

      Else
        Write (nout, *) 'A is not positive definite'
      End If

    End Program
