    Program zpstrf_example

!     ZPSTRF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp, &
        nagf_file_print_matrix_integer_comp
      Use lapack_interfaces, Only: zpstrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: zero = (0.0E0_dp, 0.0E0_dp)
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: tol
      Integer :: i, ifail, info, j, lda, n, rank
      Character (1) :: uplo
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :)
      Real (Kind=dp), Allocatable :: work(:)
      Integer, Allocatable :: piv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZPSTRF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, uplo
      lda = n
      Allocate (a(lda,n), piv(n), work(2*n))

!     Read A from data file
      If (uplo=='U') Then
        Read (nin, *)(a(i,i:n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)(a(i,1:i), i=1, n)
      End If
      tol = -1.0_dp

!     Factorize A
      Call zpstrf(uplo, n, a, lda, piv, rank, tol, work, info)

!     Zero out columns rank+1 to n
      If (uplo=='U') Then
        Do j = rank + 1, n
          a(rank+1:j, j) = zero
        End Do
      Else If (uplo=='L') Then
        Do j = rank + 1, n
          a(j:n, j) = zero
        End Do
      End If

!     Print rank
      Write (nout, *)
      Write (nout, '(1X,A15,I3)') 'Computed rank: ', rank

!     Print factor
      Write (nout, *)
      Flush (nout)
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp(uplo, 'Nonunit', n, n, a, &
        lda, 'Bracketed', 'F5.2', 'Factor', 'Integer', rlabs, 'Integer', &
        clabs, 80, 0, ifail)

!     Print pivot indices
      Write (nout, *)
      Write (nout, *) 'PIV'
      Flush (nout)
      ifail = 0
      Call nagf_file_print_matrix_integer_comp('General', 'Non-unit', 1, n, &
        piv, 1, 'I14', ' ', 'No', rlabs, 'No', clabs, 80, 1, ifail)

    End Program
