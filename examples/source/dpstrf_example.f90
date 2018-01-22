    Program dpstrf_example

!     DPSTRF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_integer_comp, &
        nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dpstrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Real (Kind=dp) :: tol
      Integer :: i, ifail, info, j, lda, n, rank
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), work(:)
      Integer, Allocatable :: piv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'DPSTRF Example Program Results'
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
      Call dpstrf(uplo, n, a, lda, piv, rank, tol, work, info)

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
      Call nagf_file_print_matrix_real_gen(uplo, 'Nonunit', n, n, a, lda, &
        'Factor', ifail)

!     Print pivot indices
      Write (nout, *)
      Write (nout, *) 'PIV'
      Flush (nout)

      ifail = 0
      Call nagf_file_print_matrix_integer_comp('General', 'Non-unit', 1, n, &
        piv, 1, 'I11', ' ', 'No', rlabs, 'No', clabs, 80, 1, ifail)

    End Program
