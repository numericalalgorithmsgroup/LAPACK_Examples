    Program dsptrf_example

!     DSPTRF Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_packed
      Use lapack_interfaces, Only: dsptrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, n
      Character (1) :: uplo
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: ap(:)
      Integer, Allocatable :: ipiv(:)
!     .. Executable Statements ..
      Write (nout, *) 'DSPTRF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n

      Allocate (ap(n*(n+1)/2), ipiv(n))

!     Read A from data file

      Read (nin, *) uplo
      If (uplo=='U') Then
        Read (nin, *)((ap(i+j*(j-1)/2),j=i,n), i=1, n)
      Else If (uplo=='L') Then
        Read (nin, *)((ap(i+(2*n-j)*(j-1)/2),j=1,i), i=1, n)
      End If

!     Factorize A
      Call dsptrf(uplo, n, ap, ipiv, info)

      Write (nout, *)
      Flush (nout)

!     Print details of factorization

!     ifail: behaviour on error exit
!            =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_real_packed(uplo, 'Nonunit', n, ap, &
        'Details of factorization', ifail)

!     Print pivot indices

      Write (nout, *)
      Write (nout, *) 'IPIV'
      Write (nout, 100) ipiv(1:n)

      If (info/=0) Then
        Write (nout, *) 'The factor D is singular'
      End If

100   Format ((3X,7I11))
    End Program
