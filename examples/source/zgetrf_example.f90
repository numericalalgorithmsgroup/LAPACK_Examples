    Program zgetrf_example

!     ZGETRF Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgetrf
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, m, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: min
!     .. Executable Statements ..
      Write (nout, *) 'ZGETRF Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) m, n
      lda = m
      Allocate (a(lda,n), ipiv(n))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, m)

!     Factorize A

      Call zgetrf(m, n, a, lda, ipiv, info)

!     Print details of factorization

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', m, n, a, &
        lda, 'Bracketed', 'F7.4', 'Details of factorization', 'Integer', &
        rlabs, 'Integer', clabs, 80, 0, ifail)

!     Print pivot indices

      Write (nout, *)
      Write (nout, *) 'IPIV'
      Write (nout, 100) ipiv(1:min(m,n))

      If (info/=0) Then
        Write (nout, *) 'The factor U is singular'
      End If

100   Format ((1X,I12,3I18))
    End Program
