    Program zgehrd_example

!     ZGEHRD Example Program Text

!     Copyright (c) 2018, Numerical Algorithms Group (NAG Ltd.)
!     For licence see
!       https://github.com/numericalalgorithmsgroup/LAPACK_Examples/blob/master/LICENCE.md

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: zgehrd
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Complex (Kind=dp), Parameter :: zero = (0.0E0_dp, 0.0E0_dp)
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, lda, lwork, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: a(:, :), tau(:), work(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZGEHRD Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      lda = n
      lwork = 64*n
      Allocate (a(lda,n), tau(n-1), work(lwork))

!     Read A from data file

      Read (nin, *)(a(i,1:n), i=1, n)

!     Reduce A to upper Hessenberg form

      Call zgehrd(n, 1, n, a, lda, tau, work, lwork, info)

!     Set the elements below the first subdiagonal to zero

      Do i = 1, n - 2
        a(i+2:n, i) = zero
      End Do

!     Print upper Hessenberg form

      Write (nout, *)
      Flush (nout)

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, a, &
        lda, 'Bracketed', 'F7.4', 'Upper Hessenberg form', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

    End Program
