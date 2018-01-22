    Program zgbsv_example

!     ZGBSV Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_band_comp
      Use lapack_interfaces, Only: zgbsv
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, info, j, k, kl, ku, ldab, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: ab(:, :), b(:)
      Integer, Allocatable :: ipiv(:)
      Character (1) :: clabs(1), rlabs(1)
!     .. Intrinsic Procedures ..
      Intrinsic :: max, min
!     .. Executable Statements ..
      Write (nout, *) 'ZGBSV Example Program Results'
      Write (nout, *)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n, kl, ku
      ldab = 2*kl + ku + 1
      Allocate (ab(ldab,n), b(n), ipiv(n))

!     Read the band matrix A and the right hand side b from data file

      k = kl + ku + 1
      Read (nin, *)((ab(k+i-j,j),j=max(i-kl,1),min(i+ku,n)), i=1, n)
      Read (nin, *) b(1:n)

!     Solve the equations Ax = b for x
      Call zgbsv(n, kl, ku, 1, ab, ldab, ipiv, b, n, info)

      If (info==0) Then

!       Print solution

        Write (nout, *) 'Solution'
        Write (nout, 100) b(1:n)

!       Print details of the factorization

        Write (nout, *)
        Flush (nout)

!       ifail: behaviour on error exit
!              =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
        ifail = 0
        Call nagf_file_print_matrix_complex_band_comp(n, n, kl, kl+ku, ab, &
          ldab, 'Bracketed', 'F7.4', 'Details of factorization', 'None', &
          rlabs, 'None', clabs, 80, 0, ifail)

!       Print pivot indices'

        Write (nout, *)
        Write (nout, *) 'Pivot indices'
        Write (nout, 110) ipiv(1:n)

      Else
        Write (nout, 120) 'The (', info, ',', info, ')', &
          ' element of the factor U is zero'
      End If

100   Format ((3X,4(' (',F7.4,',',F7.4,')',:)))
110   Format (1X, I5, 3I18)
120   Format (1X, A, I3, A, I3, A, A)
    End Program
