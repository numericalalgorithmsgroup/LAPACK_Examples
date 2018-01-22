    Program ztrexc_example

!     ZTREXC Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_complex_gen_comp
      Use lapack_interfaces, Only: ztrexc
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ifail, ifst, ilst, info, ldq, ldt, n
!     .. Local Arrays ..
      Complex (Kind=dp), Allocatable :: q(:, :), t(:, :)
      Character (1) :: clabs(1), rlabs(1)
!     .. Executable Statements ..
      Write (nout, *) 'ZTREXC Example Program Results'
      Write (nout, *)
      Flush (nout)
!     Skip heading in data file
      Read (nin, *)
      Read (nin, *) n
      ldq = 1
      ldt = n
      Allocate (q(ldq,1), t(ldt,n))

!     Read T from data file

      Read (nin, *)(t(i,1:n), i=1, n)

!     Read the row indices

      Read (nin, *) ifst, ilst

!     Reorder the Schur factor T

      Call ztrexc('No update', n, t, ldt, q, ldq, ifst, ilst, info)
      If (info/=0) Then
        Write (nout, 100) info, ilst
        Write (nout, *)
        Flush (nout)
      End If

!     Print reordered Schur form

!     ifail: behaviour on error exit
!             =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
      ifail = 0
      Call nagf_file_print_matrix_complex_gen_comp('General', ' ', n, n, t, &
        ldt, 'Bracketed', 'F7.4', 'Reordered Schur form', 'Integer', rlabs, &
        'Integer', clabs, 80, 0, ifail)

100   Format (' Reordering could not be completed. INFO = ', I3, ' ILST = ', &
        I5)
    End Program
