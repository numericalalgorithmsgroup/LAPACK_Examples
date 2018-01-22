    Program dorgbr_example

!     DORGBR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dbdsqr, dgebrd, dlacpy, dorgbr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ic, ifail, info, lda, ldc, ldu, ldvt, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), c(:, :), d(:), e(:), taup(:), &
        tauq(:), u(:, :), vt(:, :), work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DORGBR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Do ic = 1, 2
        Read (nin, *) m, n
        lda = m
        ldc = n
        ldu = m
        ldvt = n
        lwork = 64*(m+n)
        Allocate (a(lda,n), c(ldc,n), d(n), e(n-1), taup(n), tauq(n), &
          u(ldu,n), vt(ldvt,n), work(lwork))

!       Read A from data file

        Read (nin, *)(a(i,1:n), i=1, m)

!       Reduce A to bidiagonal form
        Call dgebrd(m, n, a, lda, d, e, tauq, taup, work, lwork, info)

        If (m>=n) Then

!         Copy A to VT and U

          Call dlacpy('Upper', n, n, a, lda, vt, ldvt)
          Call dlacpy('Lower', m, n, a, lda, u, ldu)

!         Form P**T explicitly, storing the result in VT
          Call dorgbr('P', n, n, m, vt, ldvt, taup, work, lwork, info)

!         Form Q explicitly, storing the result in U
          Call dorgbr('Q', m, n, n, u, ldu, tauq, work, lwork, info)

!         Compute the SVD of A
          Call dbdsqr('Upper', n, n, m, 0, d, e, vt, ldvt, u, ldu, c, ldc, &
            work, info)

!         Print singular values, left & right singular vectors

          Write (nout, *)
          Write (nout, *) 'Example 1: singular values'
          Write (nout, 100) d(1:n)
          Write (nout, *)
          Flush (nout)

!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
          ifail = 0
          Call nagf_file_print_matrix_real_gen('General', ' ', n, n, vt, ldvt, &
            'Example 1: right singular vectors, by row', ifail)

          Write (nout, *)
          Flush (nout)

          Call nagf_file_print_matrix_real_gen('General', ' ', m, n, u, ldu, &
            'Example 1: left singular vectors, by column', ifail)

        Else

!         Copy A to VT and U

          Call dlacpy('Upper', m, n, a, lda, vt, ldvt)
          Call dlacpy('Lower', m, m, a, lda, u, ldu)

!         Form P**T explicitly, storing the result in VT
          Call dorgbr('P', m, n, m, vt, ldvt, taup, work, lwork, info)

!         Form Q explicitly, storing the result in U
          Call dorgbr('Q', m, m, n, u, ldu, tauq, work, lwork, info)

!         Compute the SVD of A
          Call dbdsqr('Lower', m, n, m, 0, d, e, vt, ldvt, u, ldu, c, ldc, &
            work, info)

!         Print singular values, left & right singular vectors

          Write (nout, *)
          Write (nout, *) 'Example 2: singular values'
          Write (nout, 100) d(1:m)
          Write (nout, *)
          Flush (nout)

          ifail = 0
          Call nagf_file_print_matrix_real_gen('General', ' ', m, n, vt, ldvt, &
            'Example 2: right singular vectors, by row', ifail)

          Write (nout, *)
          Flush (nout)

          Call nagf_file_print_matrix_real_gen('General', ' ', m, m, u, ldu, &
            'Example 2: left singular vectors, by column', ifail)

        End If
        Deallocate (a, c, d, e, taup, tauq, u, vt, work)
      End Do

100   Format (3X, (8F8.4))
    End Program
