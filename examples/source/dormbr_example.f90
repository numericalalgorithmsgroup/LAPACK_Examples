    Program dormbr_example

!     DORMBR Example Program Text

!     Copyright 2017, Numerical Algorithms Group Ltd. http://www.nag.com

!     .. Use Statements ..
      Use lapack_example_aux, Only: nagf_file_print_matrix_real_gen
      Use lapack_interfaces, Only: dgebrd, dgelqf, dgeqrf, dlacpy, dlaset, &
        dorglq, dorgqr, dormbr
      Use lapack_precision, Only: dp
!     .. Implicit None Statement ..
      Implicit None
!     .. Parameters ..
      Real (Kind=dp), Parameter :: zero = 0.0E0_dp
      Integer, Parameter :: nin = 5, nout = 6
!     .. Local Scalars ..
      Integer :: i, ic, ifail, info, lda, ldpt, ldu, lwork, m, n
!     .. Local Arrays ..
      Real (Kind=dp), Allocatable :: a(:, :), d(:), e(:), pt(:, :), tau(:), &
        taup(:), tauq(:), u(:, :), work(:)
!     .. Executable Statements ..
      Write (nout, *) 'DORMBR Example Program Results'
!     Skip heading in data file
      Read (nin, *)
      Do ic = 1, 2
        Read (nin, *) m, n
        lda = m
        ldpt = n
        ldu = m
        lwork = 64*(m+n)
        Allocate (a(lda,n), d(n), e(n-1), pt(ldpt,n), tau(n), taup(n), &
          tauq(n), u(ldu,n), work(lwork))

!       Read A from data file

        Read (nin, *)(a(i,1:n), i=1, m)

        If (m>=n) Then

!         Compute the QR factorization of A
          Call dgeqrf(m, n, a, lda, tau, work, lwork, info)

!         Copy A to U
          Call dlacpy('Lower', m, n, a, lda, u, ldu)

!         Form Q explicitly, storing the result in U
          Call dorgqr(m, n, n, u, ldu, tau, work, lwork, info)

!         Copy R to PT (used as workspace)
          Call dlacpy('Upper', n, n, a, lda, pt, ldpt)

!         Set the strictly lower triangular part of R to zero
          Call dlaset('Lower', n-1, n-1, zero, zero, pt(2,1), ldpt)

!         Bidiagonalize R
          Call dgebrd(n, n, pt, ldpt, d, e, tauq, taup, work, lwork, info)

!         Update Q, storing the result in U
          Call dormbr('Q', 'Right', 'No transpose', m, n, n, pt, ldpt, tauq, &
            u, ldu, work, lwork, info)

!         Print bidiagonal form and matrix Q

          Write (nout, *)
          Write (nout, *) 'Example 1: bidiagonal matrix B'
          Write (nout, *) 'Diagonal'
          Write (nout, 100) d(1:n)
          Write (nout, *) 'Superdiagonal'
          Write (nout, 100) e(1:n-1)
          Write (nout, *)
          Flush (nout)

!         ifail: behaviour on error exit
!                =0 for hard exit, =1 for quiet-soft, =-1 for noisy-soft
          ifail = 0
          Call nagf_file_print_matrix_real_gen('General', ' ', m, n, u, ldu, &
            'Example 1: matrix Q', ifail)

        Else

!         Compute the LQ factorization of A
          Call dgelqf(m, n, a, lda, tau, work, lwork, info)

!         Copy A to PT
          Call dlacpy('Upper', m, n, a, lda, pt, ldpt)

!         Form Q explicitly, storing the result in PT
          Call dorglq(n, n, m, pt, ldpt, tau, work, lwork, info)

!         Copy L to U (used as workspace)
          Call dlacpy('Lower', m, m, a, lda, u, ldu)

!         Set the strictly upper triangular part of L to zero
          Call dlaset('Upper', m-1, m-1, zero, zero, u(1,2), ldu)

!         Bidiagonalize L
          Call dgebrd(m, m, u, ldu, d, e, tauq, taup, work, lwork, info)

!         Update P**T, storing the result in PT
          Call dormbr('P', 'Left', 'Transpose', m, n, m, u, ldu, taup, pt, &
            ldpt, work, lwork, info)

!         Print bidiagonal form and matrix P**T

          Write (nout, *)
          Write (nout, *) 'Example 2: bidiagonal matrix B'
          Write (nout, *) 'Diagonal'
          Write (nout, 100) d(1:m)
          Write (nout, *) 'Superdiagonal'
          Write (nout, 100) e(1:m-1)
          Write (nout, *)
          Flush (nout)

          ifail = 0
          Call nagf_file_print_matrix_real_gen('General', ' ', m, n, pt, ldpt, &
            'Example 2: matrix P**T', ifail)

        End If
        Deallocate (a, d, e, pt, tau, taup, tauq, u, work)
      End Do

100   Format (3X, (8F8.4))
    End Program
