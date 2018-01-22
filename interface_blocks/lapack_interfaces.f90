    Module lapack_interfaces

!     NAG COPYRIGHT 2017.

!     .. Implicit None Statement ..
      Implicit None
!     .. Interface Blocks ..
      Interface
        Subroutine dtrttp(uplo, n, a, lda, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: ap(n*(n+1)/2)
        End Subroutine
        Subroutine ztrttp(uplo, n, a, lda, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: ap(n*(n+1)/2)
        End Subroutine
        Subroutine dtpttr(uplo, n, ap, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: ap(n*(n+1)/2)
        End Subroutine
        Subroutine ztpttr(uplo, n, ap, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: ap(n*(n+1)/2)
        End Subroutine
        Subroutine dtrttf(transr, uplo, n, a, lda, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine ztrttf(transr, uplo, n, a, lda, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine dtfttr(transr, uplo, n, ar, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine ztfttr(transr, uplo, n, ar, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine dtpttf(transr, uplo, n, ap, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(n*(n+1)/2)
          Real (Kind=dp), Intent (Out) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine ztpttf(transr, uplo, n, ap, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(n*(n+1)/2)
          Complex (Kind=dp), Intent (Out) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine dtfttp(transr, uplo, n, ar, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Out) :: ap(n*(n+1)/2)
          Real (Kind=dp), Intent (In) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine ztfttp(transr, uplo, n, ar, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Out) :: ap(n*(n+1)/2)
          Complex (Kind=dp), Intent (In) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine zrot(n, cx, incx, cy, incy, c, s)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: s
          Real (Kind=dp), Intent (In) :: c
          Integer, Intent (In) :: incx, incy, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: cx(*), cy(*)
        End Subroutine
        Function dlansf(norm, transr, uplo, n, a, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: dlansf
!         .. Scalar Arguments ..
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: norm, transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(n*(n+1)/2)
          Real (Kind=dp), Intent (Inout) :: work(*)
        End Function
        Subroutine dtfsm(transr, side, uplo, trans, diag, m, n, alpha, a, b, &
          ldb)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: ldb, m, n
          Character (1), Intent (In) :: diag, side, trans, transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(*)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dsfrk(transr, uplo, trans, n, k, alpha, a, lda, beta, c)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, n
          Character (1), Intent (In) :: trans, transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: c(n*(n+1)/2)
        End Subroutine
        Function zlanhf(norm, transr, uplo, n, a, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Function Return Value ..
          Real (Kind=dp) :: zlanhf
!         .. Scalar Arguments ..
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: norm, transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(n*(n+1)/2)
          Real (Kind=dp), Intent (Inout) :: work(*)
        End Function
        Subroutine ztfsm(transr, side, uplo, trans, diag, m, n, alpha, a, b, &
          ldb)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Complex (Kind=dp), Intent (In) :: alpha
          Integer, Intent (In) :: ldb, m, n
          Character (1), Intent (In) :: diag, side, trans, transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(*)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine zhfrk(transr, uplo, trans, n, k, alpha, a, lda, beta, c)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: alpha, beta
          Integer, Intent (In) :: k, lda, n
          Character (1), Intent (In) :: trans, transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: c(n*(n+1)/2)
        End Subroutine
        Subroutine dgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine dgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
          r, c, b, ldb, x, ldx, rcond, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), af(ldaf, *), b(ldb, *), &
            c(*), r(*), x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), &
            work(max(1,4*n))
          Integer, Intent (Inout) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsgesv(n, nrhs, a, lda, ipiv, b, ldb, x, ldx, work, swork, &
          iter, info)
!         .. Use Statements ..
          Use lapack_precision, Only: sp, dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, iter
          Integer, Intent (In) :: lda, ldb, ldx, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), x(ldx, *)
          Real (Kind=dp), Intent (In) :: b(ldb, *)
          Real (Kind=sp), Intent (Out) :: swork(n*(n+nrhs))
          Real (Kind=dp), Intent (Out) :: work(n*nrhs)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine dgetrf(m, n, a, lda, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Integer, Intent (Out) :: ipiv(min(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: min
        End Subroutine
        Subroutine dgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dgees(jobvs, sort, select, n, a, lda, sdim, wr, wi, vs, &
          ldvs, work, lwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldvs, lwork, n
          Character (1), Intent (In) :: jobvs, sort
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), vs(ldvs, *), wi(*), &
            wr(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function select(wr, wi)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: select
!             .. Scalar Arguments ..
              Real (Kind=dp), Intent (In) :: wi, wr
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgeesx(jobvs, sort, select, sense, n, a, lda, sdim, wr, wi, &
          vs, ldvs, rconde, rcondv, work, lwork, iwork, liwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rconde, rcondv
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldvs, liwork, lwork, n
          Character (1), Intent (In) :: jobvs, sense, sort
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), vs(ldvs, *), wi(*), &
            wr(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function select(wr, wi)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: select
!             .. Scalar Arguments ..
              Real (Kind=dp), Intent (In) :: wi, wr
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgeequ(m, n, a, lda, r, c, rowcnd, colcnd, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, colcnd, rowcnd
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: c(n), r(m)
        End Subroutine
        Subroutine dgecon(norm, n, a, lda, anorm, rcond, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: norm
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(4*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dgerfs(trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, &
          ldx, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), af(ldaf, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dgetri(n, a, lda, ipiv, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (In) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgesv(n, nrhs, a, lda, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zgesvx(fact, trans, n, nrhs, a, lda, af, ldaf, ipiv, equed, &
          r, c, b, ldb, x, ldx, rcond, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), af(ldaf, *), &
            b(ldb, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), &
            rwork(max(1,2*n))
          Real (Kind=dp), Intent (Inout) :: c(*), r(*)
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zcgesv(n, nrhs, a, lda, ipiv, b, ldb, x, ldx, work, swork, &
          rwork, iter, info)
!         .. Use Statements ..
          Use lapack_precision, Only: sp, dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, iter
          Integer, Intent (In) :: lda, ldb, ldx, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), x(ldx, *)
          Complex (Kind=dp), Intent (In) :: b(ldb, *)
          Complex (Kind=sp), Intent (Out) :: swork(n*(n+nrhs))
          Complex (Kind=dp), Intent (Out) :: work(n*nrhs)
          Real (Kind=dp), Intent (Out) :: rwork(n)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zgetrf(m, n, a, lda, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Integer, Intent (Out) :: ipiv(min(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: min
        End Subroutine
        Subroutine zgetrs(trans, n, nrhs, a, lda, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zgeequ(m, n, a, lda, r, c, rowcnd, colcnd, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, colcnd, rowcnd
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: c(n), r(m)
        End Subroutine
        Subroutine zgees(jobvs, sort, select, n, a, lda, sdim, w, vs, ldvs, &
          work, lwork, rwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldvs, lwork, n
          Character (1), Intent (In) :: jobvs, sort
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), vs(ldvs, *), w(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function select(w)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: select
!             .. Scalar Arguments ..
              Complex (Kind=dp), Intent (In) :: w
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgeesx(jobvs, sort, select, sense, n, a, lda, sdim, w, vs, &
          ldvs, rconde, rcondv, work, lwork, rwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rconde, rcondv
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldvs, lwork, n
          Character (1), Intent (In) :: jobvs, sense, sort
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), vs(ldvs, *), w(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function select(w)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: select
!             .. Scalar Arguments ..
              Complex (Kind=dp), Intent (In) :: w
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgecon(norm, n, a, lda, anorm, rcond, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: norm
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(2*n)
        End Subroutine
        Subroutine zgerfs(trans, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, &
          ldx, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), af(ldaf, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zgetri(n, a, lda, ipiv, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (In) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgbsv(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldb, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), b(ldb, *)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine dgbsvx(fact, trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, &
          ipiv, equed, r, c, b, ldb, x, ldx, rcond, ferr, berr, work, iwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), afb(ldafb, *), &
            b(ldb, *), c(*), r(*), x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), &
            work(max(1,3*n))
          Integer, Intent (Inout) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgbtrf(m, n, kl, ku, ab, ldab, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *)
          Integer, Intent (Out) :: ipiv(min(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: min
        End Subroutine
        Subroutine dgbtrs(trans, n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, colcnd, rowcnd
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Out) :: c(n), r(m)
        End Subroutine
        Subroutine dgbcon(norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond, work, &
          iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, n
          Character (1), Intent (In) :: norm
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dgbrfs(trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv, &
          b, ldb, x, ldx, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *), afb(ldafb, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine zgbsv(n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldb, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), b(ldb, *)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zgbsvx(fact, trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, &
          ipiv, equed, r, c, b, ldb, x, ldx, rcond, ferr, berr, work, rwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), afb(ldafb, *), &
            b(ldb, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), &
            rwork(max(1,n))
          Real (Kind=dp), Intent (Inout) :: c(*), r(*)
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgbtrf(m, n, kl, ku, ab, ldab, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *)
          Integer, Intent (Out) :: ipiv(min(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: min
        End Subroutine
        Subroutine zgbtrs(trans, n, kl, ku, nrhs, ab, ldab, ipiv, b, ldb, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zgbequ(m, n, kl, ku, ab, ldab, r, c, rowcnd, colcnd, amax, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, colcnd, rowcnd
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Out) :: c(n), r(m)
        End Subroutine
        Subroutine zgbcon(norm, n, kl, ku, ab, ldab, ipiv, anorm, rcond, work, &
          rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, n
          Character (1), Intent (In) :: norm
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zgbrfs(trans, n, kl, ku, nrhs, ab, ldab, afb, ldafb, ipiv, &
          b, ldb, x, ldx, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *), afb(ldafb, *), &
            b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dgtsv(n, nrhs, dl, d, du, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: b(ldb, *), d(*), dl(*), du(*)
        End Subroutine
        Subroutine dgtsvx(fact, trans, n, nrhs, dl, d, du, dlf, df, duf, du2, &
          ipiv, b, ldb, x, ldx, rcond, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: fact, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: b(ldb, *), d(*), dl(*), du(*)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: df(*), dlf(*), du2(*), duf(*), &
            x(ldx, *)
          Integer, Intent (Inout) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dgttrf(n, dl, d, du, du2, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), dl(*), du(*)
          Real (Kind=dp), Intent (Out) :: du2(n-2)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine dgttrs(trans, n, nrhs, dl, d, du, du2, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
          Real (Kind=dp), Intent (In) :: d(*), dl(*), du(*), du2(*)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dgtcon(norm, n, dl, d, du, du2, ipiv, anorm, rcond, work, &
          iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: norm
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: d(*), dl(*), du(*), du2(*)
          Real (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dgtrfs(trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv, &
          b, ldb, x, ldx, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: b(ldb, *), d(*), df(*), dl(*), &
            dlf(*), du(*), du2(*), duf(*)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine zgtsv(n, nrhs, dl, d, du, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *), d(*), dl(*), du(*)
        End Subroutine
        Subroutine zgtsvx(fact, trans, n, nrhs, dl, d, du, dlf, df, duf, du2, &
          ipiv, b, ldb, x, ldx, rcond, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: fact, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: b(ldb, *), d(*), dl(*), du(*)
          Complex (Kind=dp), Intent (Inout) :: df(*), dlf(*), du2(*), duf(*), &
            x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (Inout) :: ipiv(*)
        End Subroutine
        Subroutine zgttrf(n, dl, d, du, du2, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: d(*), dl(*), du(*)
          Complex (Kind=dp), Intent (Out) :: du2(n-2)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zgttrs(trans, n, nrhs, dl, d, du, du2, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Complex (Kind=dp), Intent (In) :: d(*), dl(*), du(*), du2(*)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zgtcon(norm, n, dl, d, du, du2, ipiv, anorm, rcond, work, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: norm
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: d(*), dl(*), du(*), du2(*)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zgtrfs(trans, n, nrhs, dl, d, du, dlf, df, duf, du2, ipiv, &
          b, ldb, x, ldx, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: b(ldb, *), d(*), df(*), dl(*), &
            dlf(*), du(*), du2(*), duf(*)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dposv(uplo, n, nrhs, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
        End Subroutine
        Subroutine dposvx(fact, uplo, n, nrhs, a, lda, af, ldaf, equed, s, b, &
          ldb, x, ldx, rcond, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), af(ldaf, *), b(ldb, *), &
            s(*), x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dsposv(uplo, n, nrhs, a, lda, b, ldb, x, ldx, work, swork, &
          iter, info)
!         .. Use Statements ..
          Use lapack_precision, Only: sp, dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, iter
          Integer, Intent (In) :: lda, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), x(ldx, *)
          Real (Kind=dp), Intent (In) :: b(ldb, *)
          Real (Kind=sp), Intent (Out) :: swork(n*(n+nrhs))
          Real (Kind=dp), Intent (Out) :: work(n, nrhs)
        End Subroutine
        Subroutine dpotrf(uplo, n, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
        End Subroutine
        Subroutine dpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dpoequ(n, a, lda, s, scond, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, scond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: s(n)
        End Subroutine
        Subroutine dpocon(uplo, n, a, lda, anorm, rcond, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dporfs(uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx, &
          ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), af(ldaf, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dpotri(uplo, n, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
        End Subroutine
        Subroutine zposv(uplo, n, nrhs, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
        End Subroutine
        Subroutine zposvx(fact, uplo, n, nrhs, a, lda, af, ldaf, equed, s, b, &
          ldb, x, ldx, rcond, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), af(ldaf, *), &
            b(ldb, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Real (Kind=dp), Intent (Inout) :: s(*)
        End Subroutine
        Subroutine zcposv(uplo, n, nrhs, a, lda, b, ldb, x, ldx, work, swork, &
          rwork, iter, info)
!         .. Use Statements ..
          Use lapack_precision, Only: sp, dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, iter
          Integer, Intent (In) :: lda, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), x(ldx, *)
          Complex (Kind=dp), Intent (In) :: b(ldb, *)
          Complex (Kind=sp), Intent (Out) :: swork(n*(n+nrhs))
          Complex (Kind=dp), Intent (Out) :: work(n, nrhs)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine zpotrf(uplo, n, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
        End Subroutine
        Subroutine zpotrs(uplo, n, nrhs, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine zpoequ(n, a, lda, s, scond, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, scond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: s(n)
        End Subroutine
        Subroutine zpocon(uplo, n, a, lda, anorm, rcond, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine zporfs(uplo, n, nrhs, a, lda, af, ldaf, b, ldb, x, ldx, &
          ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), af(ldaf, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
        End Subroutine
        Subroutine zpotri(uplo, n, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
        End Subroutine
        Subroutine dppsv(uplo, n, nrhs, ap, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), b(ldb, *)
        End Subroutine
        Subroutine dppsvx(fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb, x, &
          ldx, rcond, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: afp(*), ap(*), b(ldb, *), s(*), &
            x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dpptrf(uplo, n, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
        End Subroutine
        Subroutine dpptrs(uplo, n, nrhs, ap, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dppequ(uplo, n, ap, s, scond, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, scond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Out) :: s(n)
        End Subroutine
        Subroutine dppcon(uplo, n, ap, anorm, rcond, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dpprfs(uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr, &
          work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: afp(*), ap(*), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dpptri(uplo, n, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
        End Subroutine
        Subroutine zppsv(uplo, n, nrhs, ap, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), b(ldb, *)
        End Subroutine
        Subroutine zppsvx(fact, uplo, n, nrhs, ap, afp, equed, s, b, ldb, x, &
          ldx, rcond, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: afp(*), ap(*), b(ldb, *), &
            x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Real (Kind=dp), Intent (Inout) :: s(*)
        End Subroutine
        Subroutine zpptrf(uplo, n, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
        End Subroutine
        Subroutine zpptrs(uplo, n, nrhs, ap, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine zppequ(uplo, n, ap, s, scond, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, scond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Out) :: s(n)
        End Subroutine
        Subroutine zppcon(uplo, n, ap, anorm, rcond, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine zpprfs(uplo, n, nrhs, ap, afp, b, ldb, x, ldx, ferr, berr, &
          work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: afp(*), ap(*), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
        End Subroutine
        Subroutine zpptri(uplo, n, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
        End Subroutine
        Subroutine dpbsv(uplo, n, kd, nrhs, ab, ldab, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), b(ldb, *)
        End Subroutine
        Subroutine dpbsvx(fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb, &
          equed, s, b, ldb, x, ldx, rcond, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), afb(ldafb, *), &
            b(ldb, *), s(*), x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dpbtrf(uplo, n, kd, ab, ldab, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *)
        End Subroutine
        Subroutine dpbtrs(uplo, n, kd, nrhs, ab, ldab, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dpbequ(uplo, n, kd, ab, ldab, s, scond, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, scond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Out) :: s(n)
        End Subroutine
        Subroutine dpbcon(uplo, n, kd, ab, ldab, anorm, rcond, work, iwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dpbrfs(uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x, &
          ldx, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *), afb(ldafb, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine zpbsv(uplo, n, kd, nrhs, ab, ldab, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), b(ldb, *)
        End Subroutine
        Subroutine zpbsvx(fact, uplo, n, kd, nrhs, ab, ldab, afb, ldafb, &
          equed, s, b, ldb, x, ldx, rcond, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (Inout) :: equed
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), afb(ldafb, *), &
            b(ldb, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Real (Kind=dp), Intent (Inout) :: s(*)
        End Subroutine
        Subroutine zpbtrf(uplo, n, kd, ab, ldab, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *)
        End Subroutine
        Subroutine zpbtrs(uplo, n, kd, nrhs, ab, ldab, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine zpbequ(uplo, n, kd, ab, ldab, s, scond, amax, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: amax, scond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Out) :: s(n)
        End Subroutine
        Subroutine zpbcon(uplo, n, kd, ab, ldab, anorm, rcond, work, rwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine zpbrfs(uplo, n, kd, nrhs, ab, ldab, afb, ldafb, b, ldb, x, &
          ldx, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldafb, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *), afb(ldafb, *), &
            b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
        End Subroutine
        Subroutine dptsv(n, nrhs, d, e, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: b(ldb, *), d(*), e(*)
        End Subroutine
        Subroutine dptsvx(fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx, rcond, &
          ferr, berr, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: fact
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: b(ldb, *), d(*), e(*)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(2*n)
          Real (Kind=dp), Intent (Inout) :: df(*), ef(*), x(ldx, *)
        End Subroutine
        Subroutine dpttrf(n, d, e, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*)
        End Subroutine
        Subroutine dpttrs(n, nrhs, d, e, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
          Real (Kind=dp), Intent (In) :: d(*), e(*)
        End Subroutine
        Subroutine dptcon(n, d, e, anorm, rcond, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: d(*), e(*)
          Real (Kind=dp), Intent (Out) :: work(n)
        End Subroutine
        Subroutine dptrfs(n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr, berr, &
          work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: b(ldb, *), d(*), df(*), e(*), ef(*)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(2*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
        End Subroutine
        Subroutine zptsv(n, nrhs, d, e, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *), e(*)
          Real (Kind=dp), Intent (Inout) :: d(*)
        End Subroutine
        Subroutine zptsvx(fact, n, nrhs, d, e, df, ef, b, ldb, x, ldx, rcond, &
          ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: fact
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: b(ldb, *), e(*)
          Complex (Kind=dp), Intent (Inout) :: ef(*), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Real (Kind=dp), Intent (In) :: d(*)
          Real (Kind=dp), Intent (Inout) :: df(*)
        End Subroutine
        Subroutine zpttrf(n, d, e, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: e(*)
          Real (Kind=dp), Intent (Inout) :: d(*)
        End Subroutine
        Subroutine zpttrs(uplo, n, nrhs, d, e, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Complex (Kind=dp), Intent (In) :: e(*)
          Real (Kind=dp), Intent (In) :: d(*)
        End Subroutine
        Subroutine zptcon(n, d, e, anorm, rcond, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: e(*)
          Real (Kind=dp), Intent (In) :: d(*)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine zptrfs(uplo, n, nrhs, d, e, df, ef, b, ldb, x, ldx, ferr, &
          berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: b(ldb, *), e(*), ef(*)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Real (Kind=dp), Intent (In) :: d(*), df(*)
        End Subroutine
        Subroutine dpstrf(uplo, n, a, lda, piv, rank, tol, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tol
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (Out) :: piv(n)
        End Subroutine
        Subroutine zpstrf(uplo, n, a, lda, piv, rank, tol, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tol
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (Out) :: piv(n)
        End Subroutine
        Subroutine dsysv(uplo, n, nrhs, a, lda, ipiv, b, ldb, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsysvx(fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, &
          x, ldx, rcond, ferr, berr, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Inout) :: af(ldaf, *), berr(*), ferr(*), &
            x(ldx, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: ipiv(*), iwork(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsytrf(uplo, n, a, lda, ipiv, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsytrs(uplo, n, nrhs, a, lda, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dsycon(uplo, n, a, lda, ipiv, anorm, rcond, work, iwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dsyrfs(uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, &
          ldx, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), af(ldaf, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dsytri(uplo, n, a, lda, ipiv, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zhesv(uplo, n, nrhs, a, lda, ipiv, b, ldb, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhesvx(fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, &
          x, ldx, rcond, ferr, berr, work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: af(ldaf, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: berr(*), ferr(*), rwork(*)
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhetrf(uplo, n, a, lda, ipiv, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhetrs(uplo, n, nrhs, a, lda, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zhecon(uplo, n, a, lda, ipiv, anorm, rcond, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zherfs(uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, &
          ldx, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), af(ldaf, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zhetri(uplo, n, a, lda, ipiv, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zsysv(uplo, n, nrhs, a, lda, ipiv, b, ldb, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zsysvx(fact, uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, &
          x, ldx, rcond, ferr, berr, work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, lwork, n, nrhs
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: af(ldaf, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: berr(*), ferr(*), rwork(*)
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zsytrf(uplo, n, a, lda, ipiv, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: ipiv(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zsytrs(uplo, n, nrhs, a, lda, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zsycon(uplo, n, a, lda, ipiv, anorm, rcond, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zsyrfs(uplo, n, nrhs, a, lda, af, ldaf, ipiv, b, ldb, x, &
          ldx, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldaf, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), af(ldaf, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zsytri(uplo, n, a, lda, ipiv, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dspsv(uplo, n, nrhs, ap, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), b(ldb, *)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine dspsvx(fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, &
          rcond, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: afp(*), x(ldx, *)
          Real (Kind=dp), Intent (In) :: ap(*), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Integer, Intent (Inout) :: ipiv(n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dsptrf(uplo, n, ap, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine dsptrs(uplo, n, nrhs, ap, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dspcon(uplo, n, ap, ipiv, anorm, rcond, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dsprfs(uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr, &
          berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: afp(*), ap(*), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Real (Kind=dp), Intent (Inout) :: x(ldx, *)
          Integer, Intent (In) :: ipiv(*)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dsptri(uplo, n, ap, ipiv, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
          Real (Kind=dp), Intent (Out) :: work(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zhpsv(uplo, n, nrhs, ap, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), b(ldb, *)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zhpsvx(fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, &
          rcond, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: afp(*), x(ldx, *)
          Complex (Kind=dp), Intent (In) :: ap(*), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (Inout) :: ipiv(n)
        End Subroutine
        Subroutine zhptrf(uplo, n, ap, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zhptrs(uplo, n, nrhs, ap, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zhpcon(uplo, n, ap, ipiv, anorm, rcond, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zhprfs(uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr, &
          berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: afp(*), ap(*), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zhptri(uplo, n, ap, ipiv, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zspsv(uplo, n, nrhs, ap, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), b(ldb, *)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zspsvx(fact, uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, &
          rcond, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: fact, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: afp(*), x(ldx, *)
          Complex (Kind=dp), Intent (In) :: ap(*), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (Inout) :: ipiv(n)
        End Subroutine
        Subroutine zsptrf(uplo, n, ap, ipiv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Integer, Intent (Out) :: ipiv(n)
        End Subroutine
        Subroutine zsptrs(uplo, n, nrhs, ap, ipiv, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zspcon(uplo, n, ap, ipiv, anorm, rcond, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: anorm
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zsprfs(uplo, n, nrhs, ap, afp, ipiv, b, ldb, x, ldx, ferr, &
          berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: afp(*), ap(*), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Complex (Kind=dp), Intent (Inout) :: x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine zsptri(uplo, n, ap, ipiv, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Integer, Intent (In) :: ipiv(*)
        End Subroutine
        Subroutine dtrtrs(uplo, trans, diag, n, nrhs, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dtrcon(norm, uplo, diag, n, a, lda, rcond, work, iwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: diag, norm, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dtrrfs(uplo, trans, diag, n, nrhs, a, lda, b, ldb, x, ldx, &
          ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *), x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dtrtri(uplo, diag, n, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: diag, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
        End Subroutine
        Subroutine ztrtrs(uplo, trans, diag, n, nrhs, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine ztrcon(norm, uplo, diag, n, a, lda, rcond, work, rwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: diag, norm, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine ztrrfs(uplo, trans, diag, n, nrhs, a, lda, b, ldb, x, ldx, &
          ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
        End Subroutine
        Subroutine ztrtri(uplo, diag, n, a, lda, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: diag, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
        End Subroutine
        Subroutine dtptrs(uplo, trans, diag, n, nrhs, ap, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dtpcon(norm, uplo, diag, n, ap, rcond, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: diag, norm, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dtprfs(uplo, trans, diag, n, nrhs, ap, b, ldb, x, ldx, &
          ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*), b(ldb, *), x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dtptri(uplo, diag, n, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: diag, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
        End Subroutine
        Subroutine ztptrs(uplo, trans, diag, n, nrhs, ap, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine ztpcon(norm, uplo, diag, n, ap, rcond, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: diag, norm, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine ztprfs(uplo, trans, diag, n, nrhs, ap, b, ldb, x, ldx, &
          ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, ldx, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*), b(ldb, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
        End Subroutine
        Subroutine ztptri(uplo, diag, n, ap, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: diag, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
        End Subroutine
        Subroutine dtbtrs(uplo, trans, diag, n, kd, nrhs, ab, ldab, b, ldb, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dtbcon(norm, uplo, diag, n, kd, ab, ldab, rcond, work, &
          iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: diag, norm, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine dtbrfs(uplo, trans, diag, n, kd, nrhs, ab, ldab, b, ldb, x, &
          ldx, ferr, berr, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ab(ldab, *), b(ldb, *), x(ldx, *)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), work(3*n)
          Integer, Intent (Out) :: iwork(n)
        End Subroutine
        Subroutine ztbtrs(uplo, trans, diag, n, kd, nrhs, ab, ldab, b, ldb, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine ztbcon(norm, uplo, diag, n, kd, ab, ldab, rcond, work, &
          rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: rcond
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, n
          Character (1), Intent (In) :: diag, norm, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine ztbrfs(uplo, trans, diag, n, kd, nrhs, ab, ldab, b, ldb, x, &
          ldx, ferr, berr, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldb, ldx, n, nrhs
          Character (1), Intent (In) :: diag, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ab(ldab, *), b(ldb, *), x(ldx, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: berr(nrhs), ferr(nrhs), rwork(n)
        End Subroutine
        Subroutine dpftrf(transr, uplo, n, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine dpftrs(transr, uplo, n, nrhs, ar, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ar(n*(n+1)/2)
          Real (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine dpftri(transr, uplo, n, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine dtftri(transr, uplo, diag, n, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: diag, transr, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine zpftrf(transr, uplo, n, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine zpftrs(transr, uplo, n, nrhs, ar, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldb, n, nrhs
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ar(n*(n+1)/2)
          Complex (Kind=dp), Intent (Inout) :: b(ldb, *)
        End Subroutine
        Subroutine zpftri(transr, uplo, n, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine ztftri(transr, uplo, diag, n, ar, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: diag, transr, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ar(n*(n+1)/2)
        End Subroutine
        Subroutine dgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgeqrt(m, n, nb, a, lda, t, ldt, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldt, m, n, nb
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), t(ldt, *)
          Real (Kind=dp), Intent (Out) :: work(nb*n)
        End Subroutine
        Subroutine dgemqrt(side, trans, m, n, k, nb, v, ldv, t, ldt, c, ldc, &
          work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, ldc, ldt, ldv, m, n, nb
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: c(ldc, *), v(ldv, *), work(*)
          Real (Kind=dp), Intent (In) :: t(ldt, *)
        End Subroutine
        Subroutine dgeqrf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dorgqr(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormqr(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgelqf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dorglq(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormlq(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgels(trans, m, n, nrhs, a, lda, b, ldb, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgeqrt(m, n, nb, a, lda, t, ldt, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldt, m, n, nb
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), t(ldt, *)
          Complex (Kind=dp), Intent (Out) :: work(nb*n)
        End Subroutine
        Subroutine zgemqrt(side, trans, m, n, k, nb, v, ldv, t, ldt, c, ldc, &
          work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, ldc, ldt, ldv, m, n, nb
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *), v(ldv, *), work(*)
          Complex (Kind=dp), Intent (In) :: t(ldt, *)
        End Subroutine
        Subroutine zgeqrf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zungqr(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmqr(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgelqf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunglq(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmlq(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: rcond
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: jpvt(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dtpqrt(m, n, l, nb, a, lda, b, ldb, t, ldt, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: l, lda, ldb, ldt, m, n, nb
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), t(ldt, *), &
            work(*)
        End Subroutine
        Subroutine dtpmqrt(side, trans, m, n, k, l, nb, v, ldv, t, ldt, c1, &
          ldc1, c2, ldc2, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, l, ldc1, ldc2, ldt, ldv, m, n, nb
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: c1(ldc1, *), c2(ldc2, *), work(*)
          Real (Kind=dp), Intent (In) :: t(ldt, *), v(ldv, *)
        End Subroutine
        Subroutine dgeqpf(m, n, a, lda, jpvt, tau, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: tau(min(m,n)), work(3*n)
          Integer, Intent (Inout) :: jpvt(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: min
        End Subroutine
        Subroutine dgeqp3(m, n, a, lda, jpvt, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: jpvt(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dtzrzf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormrz(side, trans, m, n, k, l, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, l, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgelsy(m, n, nrhs, a, lda, b, ldb, jpvt, rcond, rank, work, &
          lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: rcond
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
          Integer, Intent (Inout) :: jpvt(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine ztpqrt(m, n, l, nb, a, lda, b, ldb, t, ldt, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: l, lda, ldb, ldt, m, n, nb
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            t(ldt, *), work(*)
        End Subroutine
        Subroutine ztpmqrt(side, trans, m, n, k, l, nb, v, ldv, t, ldt, c1, &
          ldc1, c2, ldc2, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, l, ldc1, ldc2, ldt, ldv, m, n, nb
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: c1(ldc1, *), c2(ldc2, *), &
            work(*)
          Complex (Kind=dp), Intent (In) :: t(ldt, *), v(ldv, *)
        End Subroutine
        Subroutine zgeqpf(m, n, a, lda, jpvt, tau, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: tau(min(m,n)), work(n)
          Real (Kind=dp), Intent (Out) :: rwork(2*n)
          Integer, Intent (Inout) :: jpvt(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: min
        End Subroutine
        Subroutine zgeqp3(m, n, a, lda, jpvt, tau, work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
          Integer, Intent (Inout) :: jpvt(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine ztzrzf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmrz(side, trans, m, n, k, l, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, l, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgeqlf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dorgql(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormql(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgerqf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dorgrq(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormrq(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgeqlf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zungql(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmql(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgerqf(m, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zungrq(m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmrq(side, trans, m, n, k, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsyev(jobz, uplo, n, a, lda, w, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsyevx(jobz, range, uplo, n, a, lda, vl, vu, il, iu, &
          abstol, m, w, z, ldz, work, lwork, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, lda, ldz, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), w(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: iwork(*), jfail(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsyevd(job, uplo, n, a, lda, w, work, lwork, iwork, liwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, liwork, lwork, n
          Character (1), Intent (In) :: job, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), w(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsyevr(jobz, range, uplo, n, a, lda, vl, vu, il, iu, &
          abstol, m, w, z, ldz, isuppz, work, lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, lda, ldz, liwork, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), w(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: isuppz(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsytrd(uplo, n, a, lda, d, e, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), d(*), e(*), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dorgtr(uplo, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormtr(side, uplo, trans, m, n, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine ddisna(job, m, n, d, sep, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: m, n
          Character (1), Intent (In) :: job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: d(*)
          Real (Kind=dp), Intent (Inout) :: sep(*)
        End Subroutine
        Subroutine zheev(jobz, uplo, n, a, lda, w, work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
          Real (Kind=dp), Intent (Out) :: w(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zheevx(jobz, range, uplo, n, a, lda, vl, vu, il, iu, &
          abstol, m, w, z, ldz, work, lwork, rwork, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, lda, ldz, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*), w(*)
          Integer, Intent (Inout) :: iwork(*), jfail(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zheevd(job, uplo, n, a, lda, w, work, lwork, rwork, lrwork, &
          iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, liwork, lrwork, lwork, n
          Character (1), Intent (In) :: job, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork))
          Real (Kind=dp), Intent (Inout) :: w(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zheevr(jobz, range, uplo, n, a, lda, vl, vu, il, iu, &
          abstol, m, w, z, ldz, isuppz, work, lwork, rwork, lrwork, iwork, &
          liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, lda, ldz, liwork, lrwork, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork))
          Real (Kind=dp), Intent (Inout) :: w(*)
          Integer, Intent (Inout) :: isuppz(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhetrd(uplo, n, a, lda, d, e, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: d(*), e(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zungtr(uplo, n, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmtr(side, uplo, trans, m, n, a, lda, tau, c, ldc, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dspev(jobz, uplo, n, ap, w, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(3*n)
        End Subroutine
        Subroutine dspevx(jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m, &
          w, z, ldz, work, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(8*n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine dspevd(job, uplo, n, ap, w, z, ldz, work, lwork, iwork, &
          liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, liwork, lwork, n
          Character (1), Intent (In) :: job, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), w(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsptrd(uplo, n, ap, d, e, tau, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
          Real (Kind=dp), Intent (Out) :: d(n), e(n-1), tau(n-1)
        End Subroutine
        Subroutine dopgtr(uplo, n, ap, tau, q, ldq, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldq, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: ap(*), tau(*)
          Real (Kind=dp), Intent (Inout) :: q(ldq, *)
          Real (Kind=dp), Intent (Out) :: work(n-1)
        End Subroutine
        Subroutine dopmtr(side, uplo, trans, m, n, ap, tau, c, ldc, work, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldc, m, n
          Character (1), Intent (In) :: side, trans, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), c(ldc, *), work(*)
          Real (Kind=dp), Intent (In) :: tau(*)
        End Subroutine
        Subroutine zhpev(jobz, uplo, n, ap, w, z, ldz, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n-1)
          Real (Kind=dp), Intent (Out) :: rwork(3*n-2), w(n)
        End Subroutine
        Subroutine zhpevx(jobz, range, uplo, n, ap, vl, vu, il, iu, abstol, m, &
          w, z, ldz, work, rwork, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(7*n), w(n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine zhpevd(job, uplo, n, ap, w, z, ldz, work, lwork, rwork, &
          lrwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, liwork, lrwork, lwork, n
          Character (1), Intent (In) :: job, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork))
          Real (Kind=dp), Intent (Inout) :: w(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhptrd(uplo, n, ap, d, e, tau, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Complex (Kind=dp), Intent (Out) :: tau(n-1)
          Real (Kind=dp), Intent (Out) :: d(n), e(n-1)
        End Subroutine
        Subroutine zupgtr(uplo, n, ap, tau, q, ldq, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldq, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: ap(*), tau(*)
          Complex (Kind=dp), Intent (Inout) :: q(ldq, *)
          Complex (Kind=dp), Intent (Out) :: work(n-1)
        End Subroutine
        Subroutine zupmtr(side, uplo, trans, m, n, ap, tau, c, ldc, work, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldc, m, n
          Character (1), Intent (In) :: side, trans, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), c(ldc, *), work(*)
          Complex (Kind=dp), Intent (In) :: tau(*)
        End Subroutine
        Subroutine dsbev(jobz, uplo, n, kd, ab, ldab, w, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(3*n-2)
        End Subroutine
        Subroutine dsbevx(jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl, vu, &
          il, iu, abstol, m, w, z, ldz, work, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, kd, ldab, ldq, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), q(ldq, *), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(7*n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine dsbevd(job, uplo, n, kd, ab, ldab, w, z, ldz, work, lwork, &
          iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldz, liwork, lwork, n
          Character (1), Intent (In) :: job, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), w(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsbtrd(vect, uplo, n, kd, ab, ldab, d, e, q, ldq, work, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldq, n
          Character (1), Intent (In) :: uplo, vect
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), q(ldq, *)
          Real (Kind=dp), Intent (Out) :: d(n), e(n-1), work(n)
        End Subroutine
        Subroutine zhbev(jobz, uplo, n, kd, ab, ldab, w, z, ldz, work, rwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Real (Kind=dp), Intent (Out) :: rwork(3*n-2), w(n)
        End Subroutine
        Subroutine zhbevx(jobz, range, uplo, n, kd, ab, ldab, q, ldq, vl, vu, &
          il, iu, abstol, m, w, z, ldz, work, rwork, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, kd, ldab, ldq, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), q(ldq, *), &
            z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Real (Kind=dp), Intent (Out) :: rwork(7*n), w(n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine zhbevd(job, uplo, n, kd, ab, ldab, w, z, ldz, work, lwork, &
          rwork, lrwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldz, liwork, lrwork, lwork, n
          Character (1), Intent (In) :: job, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork))
          Real (Kind=dp), Intent (Inout) :: w(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhbtrd(vect, uplo, n, kd, ab, ldab, d, e, q, ldq, work, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kd, ldab, ldq, n
          Character (1), Intent (In) :: uplo, vect
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), q(ldq, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Real (Kind=dp), Intent (Out) :: d(n), e(n-1)
        End Subroutine
        Subroutine dstev(jobz, n, d, e, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, n
          Character (1), Intent (In) :: jobz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), work(*), z(ldz, *)
        End Subroutine
        Subroutine dstevx(jobz, range, n, d, e, vl, vu, il, iu, abstol, m, w, &
          z, ldz, work, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(5*n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine dstevd(job, n, d, e, z, ldz, work, lwork, iwork, liwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, liwork, lwork, n
          Character (1), Intent (In) :: job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dstevr(jobz, range, n, d, e, vl, vu, il, iu, abstol, m, w, &
          z, ldz, isuppz, work, lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ldz, liwork, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), w(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: isuppz(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsteqr(compz, n, d, e, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, n
          Character (1), Intent (In) :: compz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), work(*), z(ldz, *)
        End Subroutine
        Subroutine dsterf(n, d, e, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*)
        End Subroutine
        Subroutine dpteqr(compz, n, d, e, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, n
          Character (1), Intent (In) :: compz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(4*n)
        End Subroutine
        Subroutine dstedc(compz, n, d, e, z, ldz, work, lwork, iwork, liwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, liwork, lwork, n
          Character (1), Intent (In) :: compz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dstebz(range, order, n, vl, vu, il, iu, abstol, d, e, m, &
          nsplit, w, iblock, isplit, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, n
          Integer, Intent (Out) :: info, m, nsplit
          Character (1), Intent (In) :: order, range
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: d(*), e(*)
          Real (Kind=dp), Intent (Out) :: w(n), work(4*n)
          Integer, Intent (Out) :: iblock(n), isplit(n), iwork(3*n)
        End Subroutine
        Subroutine dstein(n, d, e, m, w, iblock, isplit, z, ldz, work, iwork, &
          ifailv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: d(*), e(*), w(*)
          Real (Kind=dp), Intent (Out) :: work(5*n)
          Real (Kind=dp), Intent (Inout) :: z(ldz, *)
          Integer, Intent (In) :: iblock(*), isplit(*)
          Integer, Intent (Out) :: ifailv(m), iwork(n)
        End Subroutine
        Subroutine dstegr(jobz, range, n, d, e, vl, vu, il, iu, abstol, m, w, &
          z, ldz, isuppz, work, lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ldz, liwork, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), w(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: isuppz(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zsteqr(compz, n, d, e, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, n
          Character (1), Intent (In) :: compz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: z(ldz, *)
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), work(*)
        End Subroutine
        Subroutine zpteqr(compz, n, d, e, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, n
          Character (1), Intent (In) :: compz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: z(ldz, *)
          Real (Kind=dp), Intent (Inout) :: d(*), e(*)
          Real (Kind=dp), Intent (Out) :: work(4*n)
        End Subroutine
        Subroutine zstedc(compz, n, d, e, z, ldz, work, lwork, rwork, lrwork, &
          iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, liwork, lrwork, lwork, n
          Character (1), Intent (In) :: compz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Complex (Kind=dp), Intent (Inout) :: z(ldz, *)
          Real (Kind=dp), Intent (Inout) :: d(*), e(*)
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zstein(n, d, e, m, w, iblock, isplit, z, ldz, work, iwork, &
          ifailv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldz, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: z(ldz, *)
          Real (Kind=dp), Intent (In) :: d(*), e(*), w(*)
          Real (Kind=dp), Intent (Out) :: work(5*n)
          Integer, Intent (In) :: iblock(*), isplit(*)
          Integer, Intent (Out) :: ifailv(m), iwork(n)
        End Subroutine
        Subroutine zstegr(jobz, range, n, d, e, vl, vu, il, iu, abstol, m, w, &
          z, ldz, isuppz, work, lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ldz, liwork, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: z(ldz, *)
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), w(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: isuppz(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgelss(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: rcond
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), s(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldu, ldvt, lwork, m, n
          Character (1), Intent (In) :: jobu, jobvt
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), s(*), u(ldu, *), &
            vt(ldvt, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgelsd(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, &
          lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: rcond
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), s(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: iwork(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgesdd(jobz, m, n, a, lda, s, u, ldu, vt, ldvt, work, &
          lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldu, ldvt, lwork, m, n
          Character (1), Intent (In) :: jobz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), u(ldu, *), vt(ldvt, *)
          Real (Kind=dp), Intent (Out) :: s(min(m,n)), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(8*min(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine dgebrd(m, n, a, lda, d, e, tauq, taup, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), d(*), e(*), taup(*), &
            tauq(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dorgbr(vect, m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
          Character (1), Intent (In) :: vect
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormbr(vect, side, trans, m, n, k, a, lda, tau, c, ldc, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans, vect
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgejsv(joba, jobu, jobv, jobr, jobt, jobp, m, n, a, lda, &
          sva, u, ldu, v, ldv, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldu, ldv, lwork, m, n
          Character (1), Intent (In) :: joba, jobp, jobr, jobt, jobu, jobv
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), u(ldu, *), v(ldv, *)
          Real (Kind=dp), Intent (Out) :: sva(n), work(lwork)
          Integer, Intent (Out) :: iwork(m+3*n)
        End Subroutine
        Subroutine dgesvj(joba, jobu, jobv, m, n, a, lda, sva, mv, v, ldv, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldv, lwork, m, mv, n
          Character (1), Intent (In) :: joba, jobu, jobv
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), v(ldv, *), work(lwork)
          Real (Kind=dp), Intent (Out) :: sva(n)
        End Subroutine
        Subroutine dgesvdx(jobu, jobvt, range, m, n, a, lda, vl, vu, il, iu, &
          ns, s, u, ldu, vt, ldvt, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: vl, vu
          Integer, Intent (In) :: il, iu, lda, ldu, ldvt, lwork, m, n
          Integer, Intent (Out) :: info, ns
          Character (1), Intent (In) :: jobu, jobvt, range
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), u(ldu, *), vt(ldvt, *)
          Real (Kind=dp), Intent (Out) :: s(min(m,n)), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(12*min(n,m))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine zgelss(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, &
          lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: rcond
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*), s(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgesvd(jobu, jobvt, m, n, a, lda, s, u, ldu, vt, ldvt, &
          work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldu, ldvt, lwork, m, n
          Character (1), Intent (In) :: jobu, jobvt
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), u(ldu, *), &
            vt(ldvt, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*), s(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgelsd(m, n, nrhs, a, lda, b, ldb, s, rcond, rank, work, &
          lwork, rwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: rcond
          Integer, Intent (Out) :: info, rank
          Integer, Intent (In) :: lda, ldb, lwork, m, n, nrhs
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*), s(*)
          Integer, Intent (Inout) :: iwork(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgesdd(jobz, m, n, a, lda, s, u, ldu, vt, ldvt, work, &
          lwork, rwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldu, ldvt, lwork, m, n
          Character (1), Intent (In) :: jobz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), u(ldu, *), &
            vt(ldvt, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
          Real (Kind=dp), Intent (Out) :: s(min(m,n))
          Integer, Intent (Out) :: iwork(8*min(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine zgebrd(m, n, a, lda, d, e, tauq, taup, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, lwork, m, n
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), taup(*), tauq(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: d(*), e(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zungbr(vect, m, n, k, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, lwork, m, n
          Character (1), Intent (In) :: vect
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmbr(vect, side, trans, m, n, k, a, lda, tau, c, ldc, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: k, lda, ldc, lwork, m, n
          Character (1), Intent (In) :: side, trans, vect
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgejsv(joba, jobu, jobv, jobr, jobt, jobp, m, n, a, lda, &
          sva, u, ldu, v, ldv, cwork, lwork, rwork, lrwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldu, ldv, lrwork, lwork, m, n
          Character (1), Intent (In) :: joba, jobp, jobr, jobt, jobu, jobv
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), u(ldu, *), v(ldv, *)
          Complex (Kind=dp), Intent (Out) :: cwork(lwork)
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork)), sva(n)
          Integer, Intent (Out) :: iwork(m+3*n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgesvj(joba, jobu, jobv, m, n, a, lda, sva, mv, v, ldv, &
          cwork, lwork, rwork, lrwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldv, lrwork, lwork, m, mv, n
          Character (1), Intent (In) :: joba, jobu, jobv
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), v(ldv, *)
          Complex (Kind=dp), Intent (Out) :: cwork(lwork)
          Real (Kind=dp), Intent (Inout) :: rwork(lrwork)
          Real (Kind=dp), Intent (Out) :: sva(n)
        End Subroutine
        Subroutine zgesvdx(jobu, jobvt, range, m, n, a, lda, vl, vu, il, iu, &
          ns, s, u, ldu, vt, ldvt, work, lwork, rwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: vl, vu
          Integer, Intent (In) :: il, iu, lda, ldu, ldvt, lwork, m, n
          Integer, Intent (Out) :: info, ns
          Character (1), Intent (In) :: jobu, jobvt, range
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), u(ldu, *), &
            vt(ldvt, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
          Real (Kind=dp), Intent (Out) :: s(min(m,n))
          Integer, Intent (Out) :: iwork(12*min(n,m))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine dgbbrd(vect, m, n, ncc, kl, ku, ab, ldab, d, e, q, ldq, pt, &
          ldpt, c, ldc, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
          Character (1), Intent (In) :: vect
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), c(ldc, *), &
            pt(ldpt, *), q(ldq, *)
          Real (Kind=dp), Intent (Out) :: d(min(m,n)), e(min(m,n)-1), &
            work(2*max(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine zgbbrd(vect, m, n, ncc, kl, ku, ab, ldab, d, e, q, ldq, pt, &
          ldpt, c, ldc, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kl, ku, ldab, ldc, ldpt, ldq, m, n, ncc
          Character (1), Intent (In) :: vect
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), c(ldc, *), &
            pt(ldpt, *), q(ldq, *)
          Complex (Kind=dp), Intent (Out) :: work(max(m,n))
          Real (Kind=dp), Intent (Out) :: d(min(m,n)), e(min(m,n)-1), &
            rwork(max(m,n))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine dbdsvdx(uplo, jobz, range, n, d, e, vl, vu, il, iu, ns, s, &
          z, ldz, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: vl, vu
          Integer, Intent (In) :: il, iu, ldz, n
          Integer, Intent (Out) :: info, ns
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: d(n), e(n-1)
          Real (Kind=dp), Intent (Out) :: s(n), work(14*n)
          Real (Kind=dp), Intent (Inout) :: z(ldz, *)
          Integer, Intent (Out) :: iwork(12*n)
        End Subroutine
        Subroutine dbdsdc(uplo, compq, n, d, e, u, ldu, vt, ldvt, q, iq, work, &
          iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldu, ldvt, n
          Character (1), Intent (In) :: compq, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), q(*), u(ldu, *), &
            vt(ldvt, *), work(*)
          Integer, Intent (Inout) :: iq(*)
          Integer, Intent (Out) :: iwork(8*n)
        End Subroutine
        Subroutine dbdsqr(uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, &
          ldc, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: c(ldc, *), d(*), e(*), u(ldu, *), &
            vt(ldvt, *), work(*)
        End Subroutine
        Subroutine zbdsqr(uplo, n, ncvt, nru, ncc, d, e, vt, ldvt, u, ldu, c, &
          ldc, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldc, ldu, ldvt, n, ncc, ncvt, nru
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *), u(ldu, *), &
            vt(ldvt, *)
          Real (Kind=dp), Intent (Inout) :: d(*), e(*), work(*)
        End Subroutine
        Subroutine dgeev(jobvl, jobvr, n, a, lda, wr, wi, vl, ldvl, vr, ldvr, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: jobvl, jobvr
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), vl(ldvl, *), &
            vr(ldvr, *), wi(*), wr(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgeevx(balanc, jobvl, jobvr, sense, n, a, lda, wr, wi, vl, &
          ldvl, vr, ldvr, ilo, ihi, scale, abnrm, rconde, rcondv, work, lwork, &
          iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: abnrm
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: balanc, jobvl, jobvr, sense
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), rconde(*), rcondv(*), &
            scale(*), vl(ldvl, *), vr(ldvr, *), wi(*), wr(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: iwork(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgehrd(n, ilo, ihi, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, lwork, n
          Integer, Intent (Out) :: info
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dorghr(n, ilo, ihi, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, lwork, n
          Integer, Intent (Out) :: info
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dormhr(side, trans, m, n, ilo, ihi, a, lda, tau, c, ldc, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldc, lwork, m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Real (Kind=dp), Intent (In) :: tau(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgebal(job, n, a, lda, ilo, ihi, scale, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: scale(n)
        End Subroutine
        Subroutine dgebak(job, side, n, ilo, ihi, scale, m, v, ldv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, ldv, m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: job, side
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: scale(*)
          Real (Kind=dp), Intent (Inout) :: v(ldv, *)
        End Subroutine
        Subroutine zgeev(jobvl, jobvr, n, a, lda, w, vl, ldvl, vr, ldvr, work, &
          lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: jobvl, jobvr
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), vl(ldvl, *), &
            vr(ldvr, *), w(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rwork(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgeevx(balanc, jobvl, jobvr, sense, n, a, lda, w, vl, ldvl, &
          vr, ldvr, ilo, ihi, scale, abnrm, rconde, rcondv, work, lwork, &
          rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: abnrm
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: balanc, jobvl, jobvr, sense
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), vl(ldvl, *), &
            vr(ldvr, *), w(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: rconde(*), rcondv(*), rwork(*), &
            scale(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgehrd(n, ilo, ihi, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, lwork, n
          Integer, Intent (Out) :: info
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunghr(n, ilo, ihi, a, lda, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, lwork, n
          Integer, Intent (Out) :: info
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zunmhr(side, trans, m, n, ilo, ihi, a, lda, tau, c, ldc, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldc, lwork, m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: side, trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), c(ldc, *)
          Complex (Kind=dp), Intent (In) :: tau(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgebal(job, n, a, lda, ilo, ihi, scale, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, n
          Character (1), Intent (In) :: job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (Out) :: scale(n)
        End Subroutine
        Subroutine zgebak(job, side, n, ilo, ihi, scale, m, v, ldv, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, ldv, m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: job, side
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: v(ldv, *)
          Real (Kind=dp), Intent (In) :: scale(*)
        End Subroutine
        Subroutine dhseqr(job, compz, n, ilo, ihi, h, ldh, wr, wi, z, ldz, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, ldh, ldz, lwork, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compz, job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: h(ldh, *), wi(*), wr(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dhsein(job, eigsrc, initv, select, n, h, ldh, wr, wi, vl, &
          ldvl, vr, ldvr, mm, m, work, ifaill, ifailr, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldh, ldvl, ldvr, mm, n
          Character (1), Intent (In) :: eigsrc, initv, job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: h(ldh, *), wi(*)
          Real (Kind=dp), Intent (Inout) :: vl(ldvl, *), vr(ldvr, *), wr(*)
          Real (Kind=dp), Intent (Out) :: work((n+2)*n)
          Integer, Intent (Inout) :: ifaill(*), ifailr(*)
          Logical, Intent (Inout) :: select(*)
        End Subroutine
        Subroutine zhseqr(job, compz, n, ilo, ihi, h, ldh, w, z, ldz, work, &
          lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, ldh, ldz, lwork, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compz, job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: h(ldh, *), w(*), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhsein(job, eigsrc, initv, select, n, h, ldh, w, vl, ldvl, &
          vr, ldvr, mm, m, work, rwork, ifaill, ifailr, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldh, ldvl, ldvr, mm, n
          Character (1), Intent (In) :: eigsrc, initv, job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: h(ldh, *)
          Complex (Kind=dp), Intent (Inout) :: vl(ldvl, *), vr(ldvr, *), w(*)
          Complex (Kind=dp), Intent (Out) :: work(n*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
          Integer, Intent (Inout) :: ifaill(*), ifailr(*)
          Logical, Intent (In) :: select(*)
        End Subroutine
        Subroutine dtrexc(compq, n, t, ldt, q, ldq, ifst, ilst, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Inout) :: ifst, ilst
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldq, ldt, n
          Character (1), Intent (In) :: compq
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: q(ldq, *), t(ldt, *)
          Real (Kind=dp), Intent (Out) :: work(n)
        End Subroutine
        Subroutine dtrsen(job, compq, select, n, t, ldt, q, ldq, wr, wi, m, s, &
          sep, work, lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: s, sep
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldq, ldt, liwork, lwork, n
          Character (1), Intent (In) :: compq, job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: q(ldq, *), t(ldt, *), wi(*), wr(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
          Logical, Intent (In) :: select(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dtrsyl(trana, tranb, isgn, m, n, a, lda, b, ldb, c, ldc, &
          scale, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: scale
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: isgn, lda, ldb, ldc, m, n
          Character (1), Intent (In) :: trana, tranb
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine dtrevc(job, howmny, select, n, t, ldt, vl, ldvl, vr, ldvr, &
          mm, m, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldt, ldvl, ldvr, mm, n
          Character (1), Intent (In) :: howmny, job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: t(ldt, *)
          Real (Kind=dp), Intent (Inout) :: vl(ldvl, *), vr(ldvr, *)
          Real (Kind=dp), Intent (Out) :: work(3*n)
          Logical, Intent (Inout) :: select(*)
        End Subroutine
        Subroutine dtrsna(job, howmny, select, n, t, ldt, vl, ldvl, vr, ldvr, &
          s, sep, mm, m, work, ldwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldt, ldvl, ldvr, ldwork, mm, n
          Character (1), Intent (In) :: howmny, job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: s(*), sep(*), work(ldwork, *)
          Real (Kind=dp), Intent (In) :: t(ldt, *), vl(ldvl, *), vr(ldvr, *)
          Integer, Intent (Inout) :: iwork(*)
          Logical, Intent (In) :: select(*)
        End Subroutine
        Subroutine ztrexc(compq, n, t, ldt, q, ldq, ifst, ilst, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ifst, ilst, ldq, ldt, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compq
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: q(ldq, *), t(ldt, *)
        End Subroutine
        Subroutine ztrsen(job, compq, select, n, t, ldt, q, ldq, w, m, s, sep, &
          work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: s, sep
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldq, ldt, lwork, n
          Character (1), Intent (In) :: compq, job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: q(ldq, *), t(ldt, *), w(*)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Logical, Intent (In) :: select(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine ztrsyl(trana, tranb, isgn, m, n, a, lda, b, ldb, c, ldc, &
          scal, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: scal
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: isgn, lda, ldb, ldc, m, n
          Character (1), Intent (In) :: trana, tranb
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *)
        End Subroutine
        Subroutine ztrevc(job, howmny, select, n, t, ldt, vl, ldvl, vr, ldvr, &
          mm, m, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldt, ldvl, ldvr, mm, n
          Character (1), Intent (In) :: howmny, job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: t(ldt, *), vl(ldvl, *), &
            vr(ldvr, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
          Logical, Intent (In) :: select(*)
        End Subroutine
        Subroutine ztrsna(job, howmny, select, n, t, ldt, vl, ldvl, vr, ldvr, &
          s, sep, mm, m, work, ldwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: ldt, ldvl, ldvr, ldwork, mm, n
          Character (1), Intent (In) :: howmny, job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: t(ldt, *), vl(ldvl, *), &
            vr(ldvr, *)
          Complex (Kind=dp), Intent (Inout) :: work(ldwork, *)
          Real (Kind=dp), Intent (Inout) :: rwork(*), s(*), sep(*)
          Logical, Intent (In) :: select(*)
        End Subroutine
        Subroutine dorcsd(jobu1, jobu2, jobv1t, jobv2t, trans, signs, m, p, q, &
          x11, ldx11, x12, ldx12, x21, ldx21, x22, ldx22, theta, u1, ldu1, u2, &
          ldu2, v1t, ldv1t, v2t, ldv2t, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, &
            ldx21, ldx22, lwork, m, p, q
          Character (1), Intent (In) :: jobu1, jobu2, jobv1t, jobv2t, signs, &
            trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Out) :: theta(min(p,m-p,q,m-q)), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: u1(ldu1, *), u2(ldu2, *), &
            v1t(ldv1t, *), v2t(ldv2t, *), x11(ldx11, *), x12(ldx12, *), &
            x21(ldx21, *), x22(ldx22, *)
          Integer, Intent (Out) :: iwork(m-min(p,m-p,q,m-q))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine zuncsd(jobu1, jobu2, jobv1t, jobv2t, trans, signs, m, p, q, &
          x11, ldx11, x12, ldx12, x21, ldx21, x22, ldx22, theta, u1, ldu1, u2, &
          ldu2, v1t, ldv1t, v2t, ldv2t, work, lwork, rwork, lrwork, iwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ldu1, ldu2, ldv1t, ldv2t, ldx11, ldx12, &
            ldx21, ldx22, lrwork, lwork, m, p, q
          Character (1), Intent (In) :: jobu1, jobu2, jobv1t, jobv2t, signs, &
            trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: u1(ldu1, *), u2(ldu2, *), &
            v1t(ldv1t, *), v2t(ldv2t, *), x11(ldx11, *), x12(ldx12, *), &
            x21(ldx21, *), x22(ldx22, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork)), &
            theta(min(p,m-p,q,m-q))
          Integer, Intent (Out) :: iwork(m-min(p,m-p,q,m-q))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine dsygv(itype, jobz, uplo, n, a, lda, b, ldb, w, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, lda, ldb, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsygvx(itype, jobz, range, uplo, n, a, lda, b, ldb, vl, vu, &
          il, iu, abstol, m, w, z, ldz, work, lwork, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, itype, iu, lda, ldb, ldz, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsygvd(itype, jobz, uplo, n, a, lda, b, ldb, w, work, &
          lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, lda, ldb, liwork, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsygst(itype, uplo, n, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, lda, ldb, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *)
          Real (Kind=dp), Intent (In) :: b(ldb, *)
        End Subroutine
        Subroutine zhegv(itype, jobz, uplo, n, a, lda, b, ldb, w, work, lwork, &
          rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, lda, ldb, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(3*n-2), w(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhegvx(itype, jobz, range, uplo, n, a, lda, b, ldb, vl, vu, &
          il, iu, abstol, m, w, z, ldz, work, lwork, rwork, iwork, jfail, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, itype, iu, lda, ldb, ldz, lwork, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(7*n), w(n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhegvd(itype, jobz, uplo, n, a, lda, b, ldb, w, work, &
          lwork, rwork, lrwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, lda, ldb, liwork, lrwork, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork)), w(n)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhegst(itype, uplo, n, a, lda, b, ldb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, lda, ldb, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
        End Subroutine
        Subroutine dspgv(itype, jobz, uplo, n, ap, bp, w, z, ldz, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), bp(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(3*n)
        End Subroutine
        Subroutine dspgvx(itype, jobz, range, uplo, n, ap, bp, vl, vu, il, iu, &
          abstol, m, w, z, ldz, work, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, itype, iu, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), bp(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(8*n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine dspgvd(itype, jobz, uplo, n, ap, bp, w, z, ldz, work, &
          lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, ldz, liwork, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*), bp(*), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dspgst(itype, uplo, n, ap, bp, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ap(*)
          Real (Kind=dp), Intent (In) :: bp(*)
        End Subroutine
        Subroutine zhpgv(itype, jobz, uplo, n, ap, bp, w, z, ldz, work, rwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), bp(*), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n-1)
          Real (Kind=dp), Intent (Out) :: rwork(3*n-2), w(n)
        End Subroutine
        Subroutine zhpgvx(itype, jobz, range, uplo, n, ap, bp, vl, vu, il, iu, &
          abstol, m, w, z, ldz, work, rwork, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, itype, iu, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), bp(*), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(7*n), w(n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine zhpgvd(itype, jobz, uplo, n, ap, bp, w, z, ldz, work, &
          lwork, rwork, lrwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, ldz, liwork, lrwork, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*), bp(*), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork)), w(n)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhpgst(itype, uplo, n, ap, bp, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: itype, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ap(*)
          Complex (Kind=dp), Intent (In) :: bp(*)
        End Subroutine
        Subroutine dsbgv(jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z, ldz, &
          work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ka, kb, ldab, ldbb, ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), bb(ldbb, *), &
            z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(3*n)
        End Subroutine
        Subroutine dsbgvx(jobz, range, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, &
          ldq, vl, vu, il, iu, abstol, m, w, z, ldz, work, iwork, jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), bb(ldbb, *), &
            q(ldq, *), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(7*n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine dsbgvd(jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z, &
          ldz, work, lwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ka, kb, ldab, ldbb, ldz, liwork, lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), bb(ldbb, *), &
            z(ldz, *)
          Real (Kind=dp), Intent (Out) :: w(n), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dsbgst(vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x, ldx, &
          work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ka, kb, ldab, ldbb, ldx, n
          Character (1), Intent (In) :: uplo, vect
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: ab(ldab, *), x(ldx, *)
          Real (Kind=dp), Intent (In) :: bb(ldbb, *)
          Real (Kind=dp), Intent (Out) :: work(2*n)
        End Subroutine
        Subroutine dpbstf(uplo, n, kb, bb, ldbb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kb, ldbb, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: bb(ldbb, *)
        End Subroutine
        Subroutine zhbgv(jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z, ldz, &
          work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ka, kb, ldab, ldbb, ldz, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), bb(ldbb, *), &
            z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Real (Kind=dp), Intent (Out) :: rwork(3*n), w(n)
        End Subroutine
        Subroutine zhbgvx(jobz, range, uplo, n, ka, kb, ab, ldab, bb, ldbb, q, &
          ldq, vl, vu, il, iu, abstol, m, w, z, ldz, work, rwork, iwork, &
          jfail, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: abstol, vl, vu
          Integer, Intent (In) :: il, iu, ka, kb, ldab, ldbb, ldq, ldz, n
          Integer, Intent (Out) :: info, m
          Character (1), Intent (In) :: jobz, range, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), bb(ldbb, *), &
            q(ldq, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Real (Kind=dp), Intent (Out) :: rwork(7*n), w(n)
          Integer, Intent (Out) :: iwork(5*n)
          Integer, Intent (Inout) :: jfail(*)
        End Subroutine
        Subroutine zhbgvd(jobz, uplo, n, ka, kb, ab, ldab, bb, ldbb, w, z, &
          ldz, work, lwork, rwork, lrwork, iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ka, kb, ldab, ldbb, ldz, liwork, lrwork, &
            lwork, n
          Character (1), Intent (In) :: jobz, uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), bb(ldbb, *), &
            z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,lrwork)), w(n)
          Integer, Intent (Out) :: iwork(max(1,liwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhbgst(vect, uplo, n, ka, kb, ab, ldab, bb, ldbb, x, ldx, &
          work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: ka, kb, ldab, ldbb, ldx, n
          Character (1), Intent (In) :: uplo, vect
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: ab(ldab, *), x(ldx, *)
          Complex (Kind=dp), Intent (In) :: bb(ldbb, *)
          Complex (Kind=dp), Intent (Out) :: work(n)
          Real (Kind=dp), Intent (Out) :: rwork(n)
        End Subroutine
        Subroutine zpbstf(uplo, n, kb, bb, ldbb, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: kb, ldbb, n
          Character (1), Intent (In) :: uplo
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: bb(ldbb, *)
        End Subroutine
        Subroutine dggsvd(jobu, jobv, jobq, m, n, p, k, l, a, lda, b, ldb, &
          alpha, beta, u, ldu, v, ldv, q, ldq, work, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            u(ldu, *), v(ldv, *)
          Real (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(3*n,m,p)+n)
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggsvd3(jobu, jobv, jobq, m, n, p, k, l, a, lda, b, ldb, &
          alpha, beta, u, ldu, v, ldv, q, ldq, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, lwork, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            u(ldu, *), v(ldv, *)
          Real (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggsvp(jobu, jobv, jobq, m, p, n, a, lda, b, ldb, tola, &
          tolb, k, l, u, ldu, v, ldv, q, ldq, iwork, tau, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tola, tolb
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            u(ldu, *), v(ldv, *)
          Real (Kind=dp), Intent (Out) :: tau(n), work(max(3*n,m,p))
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggsvp3(jobu, jobv, jobq, m, p, n, a, lda, b, ldb, tola, &
          tolb, k, l, u, ldu, v, ldv, q, ldq, iwork, tau, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tola, tolb
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, lwork, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            u(ldu, *), v(ldv, *)
          Real (Kind=dp), Intent (Out) :: tau(n), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggsvd(jobu, jobv, jobq, m, n, p, k, l, a, lda, b, ldb, &
          alpha, beta, u, ldu, v, ldv, q, ldq, work, rwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), u(ldu, *), v(ldv, *)
          Complex (Kind=dp), Intent (Out) :: work(max(3*n,m,p)+n)
          Real (Kind=dp), Intent (Out) :: alpha(n), beta(n), rwork(2*n)
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggsvd3(jobu, jobv, jobq, m, n, p, k, l, a, lda, b, ldb, &
          alpha, beta, u, ldu, v, ldv, q, ldq, work, lwork, rwork, iwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, lwork, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), u(ldu, *), v(ldv, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: alpha(n), beta(n), rwork(2*n)
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggsvp(jobu, jobv, jobq, m, p, n, a, lda, b, ldb, tola, &
          tolb, k, l, u, ldu, v, ldv, q, ldq, iwork, rwork, tau, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tola, tolb
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), u(ldu, *), v(ldv, *)
          Complex (Kind=dp), Intent (Out) :: tau(n), work(max(3*n,m,p))
          Real (Kind=dp), Intent (Out) :: rwork(2*n)
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggsvp3(jobu, jobv, jobq, m, p, n, a, lda, b, ldb, tola, &
          tolb, k, l, u, ldu, v, ldv, q, ldq, iwork, rwork, tau, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tola, tolb
          Integer, Intent (Out) :: info, k, l
          Integer, Intent (In) :: lda, ldb, ldq, ldu, ldv, lwork, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), u(ldu, *), v(ldv, *)
          Complex (Kind=dp), Intent (Out) :: tau(n), work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(2*n)
          Integer, Intent (Out) :: iwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgges(jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb, &
          sdim, alphar, alphai, beta, vsl, ldvsl, vsr, ldvsr, work, lwork, &
          bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldb, ldvsl, ldvsr, lwork, n
          Character (1), Intent (In) :: jobvsl, jobvsr, sort
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vsl(ldvsl, *), vsr(ldvsr, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            work(max(1,lwork))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function selctg(ar, ai, b)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: selctg
!             .. Scalar Arguments ..
              Real (Kind=dp), Intent (In) :: ai, ar, b
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgges3(jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb, &
          sdim, alphar, alphai, beta, vsl, ldvsl, vsr, ldvsr, work, lwork, &
          bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldb, ldvsl, ldvsr, lwork, n
          Character (1), Intent (In) :: jobvsl, jobvsr, sort
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vsl(ldvsl, *), vsr(ldvsr, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            work(max(1,lwork))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function selctg(ar, ai, b)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: selctg
!             .. Scalar Arguments ..
              Real (Kind=dp), Intent (In) :: ai, ar, b
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggesx(jobvsl, jobvsr, sort, selctg, sense, n, a, lda, b, &
          ldb, sdim, alphar, alphai, beta, vsl, ldvsl, vsr, ldvsr, rconde, &
          rcondv, work, lwork, iwork, liwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
          Character (1), Intent (In) :: jobvsl, jobvsr, sense, sort
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vsl(ldvsl, *), vsr(ldvsr, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            rconde(2), rcondv(2), work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,liwork))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function selctg(ar, ai, b)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: selctg
!             .. Scalar Arguments ..
              Real (Kind=dp), Intent (In) :: ai, ar, b
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggev(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, &
          beta, vl, ldvl, vr, ldvr, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: jobvl, jobvr
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), vl(ldvl, *), &
            vr(ldvr, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggevx(balanc, jobvl, jobvr, sense, n, a, lda, b, ldb, &
          alphar, alphai, beta, vl, ldvl, vr, ldvr, ilo, ihi, lscale, rscale, &
          abnrm, bbnrm, rconde, rcondv, work, lwork, iwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: abnrm, bbnrm
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: balanc, jobvl, jobvr, sense
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), rconde(*), &
            rcondv(*), vl(ldvl, *), vr(ldvr, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            lscale(n), rscale(n), work(max(1,lwork))
          Integer, Intent (Inout) :: iwork(*)
          Logical, Intent (Inout) :: bwork(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggev3(jobvl, jobvr, n, a, lda, b, ldb, alphar, alphai, &
          beta, vl, ldvl, vr, ldvr, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: jobvl, jobvr
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), vl(ldvl, *), &
            vr(ldvr, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgghrd(compq, compz, n, ilo, ihi, a, lda, b, ldb, q, ldq, &
          z, ldz, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldb, ldq, ldz, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compq, compz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            z(ldz, *)
        End Subroutine
        Subroutine dgghd3(compq, compz, n, ilo, ihi, a, lda, b, ldb, q, ldq, &
          z, ldz, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldb, ldq, ldz, lwork, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compq, compz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggbal(job, n, a, lda, b, ldb, ilo, ihi, lscale, rscale, &
          work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, ldb, n
          Character (1), Intent (In) :: job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), work(*)
          Real (Kind=dp), Intent (Out) :: lscale(n), rscale(n)
        End Subroutine
        Subroutine dggbak(job, side, n, ilo, ihi, lscale, rscale, m, v, ldv, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, ldv, m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: job, side
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: lscale(*), rscale(*)
          Real (Kind=dp), Intent (Inout) :: v(ldv, *)
        End Subroutine
        Subroutine zgges(jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb, &
          sdim, alpha, beta, vsl, ldvsl, vsr, ldvsr, work, lwork, rwork, &
          bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldb, ldvsl, ldvsr, lwork, n
          Character (1), Intent (In) :: jobvsl, jobvsr, sort
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vsl(ldvsl, *), vsr(ldvsr, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,8*n))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function selctg(a, b)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: selctg
!             .. Scalar Arguments ..
              Complex (Kind=dp), Intent (In) :: a, b
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgges3(jobvsl, jobvsr, sort, selctg, n, a, lda, b, ldb, &
          sdim, alpha, beta, vsl, ldvsl, vsr, ldvsr, work, lwork, rwork, &
          bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldb, ldvsl, ldvsr, lwork, n
          Character (1), Intent (In) :: jobvsl, jobvsr, sort
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vsl(ldvsl, *), vsr(ldvsr, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,8*n))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function selctg(a, b)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: selctg
!             .. Scalar Arguments ..
              Complex (Kind=dp), Intent (In) :: a, b
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggesx(jobvsl, jobvsr, sort, selctg, sense, n, a, lda, b, &
          ldb, sdim, alpha, beta, vsl, ldvsl, vsr, ldvsr, rconde, rcondv, &
          work, lwork, rwork, iwork, liwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, sdim
          Integer, Intent (In) :: lda, ldb, ldvsl, ldvsr, liwork, lwork, n
          Character (1), Intent (In) :: jobvsl, jobvsr, sense, sort
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vsl(ldvsl, *), vsr(ldvsr, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rconde(2), rcondv(2), &
            rwork(max(1,8*n))
          Integer, Intent (Out) :: iwork(max(1,liwork))
          Logical, Intent (Inout) :: bwork(*)
!         .. Interface Blocks ..
          Interface
            Function selctg(a, b)
!             .. Use Statements ..
              Use lapack_precision, Only: dp
!             .. Implicit None Statement ..
              Implicit None
!             .. Function Return Value ..
              Logical :: selctg
!             .. Scalar Arguments ..
              Complex (Kind=dp), Intent (In) :: a, b
            End Function
          End Interface
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggev(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta, vl, &
          ldvl, vr, ldvr, work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: jobvl, jobvr
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vl(ldvl, *), vr(ldvr, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,8*n))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggevx(balanc, jobvl, jobvr, sense, n, a, lda, b, ldb, &
          alpha, beta, vl, ldvl, vr, ldvr, ilo, ihi, lscale, rscale, abnrm, &
          bbnrm, rconde, rcondv, work, lwork, rwork, iwork, bwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: abnrm, bbnrm
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: balanc, jobvl, jobvr, sense
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vl(ldvl, *), vr(ldvr, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: lscale(n), rscale(n), rwork(6*n)
          Real (Kind=dp), Intent (Inout) :: rconde(*), rcondv(*)
          Integer, Intent (Inout) :: iwork(*)
          Logical, Intent (Inout) :: bwork(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggev3(jobvl, jobvr, n, a, lda, b, ldb, alpha, beta, vl, &
          ldvl, vr, ldvr, work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, n
          Character (1), Intent (In) :: jobvl, jobvr
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            vl(ldvl, *), vr(ldvr, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(max(1,8*n))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zgghrd(compq, compz, n, ilo, ihi, a, lda, b, ldb, q, ldq, &
          z, ldz, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldb, ldq, ldz, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compq, compz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), z(ldz, *)
        End Subroutine
        Subroutine zgghd3(compq, compz, n, ilo, ihi, a, lda, b, ldb, q, ldq, &
          z, ldz, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldb, ldq, ldz, lwork, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compq, compz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggbal(job, n, a, lda, b, ldb, ilo, ihi, lscale, rscale, &
          work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: ihi, ilo, info
          Integer, Intent (In) :: lda, ldb, n
          Character (1), Intent (In) :: job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: lscale(n), rscale(n), work(6*n)
        End Subroutine
        Subroutine zggbak(job, side, n, ilo, ihi, lscale, rscale, m, v, ldv, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, ldv, m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: job, side
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: v(ldv, *)
          Real (Kind=dp), Intent (In) :: lscale(*), rscale(*)
        End Subroutine
        Subroutine dhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, &
          alphar, alphai, beta, q, ldq, z, ldz, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldb, ldq, ldz, lwork, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compq, compz, job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            z(ldz, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zhgeqz(job, compq, compz, n, ilo, ihi, a, lda, b, ldb, &
          alpha, beta, q, ldq, z, ldz, work, lwork, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ihi, ilo, lda, ldb, ldq, ldz, lwork, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: compq, compz, job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Out) :: rwork(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dtgsja(jobu, jobv, jobq, m, p, n, k, l, a, lda, b, ldb, &
          tola, tolb, alpha, beta, u, ldu, v, ldv, q, ldq, work, ncycle, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tola, tolb
          Integer, Intent (Out) :: info, ncycle
          Integer, Intent (In) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            u(ldu, *), v(ldv, *)
          Real (Kind=dp), Intent (Out) :: alpha(n), beta(n), work(2*n)
        End Subroutine
        Subroutine dtgexc(wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz, &
          ifst, ilst, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Inout) :: ifst, ilst
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, ldq, ldz, lwork, n
          Logical, Intent (In) :: wantq, wantz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), q(ldq, *), &
            z(ldz, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dtgsen(ijob, wantq, wantz, select, n, a, lda, b, ldb, &
          alphar, alphai, beta, q, ldq, z, ldz, m, pl, pr, dif, work, lwork, &
          iwork, liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: pl, pr
          Integer, Intent (In) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
          Integer, Intent (Out) :: info, m
          Logical, Intent (In) :: wantq, wantz
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), dif(*), &
            q(ldq, *), z(ldz, *)
          Real (Kind=dp), Intent (Out) :: alphai(n), alphar(n), beta(n), &
            work(max(1,lwork))
          Integer, Intent (Inout) :: iwork(*)
          Logical, Intent (In) :: select(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dtgsyl(trans, ijob, m, n, a, lda, b, ldb, c, ldc, d, ldd, &
          e, lde, f, ldf, scale, dif, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: dif, scale
          Integer, Intent (In) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, &
            m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *), d(ldd, *), &
            e(lde, *)
          Real (Kind=dp), Intent (Inout) :: c(ldc, *), f(ldf, *)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(max(1,m+n+6))
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dtgevc(side, howmny, select, n, a, lda, b, ldb, vl, ldvl, &
          vr, ldvr, mm, m, work, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, mm, n
          Character (1), Intent (In) :: howmny, side
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Inout) :: vl(ldvl, *), vr(ldvr, *)
          Real (Kind=dp), Intent (Out) :: work(6*n)
          Logical, Intent (In) :: select(*)
        End Subroutine
        Subroutine dtgsna(job, howmny, select, n, a, lda, b, ldb, vl, ldvl, &
          vr, ldvr, s, dif, mm, m, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, mm, n
          Character (1), Intent (In) :: howmny, job
!         .. Array Arguments ..
          Real (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *), vl(ldvl, *), &
            vr(ldvr, *)
          Real (Kind=dp), Intent (Inout) :: dif(*), s(*)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Inout) :: iwork(*)
          Logical, Intent (In) :: select(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine ztgsja(jobu, jobv, jobq, m, p, n, k, l, a, lda, b, ldb, &
          tola, tolb, alpha, beta, u, ldu, v, ldv, q, ldq, work, ncycle, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (In) :: tola, tolb
          Integer, Intent (Out) :: info, ncycle
          Integer, Intent (In) :: k, l, lda, ldb, ldq, ldu, ldv, m, n, p
          Character (1), Intent (In) :: jobq, jobu, jobv
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), u(ldu, *), v(ldv, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: alpha(n), beta(n)
        End Subroutine
        Subroutine ztgexc(wantq, wantz, n, a, lda, b, ldb, q, ldq, z, ldz, &
          ifst, ilst, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (In) :: ifst, lda, ldb, ldq, ldz, n
          Integer, Intent (Inout) :: ilst
          Integer, Intent (Out) :: info
          Logical, Intent (In) :: wantq, wantz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), z(ldz, *)
        End Subroutine
        Subroutine ztgsen(ijob, wantq, wantz, select, n, a, lda, b, ldb, &
          alpha, beta, q, ldq, z, ldz, m, pl, pr, dif, work, lwork, iwork, &
          liwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: pl, pr
          Integer, Intent (In) :: ijob, lda, ldb, ldq, ldz, liwork, lwork, n
          Integer, Intent (Out) :: info, m
          Logical, Intent (In) :: wantq, wantz
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), &
            q(ldq, *), z(ldz, *)
          Complex (Kind=dp), Intent (Out) :: alpha(n), beta(n), &
            work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: dif(*)
          Integer, Intent (Out) :: iwork(max(1,liwork))
          Logical, Intent (In) :: select(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine ztgsyl(trans, ijob, m, n, a, lda, b, ldb, c, ldc, d, ldd, &
          e, lde, f, ldf, scale, dif, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Real (Kind=dp), Intent (Out) :: dif, scale
          Integer, Intent (In) :: ijob, lda, ldb, ldc, ldd, lde, ldf, lwork, &
            m, n
          Integer, Intent (Out) :: info
          Character (1), Intent (In) :: trans
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *), d(ldd, *), &
            e(lde, *)
          Complex (Kind=dp), Intent (Inout) :: c(ldc, *), f(ldf, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Integer, Intent (Out) :: iwork(m+n+2)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine ztgevc(side, howmny, select, n, a, lda, b, ldb, vl, ldvl, &
          vr, ldvr, mm, m, work, rwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, mm, n
          Character (1), Intent (In) :: howmny, side
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Inout) :: vl(ldvl, *), vr(ldvr, *)
          Complex (Kind=dp), Intent (Out) :: work(2*n)
          Real (Kind=dp), Intent (Out) :: rwork(2*n)
          Logical, Intent (In) :: select(*)
        End Subroutine
        Subroutine ztgsna(job, howmny, select, n, a, lda, b, ldb, vl, ldvl, &
          vr, ldvr, s, dif, mm, m, work, lwork, iwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info, m
          Integer, Intent (In) :: lda, ldb, ldvl, ldvr, lwork, mm, n
          Character (1), Intent (In) :: howmny, job
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (In) :: a(lda, *), b(ldb, *), vl(ldvl, *), &
            vr(ldvr, *)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork))
          Real (Kind=dp), Intent (Inout) :: dif(*), s(*)
          Integer, Intent (Inout) :: iwork(*)
          Logical, Intent (In) :: select(*)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dgglse(m, n, p, a, lda, b, ldb, c, d, x, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), c(m), d(p)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork)), x(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggglm(m, n, p, a, lda, b, ldb, d, x, y, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), d(m)
          Real (Kind=dp), Intent (Out) :: work(max(1,lwork)), x(n), y(p)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine dggqrf(n, m, p, a, lda, taua, b, ldb, taub, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: taua(min(n,m)), taub(min(n,p)), &
            work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine dggrqf(m, p, n, a, lda, taua, b, ldb, taub, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Real (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Real (Kind=dp), Intent (Out) :: taua(min(m,n)), taub(min(p,n)), &
            work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine zgglse(m, n, p, a, lda, b, ldb, c, d, x, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), c(m), &
            d(p)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork)), x(n)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggglm(m, n, p, a, lda, b, ldb, d, x, y, work, lwork, info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *), d(m)
          Complex (Kind=dp), Intent (Out) :: work(max(1,lwork)), x(n), y(p)
!         .. Intrinsic Procedures ..
          Intrinsic :: max
        End Subroutine
        Subroutine zggqrf(n, m, p, a, lda, taua, b, ldb, taub, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: taua(min(n,m)), taub(min(n,p)), &
            work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Subroutine zggrqf(m, p, n, a, lda, taua, b, ldb, taub, work, lwork, &
          info)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
!         .. Implicit None Statement ..
          Implicit None
!         .. Scalar Arguments ..
          Integer, Intent (Out) :: info
          Integer, Intent (In) :: lda, ldb, lwork, m, n, p
!         .. Array Arguments ..
          Complex (Kind=dp), Intent (Inout) :: a(lda, *), b(ldb, *)
          Complex (Kind=dp), Intent (Out) :: taua(min(m,n)), taub(min(p,n)), &
            work(max(1,lwork))
!         .. Intrinsic Procedures ..
          Intrinsic :: max, min
        End Subroutine
        Function zlangb(norm, n, kl, ku, ab, ldab, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: kl, ku, ldab, n
          Character (1) :: norm
          Real (Kind=dp) :: work(*), zlangb
          Complex (Kind=dp) :: ab(ldab, *)
        End Function
        Function zlange(norm, m, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, m, n
          Character (1) :: norm
          Real (Kind=dp) :: work(*), zlange
          Complex (Kind=dp) :: a(lda, *)
        End Function
        Function zlangt(norm, n, dl, d, du)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Complex (Kind=dp) :: d(*), dl(*), du(*)
          Integer :: n
          Character (1) :: norm
          Real (Kind=dp) :: zlangt
        End Function
        Function zlanhb(norm, uplo, n, k, ab, ldab, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: k, ldab, n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: work(*), zlanhb
          Complex (Kind=dp) :: ab(ldab, *)
        End Function
        Function zlanhe(norm, uplo, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: work(*), zlanhe
          Complex (Kind=dp) :: a(lda, *)
        End Function
        Function zlanhp(norm, uplo, n, ap, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Complex (Kind=dp) :: ap(*)
          Integer :: n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: work(*), zlanhp
        End Function
        Function zlanhs(norm, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, n
          Character (1) :: norm
          Real (Kind=dp) :: work(*), zlanhs
          Complex (Kind=dp) :: a(lda, *)
        End Function
        Function zlanht(norm, n, d, e)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Real (Kind=dp) :: d(*), zlanht
          Complex (Kind=dp) :: e(*)
          Integer :: n
          Character (1) :: norm
        End Function
        Function zlansb(norm, uplo, n, k, ab, ldab, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: k, ldab, n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: work(*), zlansb
          Complex (Kind=dp) :: ab(ldab, *)
        End Function
        Function zlansp(norm, uplo, n, ap, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Complex (Kind=dp) :: ap(*)
          Integer :: n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: work(*), zlansp
        End Function
        Function zlansy(norm, uplo, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: work(*), zlansy
          Complex (Kind=dp) :: a(lda, *)
        End Function
        Function zlantb(norm, uplo, diag, n, k, ab, ldab, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Character (1) :: diag, norm, uplo
          Integer :: k, ldab, n
          Real (Kind=dp) :: work(*), zlantb
          Complex (Kind=dp) :: ab(ldab, *)
        End Function
        Function zlantp(norm, uplo, diag, n, ap, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Complex (Kind=dp) :: ap(*)
          Character (1) :: diag, norm, uplo
          Integer :: n
          Real (Kind=dp) :: work(*), zlantp
        End Function
        Function zlantr(norm, uplo, diag, m, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Character (1) :: diag, norm, uplo
          Integer :: lda, m, n
          Real (Kind=dp) :: work(*), zlantr
          Complex (Kind=dp) :: a(lda, *)
        End Function
        Function dlaneg(n, d, lld, sigma, pivmin, r)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Real (Kind=dp) :: d(*), lld(*), pivmin, sigma
          Integer :: dlaneg, n, r
        End Function
        Function dlangb(norm, n, kl, ku, ab, ldab, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: kl, ku, ldab, n
          Character (1) :: norm
          Real (Kind=dp) :: ab(ldab, *), dlangb, work(*)
        End Function
        Function dlange(norm, m, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, m, n
          Character (1) :: norm
          Real (Kind=dp) :: a(lda, *), dlange, work(*)
        End Function
        Function dlangt(norm, n, dl, d, du)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Real (Kind=dp) :: d(*), dl(*), dlangt, du(*)
          Integer :: n
          Character (1) :: norm
        End Function
        Function dlanhs(norm, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, n
          Character (1) :: norm
          Real (Kind=dp) :: a(lda, *), dlanhs, work(*)
        End Function
        Function dlansb(norm, uplo, n, k, ab, ldab, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: k, ldab, n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: ab(ldab, *), dlansb, work(*)
        End Function
        Function dlansp(norm, uplo, n, ap, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Real (Kind=dp) :: ap(*), dlansp, work(*)
          Integer :: n
          Character (1) :: norm, uplo
        End Function
        Function dlanst(norm, n, d, e)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Real (Kind=dp) :: d(*), dlanst, e(*)
          Integer :: n
          Character (1) :: norm
        End Function
        Function dlansy(norm, uplo, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, n
          Character (1) :: norm, uplo
          Real (Kind=dp) :: a(lda, *), dlansy, work(*)
        End Function
        Function dlantb(norm, uplo, diag, n, k, ab, ldab, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Character (1) :: diag, norm, uplo
          Integer :: k, ldab, n
          Real (Kind=dp) :: ab(ldab, *), dlantb, work(*)
        End Function
        Function dlantp(norm, uplo, diag, n, ap, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Real (Kind=dp) :: ap(*), dlantp, work(*)
          Character (1) :: diag, norm, uplo
          Integer :: n
        End Function
        Function dlantr(norm, uplo, diag, m, n, a, lda, work)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Character (1) :: diag, norm, uplo
          Integer :: lda, m, n
          Real (Kind=dp) :: a(lda, *), dlantr, work(*)
        End Function
        Subroutine dlanv2(a, b, c, d, rt1r, rt1i, rt2r, rt2i, cs, sn)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Real (Kind=dp) :: a, b, c, cs, d, rt1i, rt1r, rt2i, rt2r, sn
        End Subroutine
        Subroutine zlacpy(uplo, m, n, a, lda, b, ldb)
!         .. Use Statements ..
          Use lapack_precision, Only: dp
          Integer :: lda, ldb, m, n
          Character (1) :: uplo
          Complex (Kind=dp) :: a(lda, *), b(ldb, *)
        End Subroutine
        Subroutine dlacpy(uplo, m, n, a, lda, b, ldb)
          Integer :: lda, ldb, m, n
          Character (1) :: uplo
          Double Precision :: a(lda, *), b(ldb, *)
        End Subroutine
        Subroutine dlaset(uplo, m, n, alpha, beta, a, lda)
          Integer :: lda, m, n
          Character (1) :: uplo
          Double Precision :: a(lda, *), alpha, beta
        End Subroutine
      End Interface
    End Module
