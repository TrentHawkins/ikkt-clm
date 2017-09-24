#     ifndef BRENT_MINIMIZATION_F90
#     define BRENT_MINIMIZATION_F90

#     include "../system/precision.F90"


!     BRENT MINIMIZATION
!
!     Given a function f, and given a bracketing triplet of abscissas ax, bx, cx (such that bx is between ax and cx, and f(bx) is
!     less than both f(ax) and f(cx) ), this routine isolates the minimum to a fractional precision of about tol using brent’s
!     method. The abscissa of the minimum is returned as xmin, and the minimum function value is returned as brent, the returned
!     function value.
!
!     parameters:
!     maximum allowed number of iterations
!     golden ratio
!     a small number that protects against trying to achieve fractional accuracy for a minimum that happens to be exactly zero


      module brent_minimization


            implicit none


            interface brent

               module procedure brent_K

        end interface brent


      contains


            function brent_K(ax,bx,cx,f,tol,xmin)


                  implicit none


                  integer ::itmax;parameter(itmax=100)

                  real(KK)::brent_K

                  real(KK)::ax
                  real(KK)::bx
                  real(KK)::cx
                  real(KK)::tol
                  real(KK)::xmin
                  real(KK)::f;external f

                  real(KK)::cgold;parameter(cgold=(+.30000e+1_KK  &
                                             -sqrt(+.50000e+1_KK))&
                                             /     +.20000e+1_KK)
                  real(KK)::zeps ;parameter(zeps = +.10000e-9_KK)
                  integer ::iter
                  real(KK)::a
                  real(KK)::b
                  real(KK)::d
                  real(KK)::e
                  real(KK)::etemp
                  real(KK)::fu
                  real(KK)::fv
                  real(KK)::fw
                  real(KK)::fx
                  real(KK)::p
                  real(KK)::q
                  real(KK)::r
                  real(KK)::tol1
                  real(KK)::tol2
                  real(KK)::u
                  real(KK)::v
                  real(KK)::w
                  real(KK)::x
                  real(KK)::xm

!                 a and b must be in ascending order, though the input abscissas need not be.

                  a=min(ax,cx)
                  b=max(ax,cx)
                  v=bx

!                 Initializations...

                  w=v
                  x=v

!                 This will be the distance moved on the step before last.

                  e=+.00000e+0_KK
                  fx=f(x)
                  fv=fx
                  fw=fx

!                 Main program loop.

                  do iter=1,itmax

                     xm=+.50000e+0_KK*(a+b)
                     tol1=tol*abs(x)+zeps
                     tol2=+.20000e+1_KK*tol1

!                    Test for done here.
                     if(abs(x-xm).le.(tol2-.50000e+0_KK*(b-a))) then

                        goto 103

              end    if!abs(x-xm).le.(tol2-.50000e+0_KK*(b-a))

!                    Construct a trial parabolic fit.

                     if(abs(e).gt.tol1) then

                        r=(x-w)*(fx-fv)
                        q=(x-v)*(fx-fw)
                        p=(x-v)*q-(x-w)*r
                        q=+.20000e+1_KK*(q-r)

                        if(q.gt.+.00000e+0_KK) then

                           p=-p

              end       if!q.gt.0.0

                        q=abs(q)
                        etemp=e
                        e=d

                        if(abs(p).ge.abs(+.50000e+0_KK*q*etemp).or.p.le.q*(a-x).or.p.ge.q*(b-x)) then

                           goto 101

              end       if!abs(p).ge.abs(+.50000e+0_KK*q*etemp).or.p.le.q*(a-x).or.p.ge.q*(b-x)

!                       The above conditions determine the acceptability of the parabolic fit. here it is O.K.:
!                       Take the parabolic step.

                        d=p/q
                        u=x+d

                        if(u-a.lt.tol2.or.b-u.lt.tol2) then

                           d=sign(tol1,xm-x)

              end       if!u-a.lt.tol2or.b-u.lt.tol2

!                       Skip over the golden section step.

                        goto 102

              end    if!abs(e).gt.tol1

!                    We arrive here for a golden section step, which we take into the larger of the two segments.

              101    if(x.ge.xm) then

                        e=a-x

                     else

                        e=b-x

              end    if!x.ge.xm

!                    Take the golden section step.

                     d=cgold*e

!                    Arrive here with d computed either from parabolic fit, or else from golden section.

              102    if(abs(d).ge.tol1) then

                        u=x+d

                     else

                        u=x+sign(tol1,d)

              end    if!abs(d).ge.tol1

                     fu=f(u)

!                    This is the one function evaluation per iteration, and now we have to decide what to do with our evaluation.
!                    Housekeeping follows:

                     if(fu.le.fx) then

                        if(u.ge.x) then

                           a=x

                        else

                           b=x

              end       if!u.ge.x

                        v=w
                        fv=fw
                        w=x
                        fw=fx
                        x=u
                        fx=fu

                     else

                        if(u.lt.x) then

                           a=u

                        else

                           b=u

              end       if!u.lt.x

                        if(fu.le.fw.or.w.eq.x) then

                           v=w
                           fv=fw
                           w=u
                           fw=fu

                        else

                           if(fu.le.fv.or.v.eq.x.or.v.eq.w) then

                              v=u
                              fv=fu

              end          if!fu.le.fv.or.v.eq.x.or.v.eq.w

              end       if!fu.le.fw.or.w.eq.x

              end    if!fu.le.fx

!                    Done with housekeeping. Back for another iteration.

              end do!iter=1,itmax

!                 Pause ’brent exceed maximum iterations’.
!                 Arrive here ready to exit with best values.

              103 xmin=x
                  brent_K=fx


                  return


        end function brent_K


  end module brent_minimization


#       endif
