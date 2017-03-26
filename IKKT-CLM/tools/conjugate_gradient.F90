#     ifndef TOOLS_CONJUGATE_GRADIENT_F90
#     define TOOLS_CONJUGATE_GRADIENT_F90

#     include "precision.F90"

#     include "tensor/tensor.F90"


      module conjugate_gradient_method


         use tensor_type


            implicit none


            real(KK),parameter::tolerance=TOLERANCEK


            interface conjugate_gradient

                  module procedure conjugate_gradient_K

        end interface conjugate_gradient


      contains


            function conjugate_gradient_K(a,b,x0,count) result(x1)


                  implicit none


                  complex(KK),intent(in   ):: b( :             )
                  complex(KK),intent(in   ):: a(1:size(b,dim=1),&
                                                1:size(b,dim=1))
                  complex(KK),intent(inout)::x0(1:size(b,dim=1))
                  complex(KK)              ::r0(1:size(b,dim=1))
                  complex(KK)              ::p0(1:size(b,dim=1))
                  real(   KK)              ::cb(1:size(b,dim=1))
                  complex(KK)              ::x1(1:size(b,dim=1))
                  complex(KK)              ::r1(1:size(b,dim=1))
                  real(   KK)              ::ca(1:size(b,dim=1))
                  complex(KK)              ::p1(1:size(b,dim=1))

                  integer,intent(  out),optional::count


                  r0=b-(a.o.x0)
                  p0=       r0

                  if(present(count)) then

                     count=1

                     do

                        cb=-norm(r0  )&
                           /norm(r0,a)

                        x1=x0-cb*     p0

                        if(norm(x1-x0)<=tolerance.or.count>=huge(count)) then

                           exit

              end       if!norm(x1-x0)<=tolerance.or.count>=huge(count)

                        r1=r0+cb*(a.o.p0)

                        ca= norm(r1)&
                           /norm(r0)

                        p1=r1+ca*     p0

                        x0=x1
                        r0=r1
                        p0=p1

                        count&
                       =count+1

              end    do

                  else

                     do

                        cb=-norm(r0  )&
                           /norm(r0,a)

                        x1=x0-cb*     p0

                        if(norm(x1-x0)<=tolerance.or.count>=huge(count)) then

                           exit

              end       if!norm(x1-x0)<=tolerance.or.count>=huge(count)

                        r1=r0+cb*(a.o.p0)

                        ca= norm(r1)&
                           /norm(r0)

                        p1=r1+ca*     p0

                        x0=x1
                        r0=r1
                        p0=p1

              end    do!

              end if!present(count)


        end function conjugate_gradient_K!a,b,x0,count


  end module conjugate_gradient_method


#  endif
