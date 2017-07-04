#     ifndef TOOLS_CONJUGATE_GRADIENT_F90
#     define TOOLS_CONJUGATE_GRADIENT_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"


      module conjugate_gradient_method


            use::tensor_type


            implicit none


            real(KK),parameter::tolerance=TOLERANCEK


            interface conjugate_gradient

                  module procedure conjugate_gradient_K

        end interface conjugate_gradient


      contains


            function conjugate_gradient_K(a,b,x0) result(x1)


                  implicit none


                  complex(KK),dimension( :               ),intent(in   ):: b
                  complex(KK),dimension(0:size(b,dim=1)-1,&
                                        0:size(b,dim=1)-1),intent(in   ):: a
                  complex(KK),dimension(0:size(b,dim=1)-1),intent(inout)::x0
                  complex(KK),dimension(0:size(b,dim=1)-1)              ::r0
                  complex(KK),dimension(0:size(b,dim=1)-1)              ::p0
                  real(   KK)                                           ::cb
                  complex(KK),dimension(0:size(b,dim=1)-1)              ::x1
                  complex(KK),dimension(0:size(b,dim=1)-1)              ::r1
                  real(   KK)                                           ::ca
                  complex(KK),dimension(0:size(b,dim=1)-1)              ::p1


                  r0=b-(a.o.x0)
                  p0=       r0

                  do

                     cb=-norm(r0  )&
                        /norm(r0,a)

                     x1=x0-cb*     p0

                     if(norm(x1-x0)<tolerance) then

                        exit

              end    if!norm(x1-x0)<tolerance)

                     r1=r0+cb*(a.o.p0)

                     ca= norm(r1)&
                        /norm(r0)

                     p1=r1+ca*     p0

                     x0=x1
                     r0=r1
                     p0=p1

              end do


        end function conjugate_gradient_K!a,b,x0


  end module conjugate_gradient_method


#  endif
