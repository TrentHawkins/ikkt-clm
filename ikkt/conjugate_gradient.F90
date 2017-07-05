#     ifndef IKKT_CONJUGATE_GRADIENT_F90
#     define IKKT_CONJUGATE_GRADIENT_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/fields.F90"
#     include "ikkt/optimal_toolset.F90"


      module conjugate_gradient_method


            use::tensor_type

            use::fields
            use::optimal_toolset


            implicit none


            real(KK),parameter::tolerance=TOLERANCEK


            interface conjugate_gradient

                  module procedure conjugate_gradient_K

        end interface conjugate_gradient


      contains


            function conjugate_gradient_K(a,b,x0) result(x1)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::b
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(inout)::x0

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::r0,p0,x1,r1,p1

                  real(KK)::cb,ca


                  r0=b-cmmv(a,x0);p0=r0

                  do

                     cb=-vv(r0)/vcmmv(a,r0);x1=x0-cb*p0

                     print *,vv(x1-x0)

                     if(sqrt(vv(x1-x0))<tolerance) then

                        exit

              end    if!sqrt(vv(x1-x0))<tolerance

                     r1=r0+cb*cmmv(a,p0);ca=vv(r1)/vv(r0);p1=r1+ca*p0

                     x0=x1
                     r0=r1
                     p0=p1

              end do


        end function conjugate_gradient_K!a,b,x0


  end module conjugate_gradient_method


#  endif
