#     ifndef CONJUGATE_GRADIENT_F90
#     define CONJUGATE_GRADIENT_F90

#     include "system/precision.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/fields.F90"
#     include "ikkt/tools/optimal_toolset.F90"


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


            subroutine conjugate_gradient_K(a,b,x)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::b
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(inout)::x

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::r,&
                                                                       p

                  real(KK)::cb,&
                            ca,norm_r_old,&
                               norm_r_new


                  r=b-cmmv(a,x)
                  p=r

                  do

                     norm_r_old=ucv(r,r)

                     cb=-norm_r_old/ucmmv(r,a,r)

                     x=x-cb*       p
                     r=r+cb*cmmv(a,p)

                     norm_r_new=ucv(r,r)

                     if(sqrt(norm_r_new)<tolerance) exit

                     ca=norm_r_new&
                       /norm_r_old

                     p=r+ca*       p

              end do

                  print *,"success"


        end subroutine conjugate_gradient_K!a,b,x


  end module conjugate_gradient_method


#  endif
