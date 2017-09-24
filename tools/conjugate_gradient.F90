#     ifndef CONJUGATE_GRADIENT_F90
#     define CONJUGATE_GRADIENT_F90

#     include "system/precision.F90"

#     include "tensor/tensor.F90"


      module conjugate_gradient_method


            use::tensor_type


            implicit none


            real(KK),parameter::tolerance=TOLERANCEK


            interface conjugate_gradient

                  module procedure conjugate_gradient_K

        end interface conjugate_gradient


      contains


            subroutine conjugate_gradient_K(a,b,x)


                  implicit none


                  complex(KK),dimension( :               ),intent(in   )::b
                  complex(KK),dimension(0:size(b,dim=1)-1,&
                                        0:size(b,dim=1)-1),intent(in   )::a
                  complex(KK),dimension(0:size(b,dim=1)-1),intent(inout)::x

                  complex(KK),dimension(0:size(b,dim=1)-1)              ::r,&
                                                                          p

                  real(KK)::cb,&
                            ca,norm_r_old,&
                               norm_r_new

                  r=b-(a.o.x)
                  p=       r

                  do

                     norm_r_old=norm(r)

                     if(norm_r_new<tolerance) exit

                     cb=-norm_r_old/norm(r,a)

                     x=x-cb*     p
                     r=r+cb*(a.o.p)

                     ca=norm_r_new&
                       /norm_r_old

                     p=r+ca*     p

              end do

                  print *,"success"

        end subroutine conjugate_gradient_K!a,b,x


  end module conjugate_gradient_method


#  endif
