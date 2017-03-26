#     ifndef IKKT_COMPLEX_LANGEVIN_F90
#     define IKKT_COMPLEX_LANGEVIN_F90

#     include "precision.F90"

#     include "monte_carlo/monte_carlo.F90"

#     include "tools/conjugate_gradient.F90"

#     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"


      module complex_langevin


         use conjugate_gradient_method

         use monte_carlo

         use constants
         use fields


            implicit none


            complex(KK),dimension(1:inner_degrees_of_freedom,&
                                  1:inner_degrees_of_freedom,&
                                  1:boson_degrees_of_freedom)::drift
            complex(KK),dimension(1:inner_degrees_of_freedom,&
                                  1:inner_degrees_of_freedom,&
                                  1:boson_degrees_of_freedom)::noise

      contains


            subroutine make_drift()


                  implicit none


                  integer::mu,nu,i,j


                  drift= .00000e+0

                  do mu=1,boson_degrees_of_freedom,+1

                     do nu=1,boson_degrees_of_freedom,+1

                        drift(:,:,mu)&
                       =drift(:,:,mu)+inner_degrees_of_freedom*((a(:,:,mu).commutation.a(:,:,nu))&
                                                                          .commutation.a(:,:,nu))

              end    do!nu=1,boson_degrees_of_freedom,+1

                     do j =1,inner_degrees_of_freedom,+1

                        do i =1,inner_degrees_of_freedom,+1

                           call make_fermion_noise()

                             drift(i,j,mu) &
                          =  drift(i,j,mu) -determinant_degree(boson_degrees_of_freedom)*(fermion_noise &
                         .o.ma(:,:,i,j,mu).o.               conjugate_gradient_K(cmm,cm.o.fermion_noise,&
                                                                                          fermion_noise))

              end       do!i =1,inner_degrees_of_freedom,+1

              end    do!j =1,inner_degrees_of_freedom,+1

              end do!mu=1,boson_degrees_of_freedom,+1


        end subroutine make_drift


            subroutine make_noise()


                  implicit none


                  noise=boson_noise


        end subroutine make_noise


            subroutine update_fields()


                  implicit none


                  a=a+drift*t%time_step()+noise*sqrt(t%time_step())


        end subroutine update_fields


  end module complex_langevin


#  endif
