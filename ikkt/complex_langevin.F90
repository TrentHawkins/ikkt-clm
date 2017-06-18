#     ifndef IKKT_COMPLEX_LANGEVIN_F90
#     define IKKT_COMPLEX_LANGEVIN_F90

#     include "main/precision.F90"
#     include "monte_carlo/monte_carlo.F90"

#     include "tools/conjugate_gradient.F90"

#     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"


      module complex_langevin


            use::monte_carlo

            use::conjugate_gradient_method

            use::constants
            use::fields


            implicit none


            complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                  0:inner_degrees_of_freedom-1,&
                                  0:boson_degrees_of_freedom-1),public::drift
            complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                  0:inner_degrees_of_freedom-1,&
                                  0:boson_degrees_of_freedom-1),public::noise

            public::langevin_step

            public::make_drift
            public::make_noise

            public::drift_norm

      contains


            subroutine boot_langevin()


                  implicit none


                  if(configuration_loaded) then

                     call load_monte_carlo()

                  else

                     call make_monte_carlo()

              end if!configuration_loaded


        end subroutine boot_langevin!



            subroutine langevin_step()


                  implicit none


                  call make_drift()
                  call make_noise()

                  a=a+drift*t%time_step()+noise*sqrt(t%time_step())


        end subroutine langevin_step


            subroutine make_drift()


                  implicit none


                  integer::mu,nu,i,j


                  drift=+.00000e+0

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

                        drift(:,:,mu)&
                       =drift(:,:,mu)+inner_degrees_of_freedom*((a(:,:,mu) &
                                                    .commutation.a(:,:,nu))&
                                                    .commutation.a(:,:,nu))

              end    do!nu=0,boson_degrees_of_freedom-1,+1

                     call update_fermion_matrix()

                     do j=0,inner_degrees_of_freedom-1,+1

                        do i=0,inner_degrees_of_freedom-1,+1

                           call make_fermion_noise()

                           drift(i,j,mu)&
                          =drift(i,j,mu)-determinant_degree(boson_degrees_of_freedom)*(fermion_noise(:) &
                           .o.ma(:,:,i,j,mu).o.conjugate_gradient_K(cmm(:,:),cm(:,:).o.fermion_noise(:),&
                                                                                       fermion_noise(:)))

              end       do!i=0,inner_degrees_of_freedom-1,+1

              end    do!j=0,inner_degrees_of_freedom-1,+1

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_drift


            subroutine make_noise()


                  implicit none


                  call make_boson_noise()

                  noise=boson_noise


        end subroutine make_noise


            function drift_norm()


                  implicit none


                  real(KK)::drift_norm

                  integer::mu


                  drift_norm=+.00000e+0

                  do mu=1,inner_degrees_of_freedom,+1

                     drift_norm&
                    =drift_norm+norm(drift(:,:,mu))

              end do!mu= ,inner_degrees_of_freedom,+1

                  drift_norm&
                 =drift_norm/a_size


        end function drift_norm


  end module complex_langevin


#  endif
