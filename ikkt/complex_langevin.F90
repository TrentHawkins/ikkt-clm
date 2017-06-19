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


            logical,public::fermions_included=.false.

            complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                  0:inner_degrees_of_freedom-1,&
                                  0:boson_degrees_of_freedom-1),public::drift
            complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                  0:inner_degrees_of_freedom-1,&
                                  0:boson_degrees_of_freedom-1),public::noise

            real(KK),public::drift_norm

            public::langevin_step

            public::make_drift
            public::make_noise

            public::make_drift_norm

      contains


            subroutine boot_langevin()


                  implicit none


                  call make_constants(inner_degrees_of_freedom,&
                                      boson_degrees_of_freedom)

                  call make_m_kernel()

                  if(configuration_loaded) then

                     call load_monte_carlo()
                     call load_fields()

                  else

                     call make_monte_carlo()
                     call make_fields(start_field_is_hot)

              end if!configuration_loaded


        end subroutine boot_langevin!


            subroutine langevin_step()


                  implicit none


                  call make_drift()
                  call make_noise()

                  a=noise*sqrt(t%time_step())


        end subroutine langevin_step


            subroutine make_drift()


                  implicit none


                  integer::mu,nu,i,j


                  drift=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

                        drift(:,:,mu)&
                       =drift(:,:,mu)-((a(:,:,mu) &
                           .commutation.a(:,:,nu))&
                           .commutation.a(:,:,nu))*inner_degrees_of_freedom

              end    do!nu=0,boson_degrees_of_freedom-1,+1

                     if(fermions_included) then

                        call update_fermion_matrix()

                        do j=0,inner_degrees_of_freedom-1,+1

                           do i=0,inner_degrees_of_freedom-1,+1

                              call make_fermi_noise(stddev)

                              drift(i,j,mu)&
                             =drift(i,j,mu)-determinant_degree(boson_degrees_of_freedom)*(fermi_noise(:) &
                          .o.ma(:,:,i,j,mu)            .o.conjugate_gradient_K(cmm(:,:),&
                                                                                cm(:,:).o.fermi_noise(:),&
                                                                                          fermi_noise(:)))

              end          do!i=0,inner_degrees_of_freedom-1,+1

              end       do!j=0,inner_degrees_of_freedom-1,+1

              end    if!fermions_included

                     call make_traceless(drift(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_drift


            subroutine make_noise()


                  implicit none


                  call make_boson_noise(stddev)

                  noise=boson_noise


        end subroutine make_noise


            subroutine make_drift_norm()


                  implicit none


                  integer::mu


                  drift_norm=+.00000e+0_KK

                  do mu=1,inner_degrees_of_freedom,+1

                     drift_norm&
                    =drift_norm+norm(drift(:,:,mu))

              end do!mu= ,inner_degrees_of_freedom,+1

                  drift_norm&
                 =drift_norm/a_size


        end subroutine make_drift_norm


  end module complex_langevin


#  endif
