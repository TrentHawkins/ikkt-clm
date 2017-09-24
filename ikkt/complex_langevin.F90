#     ifndef COMPLEX_LANGEVIN_F90
#     define COMPLEX_LANGEVIN_F90

#     include "system/precision.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/monte_carlo.F90"

#     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"

#     ifndef OPTIMAL

#     include "tools/conjugate_gradient.F90"

#     else

#     include "ikkt/tools/optimal_toolset.F90"
#     include "ikkt/tools/conjugate_gradient.F90"

#  endif

#     include "ikkt/tools/gauge_cooling.F90"


      module complex_langevin


            use::tensor_type

            use::monte_carlo

            use::conjugate_gradient_method

            use::constants
            use::fields

#           ifdef OPTIMAL

            use::optimal_toolset

#        endif

            use::gauge_cooling


            implicit none


            complex(KK),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::drift,&
                                                                                    noise

            real(KK),public::drift_norm

            public::langevin_step

            public::make_drift     ,&
                    make_noise     ,&
                    make_drift_norm


      contains


            subroutine boot_langevin()


                  implicit none


                  if(configuration_loaded) then

                     call load_monte_carlo()
                     call load_fields()

                  else

                     call make_monte_carlo()
                     call make_fields()

              end if!configuration_loaded

                  call make_constants(inner_degrees_of_freedom,&
                                      boson_degrees_of_freedom)

                  if(allocated(drift)) deallocate(drift)
                                         allocate(drift(0:inner_degrees_of_freedom-1,&
                                                        0:inner_degrees_of_freedom-1,&
                                                        0:boson_degrees_of_freedom-1))
                  if(allocated(noise)) deallocate(noise)
                                         allocate(noise(0:inner_degrees_of_freedom-1,&
                                                        0:inner_degrees_of_freedom-1,&
                                                        0:boson_degrees_of_freedom-1))


        end subroutine boot_langevin!


            subroutine langevin_step()


                  implicit none


                  call make_drift()
                  call make_noise()

                  a&
                 =a+drift*     t%time_step()&
                   +noise*sqrt(t%time_step())

                  if(gauge_cooling_active) then

                     call apply_cooling()

              end if!gauge_cooling_active

                  if(timestep_is_variable) then

                     call make_drift_norm()
                     call t%push_time(drift_norm)

                  else

                     call t%push_time()

              end if!timestep_is_variable


        end subroutine langevin_step!


            subroutine make_drift()


                  implicit none


                  integer::mu,nu,i,j


                  drift=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

                        drift(:,:,mu)&
                       =drift(:,:,mu)-((a(:,:,mu).commutation.a(:,:,nu)).commutation.a(:,:,nu))*inner_degrees_of_freedom

              end    do!nu=0,boson_degrees_of_freedom-1,+1

                     if(massive_deformations) then

                        drift(:,:,mu)&
                       =drift(:,:,mu)-boson_mass(mu)*a(:,:,mu)*boson_epsilon*inner_degrees_of_freedom

              end    if!massive_deformations

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  if(fermions_included_in) then

                     f=zero!fermi_noise(.10000e+1_KK)

#                 ifndef OPTIMAL

                     call update_fermion_matrix()

                     do mu=0,boson_degrees_of_freedom-1,+1

                        do j=0,inner_degrees_of_freedom-1,+1

                           do i=0,inner_degrees_of_freedom-1,+1

                              call conjugate_gradient(cmm(:,:),cm(:,:).o.fermi_noise(.10000e+1_KK),f(:))

                              drift(i,j,mu)&
                             =drift(i,j,mu)&
                             +determinant_degree(boson_degrees_of_freedom)*(fermi_noise(.10000e+1_KK).c.(ma(:,:,i,j,mu).o.f(:)))

              end          do!i=0,inner_degrees_of_freedom-1,+1

              end       do!j=0,inner_degrees_of_freedom-1,+1

              end    do!mu=0,boson_degrees_of_freedom-1,+1

#                 else

                     call conjugate_gradient(a,cmv(a,fermi_noise(.10000e+1_KK)),f)

                     drift&
                    =drift-determinant_degree(boson_degrees_of_freedom)*umav(fermi_noise(.10000e+1_KK),f)

#              endif

              end if!fermions_included_in

                  do mu=0,boson_degrees_of_freedom-1,+1

            !        call make_hermitian(drift(:,:,mu))
                     call make_traceless(drift(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_drift!


            subroutine make_drift_norm()


                  implicit none


                  integer::mu


                  drift_norm=+.00000e+0_KK

                  do mu=1,boson_degrees_of_freedom-1,+1

                     drift_norm&
                    =drift_norm+norm(drift(:,:,mu))

              end do!mu= ,boson_degrees_of_freedom-1,+1

                  drift_norm&
                 =drift_norm/a_size


        end subroutine make_drift_norm!


            subroutine make_noise()


                  implicit none


                  integer::mu


                  noise=boson_noise(.20000e+1_KK)


        end subroutine make_noise!


            subroutine eject_complex_langevin()


                  implicit none


                  call eject_gauge_cooler()
                  call eject_constants()

                  if(allocated(drift)) deallocate(drift)
                  if(allocated(drift)) deallocate(noise)


        end subroutine eject_complex_langevin


  end module complex_langevin


#  endif
