#     ifndef SIMULATION_COMPLEX_LANGEVIN_F90
#     define SIMULATION_COMPLEX_LANGEVIN_F90

#     include "../system/precision.F90"

#     include "../tools/tensor/tensor.F90"

#     include "../monte_carlo/monte_carlo.F90"

#     include "../simulation/constants.F90"
#     include "../simulation/fields.F90"

#     ifndef OPTIMAL

#     include "../tools/conjugate_gradient.F90"

#     else

#     include "../simulation/optimal_toolset.F90"
#     include "../simulation/conjugate_gradient.F90"

#  endif

#     include "gauge_cooling.F90"


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
                                   :                          ),allocatable,public::drift
            complex(KK),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::noise

            public::boot_langevin
            public::langevin_step
            public::wrap_langevin

            public::make_drift
            public::make_noise
            public::drift_norm


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
                 =a+drift*     time(step) &
                   +noise*sqrt(time(step))

                  if(gauge_cooling_active) call apply_cooling()
                  if(timestep_is_adaptive) call agnostically_adapt_time_step(drift_norm())


        end subroutine langevin_step!


            subroutine wrap_langevin()


                  implicit none


                  call save_monte_carlo()
                  call save_fields()


        end subroutine wrap_langevin!


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
                       =drift(:,:,mu)-boson_mass(mu)*a(:,:,mu)*boson_mass_factor*inner_degrees_of_freedom

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
                             +determinant_factor(boson_degrees_of_freedom)*(fermi_noise(.10000e+1_KK).c.ma(:,:,i,j,mu).o.f(:))

              end          do!i=0,inner_degrees_of_freedom-1,+1

              end       do!j=0,inner_degrees_of_freedom-1,+1

              end    do!mu=0,boson_degrees_of_freedom-1,+1

#                 else

                     call conjugate_gradient(a,cmv(a,fermi_noise(.10000e+1_KK)),f)

                     drift&
                    =drift-determinant_factor(boson_degrees_of_freedom)*umav(fermi_noise(.10000e+1_KK),f)

#              endif

              end if!fermions_included_in

                  do mu=0,boson_degrees_of_freedom-1,+1

!                    call make_hermitian(drift(:,:,mu))
                     call make_traceless(drift(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_drift!


            subroutine make_noise()


                  implicit none


                  integer::mu


                  noise=boson_noise(.20000e+1_KK)


        end subroutine make_noise!


            function drift_norm()


                  implicit none


                  real(KK)::drift_norm

                  integer::mu


                  drift_norm=+.00000e+0_KK

                  do mu=1,boson_degrees_of_freedom-1,+1

                     drift_norm&
                    =drift_norm+norm(drift(:,:,mu))

              end do!mu= ,boson_degrees_of_freedom-1,+1

                       drift_norm&
                 =sqrt(drift_norm/a_size)


        end function drift_norm!


            subroutine print_observables(unit)


                  implicit none


                  integer,intent(inout)::unit

                  integer::mu


                  call print_time(unit,life,"DT"); call print_time              (unit,step); write(unit,*)
                  call print_time(unit,life,"CG"); call print_conjugate_gradient(unit     ); write(unit,*)
                  call print_time(unit,life,"GC"); call print_gauge_cooling     (unit     ); write(unit,*)
                                                                                             write(unit,*)


        end subroutine print_observables!unit


            subroutine eject_complex_langevin()


                  implicit none


                  call eject_gauge_cooler()
                  call eject_constants()

                  if(allocated(drift)) deallocate(drift)
                  if(allocated(noise)) deallocate(noise)


        end subroutine eject_complex_langevin


  end module complex_langevin


#  endif
