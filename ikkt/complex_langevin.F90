#     ifndef IKKT_COMPLEX_LANGEVIN_F90
#     define IKKT_COMPLEX_LANGEVIN_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/monte_carlo.F90"

#     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"

#     ifndef OPTIMAL

#     include "tools/conjugate_gradient.F90"

#     else

#     include "ikkt/optimal_toolset.F90"
#     include "ikkt/conjugate_gradient.F90"

#  endif

#     include "ikkt/gauge_cooling.F90"


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


            logical,public::fermions_included=.false.

            complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                  0:inner_degrees_of_freedom-1,&
                                  0:boson_degrees_of_freedom-1),public::drift,&
                                                                        noise

            real(KK),public::drift_norm

            public::langevin_step

            public::make_drift
            public::make_drift_norm
            public::make_noise

      contains


            subroutine boot_langevin()


                  implicit none


                  call make_constants(inner_degrees_of_freedom,&
                                      boson_degrees_of_freedom)

#                 ifndef OPTIMAL

                  call make_m_kernel()

#              endif

                  if(configuration_loaded) then

                     call load_monte_carlo()
                     call load_fields()

                  else

                     call make_monte_carlo()
                     call make_fields()

              end if!configuration_loaded


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

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  if(fermions_included) then

                     call make_fermi_noise(standard_deviation/2);f=zero!fermi_noise

#                 ifndef OPTIMAL

                     call update_fermion_matrix()

                     do mu=0,boson_degrees_of_freedom-1,+1

                        do j=0,inner_degrees_of_freedom-1,+1

                           do i=0,inner_degrees_of_freedom-1,+1

                              drift(i,j,mu)&
                             =drift(i,j,mu)+determinant_degree(boson_degrees_of_freedom)*(fermi_noise(:) &
                         .c.(ma(:,:,i,j,mu)              .o.conjugate_gradient(cmm(:,:)                 ,&
                                                                                cm(:,:).o.fermi_noise(:),f(:))))

              end          do!i=0,inner_degrees_of_freedom-1,+1

              end       do!j=0,inner_degrees_of_freedom-1,+1

              end    do!mu=0,boson_degrees_of_freedom-1,+1

#                 else

                     drift&
                    =drift-determinant_degree(boson_degrees_of_freedom)*umav(fermi_noise ,&
                                                  conjugate_gradient(a,cmv(a,fermi_noise),f))

#              endif

                     do mu=0,boson_degrees_of_freedom-1,+1

                        if(massive_deformations) then

                           drift(:,:,mu)&
                          =drift(:,:,mu)-boson_mass(mu)*a(:,:,mu)*epsilon*inner_degrees_of_freedom

              end       if!massive_deformations

              end    do!mu=0,boson_degrees_of_freedom-1,+1

              end if!fermions_included

                  do mu=0,boson_degrees_of_freedom-1,+1

            !        call make_hermitian(drift(:,:,mu),+.10000e+1_KK)
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


                  call make_boson_noise(standard_deviation)

                  noise=boson_noise
            !     noise=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     call make_hermitian(noise(:,:,mu),+.10000e+1_KK)
                     call make_traceless(noise(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_noise!


  end module complex_langevin


#  endif
