#     ifndef COMPLEX_LANGEVIN_F90
#     define COMPLEX_LANGEVIN_F90

#     include "../system/precision.F90"

#     include "../tensor/tensor.F90"

#     include "../monte_carlo/monte_carlo.F90"

#     include "../ikkt/constants.F90"
#     include "../ikkt/fields.F90"

#     ifndef OPTIMAL

#     include "../tools/conjugate_gradient.F90"

#     else

#     include "../ikkt/optimal_toolset.F90"
#     include "../ikkt/conjugate_gradient.F90"

#  endif

#     include "../ikkt/gauge_cooling.F90"


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

            private::read_field_parameters


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
                             +determinant_degree(boson_degrees_of_freedom)*(fermi_noise(.10000e+1_KK).c.ma(:,:,i,j,mu).o.f(:))

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


            subroutine  read_field_parameters()


                  implicit none


                  integer::mu

!                 character(*)::temp_file_name


                  write(*,"(2a)",advance="no") "inner_degrees_of_freedom: ",t_yellow
                   read(*,   *               )  inner_degrees_of_freedom
                  write(*,"( a)",advance="no")                              t_normal
                  write(*,"(2a)",advance="no") "boson_degrees_of_freedom: ",t_yellow
                   read(*,   *               )  boson_degrees_of_freedom
                  write(*,"( a)",advance="no")                              t_normal

                                                   fermi_degrees_of_freedom=2 &
                                                **(boson_degrees_of_freedom/2 &
                                                                           -1)
                                           n     = inner_degrees_of_freedom-1
                                           d     = boson_degrees_of_freedom-1
                                           p     = fermi_degrees_of_freedom-1
                                           n_size= inner_degrees_of_freedom&
                                                 * inner_degrees_of_freedom
                                           a_size= boson_degrees_of_freedom* n_size
                                           f_size= fermi_degrees_of_freedom*(n_size-1)

!                 write(temp_file_name,"(2(a2,i2.2))") ":n",inner_degrees_of_freedom,&
!                                                      ":d",boson_degrees_of_freedom

!                 conf_file_name&
!                =conf_file_name//trim(temp_file_name)

!                 if(fermions_included_in) then

!                    write(temp_file_name,"(a2,i2.2)") ":p",inner_degrees_of_freedom

!                    conf_file_name&
!                   =conf_file_name//trim(temp_file_name)

!             end if!fermions_included_in

                  write(*,   *               )

                  if(massive_deformations) then

                     write(*,"(2a)",advance="no") "boson_mass_factor: ",t_yellow
                      read(*,   *               )  boson_mass_factor
                     write(*,"( a)",advance="no")                       t_normal

!                    write(temp_file_name,"(a2,f4.2,a2)") ":b",boson_mass_factor,":m"

!                    conf_file_name&
!                   =conf_file_name//trim(temp_file_name)

                     write(*,   *               )

                     if(allocated(boson_mass)) deallocate(boson_mass)
                                                 allocate(boson_mass(0:boson_degrees_of_freedom-1))

                     do mu=0,boson_degrees_of_freedom-1,+1

                        write(*,"(a,i2.2,2a)",advance="no") "boson_mass[",mu+1,"]: ",t_yellow
                         read(*,          *               )  boson_mass(  mu    )
                        write(*,"(        a)",advance="no")                          t_normal

!                       write(temp_file_name,"(sp,f4.1)") log2(boson_mass(mu))

!                       conf_file_name&
!                      =conf_file_name//trim(temp_file_name)

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     write(*,          *               )

                     if(fermions_included_in) then

                        write(*,"(2a)",advance="no") "fermi_mass: ",t_yellow
                         read(*,   *               )  fermi_mass
                        write(*,"( a)",advance="no")                t_normal

!                       write(temp_file_name,"(a2,f4.2)") ":f",fermi_mass

!                       conf_file_name&
!                      =conf_file_name//trim(temp_file_name)

                        write(*,   *               )

              end    if!fermions_included_in

              end if!massive_deformations

                  if(allocated(a)) deallocate(a)
                                     allocate(a(0:inner_degrees_of_freedom-1,&
                                                0:inner_degrees_of_freedom-1,&
                                                0:boson_degrees_of_freedom-1))

                  if(fermions_included_in) then

#                    ifdef OPTIMAL

                     if(allocated(f)) deallocate(f)
                                        allocate(f(0:inner_degrees_of_freedom-1,&
                                                   0:inner_degrees_of_freedom-1,&
                                                   0:fermi_degrees_of_freedom-1))

#                    else

                     if(allocated(f)) deallocate(f)
                                        allocate(f(0:f_size-1))

                     if(allocated(m_eigenvalues_)) deallocate(m_eigenvalues_)
                                                     allocate(m_eigenvalues_(0:f_size-1))

                     if(allocated( m )) deallocate( m )
                                          allocate( m (0:f_size-1,&
                                                       0:f_size-1))
                     if(allocated(cm )) deallocate(cm )
                                          allocate(cm (0:f_size-1,&
                                                       0:f_size-1))
                     if(allocated(cmm)) deallocate(cmm)
                                          allocate(cmm(0:f_size-1,&
                                                       0:f_size-1))
                     if(allocated( ma)) deallocate( ma)
                                          allocate( ma(0:f_size-1,&
                                                       0:f_size-1,0:inner_degrees_of_freedom-1,&
                                                                  0:inner_degrees_of_freedom-1,&
                                                                  0:boson_degrees_of_freedom-1))

#                 endif

              end if!fermions_included_in


        end subroutine  read_field_parameters!


            subroutine eject_complex_langevin()


                  implicit none


                  call eject_gauge_cooler()
                  call eject_constants()

                  if(allocated(drift)) deallocate(drift)
                  if(allocated(noise)) deallocate(noise)


        end subroutine eject_complex_langevin


  end module complex_langevin


#  endif
