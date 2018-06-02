#     ifndef SIMULATION_TEMPLATE_LANGEVIN_DRIFT_F90
#     define SIMULATION_TEMPLATE_LANGEVIN_DRIFT_F90

#     include "system/precision.F90"

#     include "tools/tensor/tensor.F90"

#     include "simulation/constants.F90"
#     include "simulation/fields.F90"

#     ifndef OPTIMAL

#     include "tools/conjugate_gradient.F90"

#     else

#     include "simulation/ikkt/optimal_toolset.F90"
#     include "simulation/conjugate_gradient.F90"

#  endif

!     Find & Replace (NOT case-sensitive) "template_" with the name of your own drift term. For example: "boson_", "mass_", e.t.c.
!     Include your own libraries here. Do not forget the relevant USE statements for any modules that reside in said libraries.

#


      module template_langevin_drift


            use::tensor_type

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
                                   :                          ),allocatable,public::drift_template

            public::make_drift_template
            public::norm_drift_template

            private::read_field_template_parameters


      contains


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


  end module drift_langevin_template


#  endif
