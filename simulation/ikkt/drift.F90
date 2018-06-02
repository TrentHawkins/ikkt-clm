#     ifndef SIMULATION_IKKT_LANGEVIN_DRIFT_F90
#     define SIMULATION_IKKT_LANGEVIN_DRIFT_F90

#     include "system/precision.F90"

#     include "tools/tensor/tensor.F90"

#     include "simulation/ikkt/constants.F90"
#     include "simulation/ikkt/fields.F90"

#     ifndef OPTIMAL

#     include "tools/conjugate_gradient.F90"

#     else

#     include "simulation/ikkt/optimal_toolset.F90"
#     include "simulation/ikkt/conjugate_gradient.F90"

#  endif

#


      module ikkt_drift


            use::tensor_type

            use::conjugate_gradient_method

            use::constants
            use::fields

#           ifdef OPTIMAL

            use::optimal_toolset

#        endif


            implicit none


            complex(KK),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable::ikkt_drift

            public::make_ikkt_drift
            public::norm_ikkt_drift

            private::read_field_ikkt_parameters


      contains


            subroutine make_drift()


                  implicit none


                  integer::mu,nu,i,j


                  ikkt_drift=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

                        ikkt_drift(:,:,mu)&
                       =ikkt_drift(:,:,mu)-((a(:,:,mu).commutation.a(:,:,nu)).commutation.a(:,:,nu))*inner_degrees_of_freedom

              end    do!nu=0,boson_degrees_of_freedom-1,+1

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  f=zero!fermi_noise(.10000e+1_KK)

#                 ifndef OPTIMAL

                  call update_fermion_matrix()

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do j=0,inner_degrees_of_freedom-1,+1

                        do i=0,inner_degrees_of_freedom-1,+1

                           call conjugate_gradient(cmm(:,:),cm(:,:).o.fermi_noise(one),f(:))

                           ikkt_drift(i,j,mu)&
                          =ikkt_drift(i,j,mu)+determinant_factor(boson_degrees_of_freedom)
                                             *(fermi_noise(one).c.ma(:,:,i,j,mu).o.f(:))

              end       do!i=0,inner_degrees_of_freedom-1,+1

              end    do!j=0,inner_degrees_of_freedom-1,+1

              end do!mu=0,boson_degrees_of_freedom-1,+1

#                 else

                  call conjugate_gradient(a,cmv(a,fermi_noise(one)),f)

                  drift&
                 =drift-determinant_factor(boson_degrees_of_freedom)*umav(fermi_noise(one),f)

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

                  if(allocated(a)) deallocate(a)
                                     allocate(a(0:inner_degrees_of_freedom-1,&
                                                0:inner_degrees_of_freedom-1,&
                                                0:boson_degrees_of_freedom-1))

#                 ifdef OPTIMAL

                  if(allocated(f)) deallocate(f)
                                     allocate(f(0:inner_degrees_of_freedom-1,&
                                                0:inner_degrees_of_freedom-1,&
                                                0:fermi_degrees_of_freedom-1))

#                 else

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


        end subroutine  read_field_parameters!


  end module drift_langevin_template


#  endif
