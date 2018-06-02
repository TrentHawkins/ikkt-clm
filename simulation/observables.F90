#     ifndef SIMULATION_OBSERVABLES_F90
#     define SIMULATION_OBSERVABLES_F90

#     include "../system/precision.F90"

#     include "../tools/constants.F90"
#     include "../tools/tensor/tensor.F90"

#     include "../monte_carlo/time.F90"

#     include "fields.F90"

#     ifdef OPTIMAL

#     include "../simulation/optimal_toolset.F90"

#  endif

#     include "../simulation/complex_langevin.F90"


      module observables


            use::mathematical_constants

            use::tensor_type

            use::time_type

            use::fields

#           ifdef OPTIMAL

            use::optimal_toolset

#        endif

            use::complex_langevin


            implicit none


            character(*),private,parameter::           format_observables_K=COMPLEXGK
            character(*),private,parameter::text_field_format_observables_K=COMPLEXAK

            character(:),allocatable,public::meas_file_name
            integer                 ,public::meas_unit

            complex(KK),                                      private::aa_aa
            complex(KK),dimension(0:boson_degrees_of_freedom),private::aa

            public::prepare_observables
            public::print_faraday_squared
            public::boson_action
!           public::fermi_action
            public::print_action
            public::      lambda
            public::print_lambda
            public::print_observables


      contains


            subroutine prepare_observables()


                  implicit none


                  complex(KK)::aa_aa

                  integer::mu,nu


                  aa_aa=zero; aa(boson_degrees_of_freedom)=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

                        aa_aa&
                       =aa_aa-trace((a(:,:,mu).commutation.a(:,:,nu)) &
                             *      (a(:,:,mu).commutation.a(:,:,nu)))

              end    do!nu=0,boson_degrees_of_freedom-1,+1

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  do mu=0,boson_degrees_of_freedom-1,+1

                     aa(mu)=trace(a(:,:,k).o.a(:,:,k))

                     aa(boson_degrees_of_freedom)&
                    =aa(boson_degrees_of_freedom)+aa(mu)

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine prepare_observables!


            subroutine print_faraday_squared(unit)


                  implicit none


                  integer,intent(inout)::unit


                  write(unit,format_observables_K,advanced="no") aa_aa/inner_degrees_of_freedom


        end subroutine print_faraday_squared!unit


            function boson_action()


                  implicit none


                  complex(KK)::boson_action
                  integer    ::mu


                  boson_action=aa_aa*inner_degrees_of_freedom/4

                  if(massive_deformations) then

                     do mu=0,boson_degrees_of_freedom-1,+1

                        boson_action&
                       =boson_action+boson_mass(mu)*aa(mu)*boson_mass_factor*inner_degrees_of_freedom/2

              end    do!mu=0,boson_degrees_of_freedom-1,+1

              end if!massive_deformations


        end function boson_action!


!           function fermi_action()


!                 implicit none


!                 complex(KK)::fermi_action


!                 fermi_action=determinant_factor(boson_degrees_of_freedom)*sum(log(m_eigenvalues_))


!       end function fermi_action!


            subroutine print_action(unit)


                  implicit none


                  integer,intent(inout)::unit


                  write(unit,format_observables_K,advance="no") boson_action()
!                 write(unit,format_observables_K,advance="no") fermi_action()
!                 write(unit,format_observables_K,advance="no") boson_action()&
!                                                              +fermi_action()


        end subroutine print_action!unit


            subroutine print_lambda(unit)


                  implicit none


                  integer,intent(inout)::unit

                  integer::mu


                  do mu=0,boson_degrees_of_freedom-1,+1

                     write(unit,format_observables_K) aa(mu)/inner_degrees_of_freedom

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  write(unit,format_observables_K) aa(boson_degrees_of_freedom)/inner_degrees_of_freedom


        end subroutine print_lambda!unit


            subroutine print_observables(unit)


                  implicit none


                  integer,intent(inout)::unit

                  integer::mu


                  call print_time(unit,skip,"FF"); call print_faraday_squared(unit); write(unit,*)
                  call print_time(unit,skip,"SB"); call print_action         (unit); write(unit,*)
                  call print_time(unit,skip,"AA"); call print_lambda         (unit); write(unit,*)
                                                                                     write(unit,*)
                                                                                     write(unit,*)


        end subroutine print_observables!unit


  end module observables


#  endif
