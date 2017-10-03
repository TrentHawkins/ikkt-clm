#     ifndef OBSERVABLES_F90
#     define OBSERVABLES_F90

#     include "../system/precision.F90"

#     include "../tensor/tensor.F90"

#     include "../monte_carlo/time.F90"

#     include "../ikkt/fields.F90"

#     ifdef OPTIMAL

#     include "../ikkt/optimal_toolset.F90"

#  endif

#     include "../ikkt/complex_langevin.F90"


      module observables


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

            public::print_observables

            private::faraday_squared
            private::lambda

            public::boson_action
!           public::fermi_action


      contains


            subroutine print_observables(unit)


                  implicit none


                  integer,intent(in   )::unit

                  integer::mu


!                 call make_drift_norm()

                  call write(unit,                   t             )
                       write(unit,format_observables_K,advance="no") faraday_squared()

                  if(massive_deformations) then

                       write(unit,format_observables_K,advance="no") boson_epsilon

                     if(fermions_included_in) then

                       write(unit,format_observables_K,advance="no") fermi_mass

              end    if!fermions_included_in

                     do mu=0,boson_degrees_of_freedom-1,+1

                       write(unit,format_observables_K,advance="no") lambda(mu)

              end    do!mu=0,boson_degrees_of_freedom-1,+1

              end if!massive_deformations

                  write(unit,*)

                  if(measure_time_skipped) then

                     call s%push_time()

              end if!measure_time_skipped


        end subroutine print_observables!unit,measurement_time


            function faraday_squared()


                  implicit none


                  real(KK)::faraday_squared

                  integer::mu,nu


                  faraday_squared=+.00000e+0_KK

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

!                       faraday_squared&
!                      =faraday_squared+norm(a(:,:,mu).commutation.a(:,:,nu))

                        faraday_squared&
                       =faraday_squared-trace((a(:,:,mu).commutation.a(:,:,nu)) &
                                           .o.(a(:,:,mu).commutation.a(:,:,nu)))/inner_degrees_of_freedom

              end    do!nu=0,boson_degrees_of_freedom-1,+1

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end function faraday_squared


            function lambda(k)


                  implicit none


                  integer,intent(in   )::k

                  complex(KK)::lambda


                  lambda=trace(a(:,:,k).o.a(:,:,k))


        end function lambda!k


            function boson_action()


                  implicit none


                  real(KK)::boson_action

                  integer::mu


                  boson_action=faraday_squared()*inner_degrees_of_freedom&
                                                *inner_degrees_of_freedom/4

                  if(massive_deformations) then

                     do mu=0,boson_degrees_of_freedom-1,+1

                        boson_action&
                       =boson_action+boson_mass(mu)*lambda(mu)*boson_epsilon*inner_degrees_of_freedom&
                                                                            *inner_degrees_of_freedom/2

              end    do!mu=0,boson_degrees_of_freedom-1,+1

              end if!massive_deformations


        end function boson_action


!           function fermi_action()


!                 implicit none


!                 complex(KK)::fermi_action


!                 fermi_action&
!                =fermi_action+determinant_degree(boson_degrees_of_freedom)*sum(log(m_eigenvalues_))


!       end function fermi_action


  end module observables


#  endif
