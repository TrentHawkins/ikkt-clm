#     ifndef IKKT_OBSERVABLES_F90
#     define IKKT_OBSERVABLES_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/time.F90"

#     include "ikkt/fields.F90"

#     ifdef OPTIMAL

#     include "ikkt/optimal_toolset.F90"

#  endif

#     include "ikkt/complex_langevin.F90"


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
            public::fermi_action


      contains


            subroutine print_observables(unit)


                  implicit none


                  integer        ,intent(in   )::unit

                  integer::mu


            !     call make_drift_norm()

                  call write(unit,                   t             )
                       write(unit,format_observables_K,advance="no") faraday_squared()

                  if(massive_deformations) then

                       write(unit,format_observables_K,advance="no") epsilon,fermi_mass

                     do mu=0,boson_degrees_of_freedom-1,+1

                       write(unit,format_observables_K,advance="no") lambda(mu)

              end    do!mu=0,boson_degrees_of_freedom-1,+1

              end if!massive_deformations

                  write(unit,*)


        end subroutine print_observables!unit,measurement_time


            function faraday_squared()


                  implicit none


                  real(KK)::faraday_squared

                  integer::mu,nu


                  faraday_squared=+.00000e+0_KK

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

            !           faraday_squared&
            !          =faraday_squared+norm(a(:,:,mu).commutation.a(:,:,nu))

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

#                 ifdef SYMMETRY_BREAKING

                  integer::mu

#              endif


                  boson_action=faraday_squared()*inner_degrees_of_freedom&
                                                *inner_degrees_of_freedom/4

#                 ifdef SYMMETRY_BREAKING

                  do mu=0,boson_degrees_of_freedom-1,+1

                     boson_action&
                    =boson_action+boson_mass(mu)*lambda(mu)*epsilon*inner_degrees_of_freedom&
                                                                   *inner_degrees_of_freedom/2

              end do!mu=0,boson_degrees_of_freedom-1,+1

#              endif


        end function boson_action


            function fermi_action()


                  implicit none


                  complex(KK)::fermi_action


                  fermi_action&
                 =fermi_action+determinant_degree(boson_degrees_of_freedom)*sum(log(m_eigenvalues0&
                                                                                   /m_eigenvalues1))


        end function fermi_action


  end module observables


#  endif
