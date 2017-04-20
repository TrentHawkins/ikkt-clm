#     ifndef IKKT_OBSERVABLES_F90
#     define IKKT_OBSERVABLES_F90

#     include "precision.F90"
#     include "interface.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/time.F90"

#     include "ikkt/fields.F90"
#     include "ikkt/complex_langevin.F90"


      module observables


            use::interface

            use::tensor_type

            use::time_type

            use::fields
            use::complex_langevin


            implicit none


            character(*),private,parameter::           format_observables_K=COMPLEXGK
            character(*),private,parameter::text_field_format_observables_K=COMPLEXAK

            public::print_observables

            private::  boson_action
            private::fermion_action


      contains


            subroutine print_observables(unit,measurement_time)


                  implicit none


                  integer        ,intent(in   )::unit
                  class(time(KK)),intent(in   )::measurement_time


                  write(unit,                   *) measurement_time
                  write(unit,format_observables_K) boson_action(),fermion_action()


        end subroutine print_observables


            function boson_action()


                  implicit none


                  real(KK)::boson_action

                  integer::mu,nu


                  boson_action=+.00000e+0

                  do mu=1,inner_degrees_of_freedom,+1

                     do nu=1,inner_degrees_of_freedom,+1

                        boson_action&
                       =boson_action+norm(a(:,:,mu).commutation.a(:,:,nu))

              end    do!nu=1,inner_degrees_of_freedom,+1

              end do!mu=1,inner_degrees_of_freedom,+1

                  boson_action&
                 =boson_action*inner_degrees_of_freedom*00.25


        end function boson_action


            function fermion_action()


                  implicit none


                  complex(KK)::fermion_action


                  fermion_action&
                 =fermion_action+determinant_degree(boson_degrees_of_freedom)*sum(log(m_eigenvalues))


        end function fermion_action


  end module observables


#  endif
