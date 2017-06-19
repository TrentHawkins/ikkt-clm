#     ifndef IKKT_OBSERVABLES_F90
#     define IKKT_OBSERVABLES_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/time.F90"

#     include "ikkt/fields.F90"
#     include "ikkt/complex_langevin.F90"


      module observables


            use::tensor_type

            use::time_type

            use::fields
            use::complex_langevin


            implicit none


            character(*),private,parameter::           format_observables_K=COMPLEXGK
            character(*),private,parameter::text_field_format_observables_K=COMPLEXAK

            character(:),allocatable,public::meas_file_name

            public::print_observables

            private::  boson_action
            private::fermion_action


      contains


            subroutine print_observables(unit,measurement_time)


                  implicit none


                  integer        ,intent(in   )::unit
                  class(time(KK)),intent(in   )::measurement_time


                  call write(unit,  measurement_time               )
                       write(unit,format_observables_K,advance="no") a(:,:,0) !boson_action()

                  if(fermions_included) then

                       write(unit,format_observables_K             ) fermion_action()

                  else

                       write(unit,                   *             )

              end if!fermions_included


        end subroutine print_observables!unit,measurement_time


            function boson_action()


                  implicit none


                  real(KK)::boson_action

                  integer::mu,nu


                  boson_action=+.00000e+0_KK

                  do mu=0,inner_degrees_of_freedom-1,+1

                     do nu=0,inner_degrees_of_freedom-1,+1

                        boson_action&
                       =boson_action+trace((a(:,:,mu).commutation.a(:,:,nu)).o.(a(:,:,mu).commutation.a(:,:,nu)))
            !           boson_action&
            !          =boson_action+trace((a(:,:,mu).o.a(:,:,nu).o.a(:,:,mu).o.a(:,:,nu))&
            !                             -(a(:,:,mu).o.a(:,:,mu).o.a(:,:,nu).o.a(:,:,nu)))

              end    do!nu=0,inner_degrees_of_freedom-1,+1

              end do!mu=0,inner_degrees_of_freedom-1,+1

                  boson_action&
                 =boson_action*inner_degrees_of_freedom*.25000e+0_KK


        end function boson_action


            function fermion_action()


                  implicit none


                  complex(KK)::fermion_action


                  fermion_action&
                 =fermion_action+determinant_degree(boson_degrees_of_freedom)*sum(log(m_eigenvalues0/m_eigenvalues1))


        end function fermion_action


  end module observables


#  endif
