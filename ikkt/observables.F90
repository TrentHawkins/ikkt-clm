#     ifndef IKKT_OBSERVABLES_F90
#     define IKKT_OBSERVABLES_F90

#     include "precision.F90"
#     include "interface.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/fields.F90"


      module observables


            use interface

            use tensor_type

            use fields


            implicit none


      contains


            function boson_action()


                  implicit none


                  complex(KK)::boson_action

                  integer::mu,nu


                  boson_action= .00000e+0

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
