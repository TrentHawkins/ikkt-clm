#     ifndef IKKT_INTERFACE_F90
#     define IKKT_INTERFACE_F90

#     include "precision.F90"


      module interface


            implicit none


            character(252),parameter::name="IKKT-CLM"

            real(KK)::time_setting= .00000e+0
            real(KK)::average_step= .10000e-4

            integer,parameter::inner_degrees_of_freedom=4
            integer,parameter::boson_degrees_of_freedom=4
            integer,parameter::fermi_degrees_of_freedom&
                          =2**(boson_degrees_of_freedom/2-1)



  end module interface


#  endif
