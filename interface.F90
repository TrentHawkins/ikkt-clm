#     ifndef IKKT_INTERFACE_F90
#     define IKKT_INTERFACE_F90

#     include "precision.F90"


      module interface


            implicit none


            logical::configuration_loaded=.false.
            logical::timestep_is_variable=.false.
            logical::start_field_is_hot

            character(:),allocatable,public::base_file_name
            character(:),allocatable,public::seed_file_name
            character(:),allocatable,public::time_file_name
            character(:),allocatable,public::conf_file_name
            character(:),allocatable,public::meas_file_name

            real(KK),public::time_setting=+.00000e+0
            real(KK),public::average_step=+.10000e-4
            real(KK),public::ac_time_skip=+.10000e-4

            integer,public,parameter::inner_degrees_of_freedom=4
            integer,public,parameter::boson_degrees_of_freedom=4
            integer,public,parameter::fermi_degrees_of_freedom=2**(boson_degrees_of_freedom/2-1)


  end module interface


#  endif
