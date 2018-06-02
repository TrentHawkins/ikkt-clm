#     ifndef AUTOCORRELATION_TIME_F90
#     define AUTOCORRELATION_TIME_F90

#     include "../../system/precision.F90"


      module autocorrelation_time


            use,intrinsic::iso_fortran_env,only:stdin => input_unit,&
                                                stdout=>output_unit,&
                                                stderr=> error_unit

            implicit none


      contains


            function autocorrelation(record_size,record_data,step)


                  implicit none


                  integer,intent(in   )::record_size,step

                  real(KK),dimension(0:record_size-1),intent(in  )::record_data

                  real(KK)::autocorrelation


                  if(record_size<=step) stop "time exceeding record size"

                  autocorrelation=sum((record_data(   0:record_size     -1)-sum(record_data(   0:record_size     -1))/record_size) &
                                     *(record_data(step:record_size+step-1)-sum(record_data(step:record_size+step-1))/record_size))&
                                                                                                                     /record_size


        end function autocorrelation!record_size,record_data,step


  end module autocorrelation_time


#  endif
