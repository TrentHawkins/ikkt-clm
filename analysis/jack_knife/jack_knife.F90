#     ifndef JACK_KNIFE_F90
#     define JACK_KNIFE_F90

#     include "../../system/precision.F90"

#     include "../../tools/constants.F90"


      module jack_knife


            use,intrinsic::iso_fortran_env,only:stdin => input_unit,&
                                                stdout=>output_unit,&
                                                stderr=> error_unit

            use::mathematical_constants


            implicit none


      contains


            subroutine jack(jack_bins,record_size,record_data,average,error)


                  implicit none


                  integer,intent(in   )::jack_bins

                  integer ,                           intent(in  )::record_size
                  real(KK),dimension(0:record_size-1),intent(in  )::record_data

                  real(KK),intent(  out)::average
                  real(KK),intent(  out)::error

                  real(KK),dimension(:),allocatable::o

                  integer::bin_width
                  integer::i
                  integer::j


                  allocate(o(0:jack_bins-1)); o=nil

                  bin_width=record_size/jack_bins

                  do i=0,record_size-1

                     do j=0,jack_bins-1

                        if(i/bin_width/=j) o(j)&
                                          =o(j)+record_data(i)/(record_size-bin_width)

              end    do!j=0,jack_bins-1

              end do!i=0,record_size-1

                  average=sum(o)/jack_bins; error=sqrt(sum((o-average)&
                                                          *(o-average)))

                  deallocate(o)


        end subroutine jack!jack_bins,record_size,record_data,average,error


  end module jack_knife


#  endif
