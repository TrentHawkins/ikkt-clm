#     ifndef JACK_KNIFE_F90
#     define JACK_KNIFE_F90

#     include "../system/precision.F90"

#     include "../monte_carlo/average.F90"


      module jack_knife


            use::average_type


            implicit none


            interface jack

               module procedure jack_o
               module procedure jack_ox

               module procedure jack_weighted_o
               module procedure jack_weighted_ox

        end interface jack


      contains


            subroutine jack_o(record_size,jack_bins,record_values,average_o,error_o)


                  implicit none


                  integer,intent(in   )::record_size,jack_bins

                  real(KK),dimension(0:record_size-1),intent(in  )::record_values

                  real(KK),intent(  out)::average_o

                  real(KK),intent(  out)::error_o

                  real(KK),dimension(:),allocatable::o

                  integer::i,&
                           j,bin_width


                  allocate(o(0:jack_bins-1));o=+.00000e+0

                  bin_width=record_size/jack_bins

                  if(bin_width<1) stop "bin_width 0 or less"

                  do i=0,record_size-1

                     do j=0,jack_bins-1

                        if(i/bin_width/=j) o(j)&
                                          =o(j)+record_values(i)/(record_size-bin_width)

              end    do!j=0,jack_bins-1

              end do!i=0,record_size-1

                  average_o=sum(o)/jack_bins;error_o=sqrt(sum((o-average_o)&
                                                             *(o-average_o)))

                  deallocate(o)


        end subroutine jack_o!record_size,jack_bins,record_values,average_o,error_o


            subroutine jack_ox(record_size,jack_bins,record_values,average_o,error_o,&
                                                                   average_x,error_x)


                  implicit none


                  integer,intent(in   )::record_size,jack_bins

                  real(KK),dimension(0:record_size-1),intent(in  )::record_values

                  real(KK),intent(  out)::average_o,&
                                          average_x

                  real(KK),intent(  out)::error_o,&
                                          error_x

                  real(KK),dimension(:),allocatable::o,&
                                                     x

                  integer::i,&
                           j,bin_width


                  allocate(o(0:jack_bins-1));o=+.00000e+0
                  allocate(x(0:jack_bins-1));x=+.00000e+0

                  bin_width=record_size/jack_bins

                  if(bin_width<1) stop "bin_width 0 or less"

                  do i=0,record_size-1

                     do j=0,jack_bins-1

                        if(i/bin_width/=j) then

                           o(j)&
                          =o(j)+record_values(i)/(record_size-bin_width)

              end       if!i/bin_width/=j

              end    do!j=0,jack_bins-1

              end do!i=0,record_size-1

                  do i=0,record_size-1

                     do j=0,jack_bins-1

                        if(i/bin_width/=j) x(j)&
                                          =x(j)+(record_values(i)-o(j))&
                                               *(record_values(i)-o(j))/(record_size-bin_width)

              end    do!j=0,jack_bins-1

              end do!i=0,record_size-1

                  average_o=sum(o)/jack_bins;error_o=sqrt(sum((o-average_o)&
                                                             *(o-average_o)))
                  average_x=sum(x)/jack_bins;error_x=sqrt(sum((x-average_x)&
                                                             *(x-average_x)))

                  deallocate(o)
                  deallocate(x)


        end subroutine jack_ox!record_size,jack_bins,record_values,average_o,error_o,&
!                                                                  average_x,error_x


            subroutine jack_weighted_o(record_size,jack_bins,record_values ,&
                                                             record_weights,average_o,error_o)


                  implicit none


                  integer,intent(in   )::record_size,jack_bins

                  real(KK),dimension(0:record_size-1),intent(in  )::record_values ,&
                                                                    record_weights

                  real(KK),intent(  out)::average_o,error_o

                  type(average(KK))::average_

                  type(average(KK)),dimension(:),allocatable::o

                  integer::i,&
                           j,bin_width


                    allocate(o(0:jack_bins-1));o=average()

                  bin_width=record_size/jack_bins

                  if(bin_width<1) stop "bin_width 0 or less"

                  do i=0,record_size-1

                     do j=0,jack_bins-1

                        if(i/bin_width/=j) then

                           o(j)&
                          =o(j)+average(record_weights(i),record_values(i))

              end       if!i/bin_width/=j

              end    do!j=0,jack_bins-1

              end do!i=0,record_size-1

                  average_=sum(o);average_o=average_%value;error_o=sqrt(sum((o%value-average_o)&
                                                                           *(o%value-average_o)))

                  deallocate(o)


        end subroutine jack_weighted_o!record_size,jack_bins,record_values ,&
!                                                            record_weights,average_o,error_o


            subroutine jack_weighted_ox(record_size,jack_bins,record_values ,&
                                                              record_weights,average_o,error_o,&
                                                                             average_x,error_x)


                  implicit none


                  integer,intent(in   )::record_size,jack_bins

                  real(KK),dimension(0:record_size-1),intent(in  )::record_values ,&
                                                                    record_weights

                  real(KK),intent(  out)::average_o,error_o,&
                                          average_x,error_x

                  type(average(KK))::average_

                  type(average(KK)),dimension(:),allocatable::o,&
                                                              x

                  integer::i,&
                           j,bin_width



                    allocate(o(0:jack_bins-1));o=average()
                    allocate(x(0:jack_bins-1));x=average()

                  bin_width=record_size/jack_bins

                  if(bin_width<1) stop "bin_width 0 or less"

                  do i=0,record_size-1

                     do j=0,jack_bins-1

                        if(i/bin_width/=j) then

                           o(j)&
                          =o(j)+average(record_weights(i),record_values(i))

              end       if!i/bin_width/=j

              end    do!j=0,jack_bins-1

              end do!i=0,record_size-1

                  do i=0,record_size-1

                     do j=0,jack_bins-1

                        if(i/bin_width/=j) then

                           x(j)&
                          =x(j)+average(record_weights(i),(record_values(i)-o(j)%value)&
                                                         *(record_values(i)-o(j)%value))

              end       if!i/bin_width/=j

              end    do!j=0,jack_bins-1

              end do!i=0,record_size-1

                  average_=sum(o);average_o=average_%value;error_o=sqrt(sum((o%value-average_o)&
                                                                           *(o%value-average_o)))
                  average_=sum(x);average_x=average_%value;error_x=sqrt(sum((x%value-average_x)&
                                                                           *(x%value-average_x)))

                  deallocate(o)
                  deallocate(x)


        end subroutine jack_weighted_ox!record_size,jack_bins,record_values ,&
!                                                             record_weights,average_o,error_o,&
!                                                                            average_x,error_x


  end module jack_knife


#  endif
