#     ifndef MONTE_CARLO_F90
#     define MONTE_CARLO_F90

#     include "../system/precision.F90"
#     include "../system/text_format.F90"

#     include "../monte_carlo/random_number_generator.F90"
#     include "../monte_carlo/average.F90"
#     include "../monte_carlo/time.F90"


      module monte_carlo


            use::text_formatting

            use::random_number_generator
            use::average_type
            use::time_type


            implicit none


            logical::measure_time_skipped=.false.

            real(KK),public::time_setting=+.00000e+0_KK
            real(KK),public::measure_skip=+.00000e+0_KK
            real(KK),public::average_step=+.00000e+0_KK

            type(time(KK)),public::s
            type(time(KK)),public::t

            public::read_time_parameters

            public::make_monte_carlo_K
            public::load_monte_carlo_K
            public::save_monte_carlo_K


            interface make_monte_carlo

               module procedure make_monte_carlo_K

        end interface make_monte_carlo

            interface load_monte_carlo

               module procedure load_monte_carlo_K

        end interface load_monte_carlo

            interface save_monte_carlo

               module procedure save_monte_carlo_K

        end interface save_monte_carlo


      contains


            subroutine make_monte_carlo_K()


                  implicit none


                  call read_time_parameters()

                  call make_seed()

                  if(measure_time_skipped) then

                     call s%set(time_setting,measure_skip)
                     call t%set(measure_skip,average_step)

                  else

                     call t%set(time_setting,average_step)

              end if!measure_time_skipped


        end subroutine make_monte_carlo_K!


            subroutine load_monte_carlo_K()


                  implicit none


                  integer::unit


                  call read_time_parameters()

                  call load_seed()

                   open(newunit=unit,file=time_file_name)

                  if(measure_time_skipped) then

                     call s%read(unit,time_setting,measure_skip)
                     call t%read(unit,measure_skip,average_step)

                  else

                     call t%read(unit,time_setting,average_step)

              end if!measure_time_skipped

                  close(        unit                    )


        end subroutine load_monte_carlo_K!


            subroutine save_monte_carlo_K()


                  implicit none


                  integer::unit


                  call save_seed()

                   open(newunit=unit,file=time_file_name)

                  if(measure_time_skipped) then

                     call s%write(unit)

              end if!measure_time_skipped

                  call t%write(unit)

                  close(        unit                    )

        end subroutine save_monte_carlo_K!


            subroutine read_time_parameters()


                  implicit none


!                 character(*)::temp_file_name


                  write(*,"(2a)",advance="no") "time_setting: ",t_yellow
                   read(*,   *               )  time_setting
                  write(*,"(2a)",advance="no")                  t_normal

                  write(*,"(2a)",advance="no") "measure_skip: ",t_yellow
                   read(*,   *               )  measure_skip
                  write(*,"(2a)",advance="no")                  t_normal

                  write(*,"(2a)",advance="no") "average_step: ",t_yellow
                   read(*,   *               )  average_step
                  write(*,"(2a)",advance="no")                  t_normal

                  write(*,   *               )

                  if(measure_skip/= .00000e+0) measure_time_skipped=.true.

!                 write(temp_file_name,"(a2,sp,i2.1)")  ":t",nint(log10(time_setting))

!                 time_file_name&
!                =time_file_name//trim(temp_file_name)

!                 if(measure_time_skipped) then

!                    write(temp_file_name,"(a3,sp,i2.1)") ":Dt",nint(log10(measure_skip))

!                    time_file_name&
!                   =time_file_name//trim(temp_file_name)

!             end if!measure_time_skipped

!                 write(temp_file_name,"(a3,sp,i2.1)") ":dt",nint(log10(average_step))

!                 time_file_name&
!                =time_file_name//trim(temp_file_name)


        end subroutine read_time_parameters!


  end module monte_carlo


#  endif
