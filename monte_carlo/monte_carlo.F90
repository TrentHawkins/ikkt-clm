#     ifndef MONTE_CARLO_MONTE_CARLO_F90
#     define MONTE_CARLO_MONTE_CARLO_F90

#     include "monte_carlo/random_number_generator.F90"
#     include "monte_carlo/average.F90"
#     include "monte_carlo/time.F90"


      module monte_carlo


            use::random_number_generator
            use::average_type
            use::time_type


            implicit none


            real(KK),public::time_setting=+.00000e+0_KK
            real(KK),public::average_step=+.10000e-4
            real(KK),public::ac_time_skip=+.10000e-4

            class(time(KK)),allocatable,public::s
            class(time(KK)),allocatable,public::t

            private::prepare_time_K

            public::make_monte_carlo_K
            public::load_monte_carlo_K
            public::save_monte_carlo_K


            interface prepare_time

               module procedure prepare_time_K

        end interface prepare_time

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


                  call prepare_time()
                  call read_time_parameters()

                  call   make_seed(                         )
                  call s%make_time(time_setting,ac_time_skip)
                  call t%make_time(ac_time_skip,average_step)


        end subroutine make_monte_carlo_K


            subroutine load_monte_carlo_K()


                  implicit none


                  call prepare_time()
                  call read_time_parameters()

                  call   load_seed(                         )
                  call s%load_time(time_setting,ac_time_skip)
                  call t%load_time(ac_time_skip,average_step)


        end subroutine load_monte_carlo_K


            subroutine save_monte_carlo_K()


                  implicit none


                  call   save_seed()
                  call s%save_time()
                  call t%save_time()


        end subroutine save_monte_carlo_K


            subroutine prepare_time_K()


                  implicit none


                  if(allocated(s)) then

                     deallocate(s)

              end if!allocated(s)

                  allocate(time(KK)::s)

                  if(allocated(t)) then

                     deallocate(t)

              end if!allocated(t)

                  if(timestep_is_variable) then

                     allocate(dynamic_time(KK)::t)

                  else

                     allocate(        time(KK)::t)

              end if!timestep_is_variable


        end subroutine prepare_time_K


            subroutine read_time_parameters()


                  implicit none


                  write(*,"(a)",advance="no") "time_setting: "
                   read(*,  *               )  time_setting
                  write(*,  *               )
                  write(*,"(a)",advance="no") "average_step: "
                   read(*,  *               )  average_step
                  write(*,  *               )
                  write(*,"(a)",advance="no") "ac_time_skip: "
                   read(*,  *               )  ac_time_skip
                  write(*,  *               )


        end subroutine read_time_parameters


  end module monte_carlo


#  endif
