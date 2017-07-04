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

            type(time(KK)),public::t

            private::read_time_parameters

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

                  call   make_seed(                         )
                  call t%make_time(time_setting,average_step)


        end subroutine make_monte_carlo_K!


            subroutine load_monte_carlo_K()


                  implicit none


                  call read_time_parameters()

                  call   load_seed(                         )
                  call t%load_time(time_setting,average_step)


        end subroutine load_monte_carlo_K!


            subroutine save_monte_carlo_K()


                  implicit none


                  call   save_seed(                         )
                  call t%save_time(                         )


        end subroutine save_monte_carlo_K!


            subroutine read_time_parameters()


                  implicit none


                  write(*,"(a)",advance="no") "time_setting: "
                   read(*,  *               )  time_setting
                  write(*,  *               )
                  write(*,"(a)",advance="no") "average_step: "
                   read(*,  *               )  average_step
                  write(*,  *               )


        end subroutine read_time_parameters!


  end module monte_carlo


#  endif
