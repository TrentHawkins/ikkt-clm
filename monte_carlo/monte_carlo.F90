#     ifndef MONTE_CARLO_MONTE_CARLO_F90
#     define MONTE_CARLO_MONTE_CARLO_F90

#     include "monte_carlo/random_number_generator.F90"
#     include "monte_carlo/average.F90"
#     include "monte_carlo/time.F90"


      module monte_carlo


         use random_number_generator
         use average_type
         use time_type


            implicit none


            character(256),public::name


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


            subroutine prepare_time_K(dynamic)


                  implicit none


                  logical,intent(in   )::dynamic


                  if(allocated(s)) then

                     deallocate(s)

              end if!allocated(s)

                  allocate(time(KK)::s)

                  if(allocated(t)) then

                     deallocate(t)

              end if!allocated(t)

                  if(dynamic==.true.) then

                     allocate(dynamic_time(KK)::t)

                  else

                     allocate(        time(KK)::t)

              end if!dynamic==.true.


        end subroutine prepare_time_K!dynamic


            subroutine make_monte_carlo_K(new_name,time_setting,average_step,time_skip,dynamic)


                  implicit none


                  character(*),intent(in   )::new_name

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step
                  real(KK),intent(in   )::time_skip

                  logical,intent(in   )::dynamic


                  name=new_name

                  call prepare_time(dynamic)

                  call   make_seed()

                  call s%make_time(time_setting,time_skip)
                  call t%make_time(time_skip,average_step)


        end subroutine make_monte_carlo_K!new_name,time_setting,average_step,time_skip,dynamic


            subroutine load_monte_carlo_K(file_name)


                  implicit none


                  character(*),intent(in   )::file_name


                  call   load_seed(file_name)

                  call s%load_time(file_name)
                  call t%load_time(file_name)


        end subroutine load_monte_carlo_K!file_name


            subroutine save_monte_carlo_K(file_name)


                  implicit none


                  character(*),intent(in   )::file_name


                  call   save_seed(file_name)

                  call s%save_time(file_name)
                  call t%save_time(file_name)


        end subroutine save_monte_carlo_K!file_name


  end module monte_carlo


#  endif
