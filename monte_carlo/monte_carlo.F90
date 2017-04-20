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


            subroutine prepare_time_K(is_dynamic)


                  implicit none


                  logical,intent(in   )::is_dynamic


                  if(allocated(s)) then

                     deallocate(s)

              end if!allocated(s)

                  allocate(time(KK)::s)

                  write(*,*) s!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                  if(allocated(t)) then

                     deallocate(t)

              end if!allocated(t)

                  if(is_dynamic==.true.) then

                     allocate(dynamic_time(KK)::t)

                  else

                     allocate(        time(KK)::t)

              end if!is_dynamic==.true.


        end subroutine prepare_time_K!is_dynamic


            subroutine make_monte_carlo_K(time_setting,average_step,ac_time_skip,is_dynamic)


                  implicit none


                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step
                  real(KK),intent(in   )::ac_time_skip

                  logical,intent(in   )::is_dynamic


                  call prepare_time(is_dynamic)

                  call   make_seed(                         )
                  call s%make_time(time_setting,ac_time_skip)
                  call t%make_time(ac_time_skip,average_step)


        end subroutine make_monte_carlo_K!time_setting,average_step,ac_time_skip,is_dynamic


            subroutine load_monte_carlo_K(time_setting,average_step,ac_time_skip,is_dynamic,seed_file_name,time_file_name)


                  implicit none


                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step
                  real(KK),intent(in   )::ac_time_skip

                  logical,intent(in   )::is_dynamic

                  character(*),intent(in   )::seed_file_name
                  character(*),intent(in   )::time_file_name


                  call prepare_time(is_dynamic)

                  call   load_seed(                          trim(adjustl(seed_file_name)))
                  call s%load_time(time_setting,ac_time_skip,trim(adjustl(time_file_name)))
                  call t%load_time(ac_time_skip,average_step,trim(adjustl(time_file_name)))


        end subroutine load_monte_carlo_K!time_setting,average_step,ac_time_skip,is_dynamic,seed_file_name,time_file_name


            subroutine save_monte_carlo_K(seed_file_name,time_file_name)


                  implicit none


                  character(*),intent(in   )::seed_file_name
                  character(*),intent(in   )::time_file_name


                  call   save_seed(trim(adjustl(seed_file_name)))
                  call s%save_time(trim(adjustl(time_file_name)))
                  call t%save_time(trim(adjustl(time_file_name)))


        end subroutine save_monte_carlo_K!seed_file_name,time_file_name


  end module monte_carlo


#  endif
