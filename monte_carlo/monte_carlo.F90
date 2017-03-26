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


            interface make_monte_carlo

               module procedure make_monte_carlo_K

        end interface make_monte_carlo


      contains


            subroutine make_monte_carlo_K(new_name,time_setting,average_step,time_skip,dynamic)


                  implicit none


                  character(*),intent(in   )::new_name

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step
                  real(KK),intent(in   )::time_skip

                  logical,intent(in   )::dynamic


                  name=new_name

                  allocate(time(KK)::s);call s%make_time(time_setting,time_skip)

                  if(dynamic.eq..true.) then

                     allocate(dynamic_time(KK)::t);call t%make_time(time_skip,average_step)

                  else

                     allocate(        time(KK)::t);call t%make_time(time_skip,average_step)

              end if!dynamic.eq..true.


        end subroutine make_monte_carlo_K!name,time_setting,average_step,average_skip,dynamic_flag


  end module monte_carlo


      module monte_carlo_type


         use random_number_generator

         use time_type


            implicit none


            type::monte_carlo(precision)

               integer,kind,public::precision

               character(256)::name

               class(time(precision)),allocatable,private::s
               class(time(precision)),allocatable,private::t

            contains

               procedure,private::initialize_monte_carlo_K;generic::initialize_monte_carlo=>initialize_monte_carlo_K

        end type  monte_carlo


      contains


            subroutine initialize_monte_carlo_K(this,name,time_setting,average_step,time_skip,dynamic)


                  implicit none


                  class(monte_carlo(KK)),intent(inout)::this

                  character(*),intent(in   )::name

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step
                  real(KK),intent(in   )::time_skip

                  logical,intent(in   )::dynamic


                  this%name=name

                  allocate(time(KK)::this%s);call this%s%make_time(time_setting,time_skip)

                  if(dynamic.eq..true.) then

                     allocate(dynamic_time(KK)::this%t);call this%t%make_time(time_skip,average_step)

                  else

                     allocate(        time(KK)::this%t);call this%t%make_time(time_skip,average_step)

              end if!dynamic.eq..true.


        end subroutine initialize_monte_carlo_K!this,name,time_setting,average_step,average_skip,dynamic_flag


            function monte_carlo_constructor_K(name,time_setting,average_step,time_skip,dynamic) result(that)


                  implicit none


                  character(*),intent(in   )::name

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step
                  real(KK),intent(in   )::time_skip

                  logical,intent(in   )::dynamic

                  type(monte_carlo(KK))::that


                  call initialize_monte_carlo(name,time_setting,average_step,time_skip,dynamic)


        end function monte_carlo_constructor_K!name,time_setting,average_step,time_skip,dynamic


  end module monte_carlo_type


#  endif
