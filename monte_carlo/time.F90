#     ifndef TIME_F90
#     define TIME_F90

#     include "../system/precision.F90"

#     include "../monte_carlo/average.F90"


      module time_type


            use::average_type


            implicit none


            character(*),private,parameter::           format_time_K=REALGK
            character(*),private,parameter::text_field_format_time_K=REALAK

            real(KK),parameter::time_tolerance_K=TOLERANCEK

            character(:),allocatable,public::time_file_name

            logical                 ,public::timestep_is_variable=.false.

            type::time(precision)

               integer,kind::precision

               real(precision),private::closing_time
               real(precision),private::time_setting
               real(precision),private::average_step

               type(average(precision)),private::log_average_control
               type(average(precision)),private::log_current_control

            contains

               procedure,public ::   set_K;generic::   set=>   set_K
               procedure,public ::  read_K;generic::  read=>  read_K
               procedure,public :: write_K;generic:: write=> write_K
               procedure,private::rewind_K;generic::rewind=>rewind_K

               procedure,public ::current_time_K;generic::current_time=>current_time_K
               procedure,public ::current_step_K;generic::current_step=>current_step_K

               procedure,public ::        time_left_K;generic::time_left=>        time_left_K
               procedure,private::        zero_time_K;generic::zero_time=>        zero_time_K
               procedure,public :: static_push_time_K;generic::push_time=> static_push_time_K
               procedure,public ::dynamic_push_time_K;generic::push_time=>dynamic_push_time_K

        end type  time


            interface  read

               module procedure  read_time_K

        end interface  read

            interface write

               module procedure write_time_K

        end interface write


      contains


            subroutine    set_K(this,time_setting,average_step)


                  implicit none


                  type(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step


                  this%log_average_control=average(            )
                  this%log_current_control=average(average_step)

                  call this%rewind(time_setting,average_step)


        end subroutine    set_K!this,time_setting,average_step


            subroutine   read_K(this,unit,time_setting,average_step)


                  implicit none


                  type(time(KK)),intent(inout)::this

                  integer ,intent(in   )::unit
                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step


                  call  read(unit,this)
                        read(unit,   *)

                  call this%rewind(time_setting,average_step)


        end subroutine   read_K!unit,this


            subroutine  write_K(this,unit)


                  implicit none


                  type(time(KK)),intent(in   )::this

                  integer ,intent(in   )::unit


                  call write(unit,this)
                       write(unit,   *)


        end subroutine  write_K!unit,this


            subroutine rewind_K(this,time_setting,average_step)


                  type(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step


                  this%time_setting&
                      =time_setting

                  this%average_step&
                      =average_step

                  this%closing_time&
                 =this%time_setting+nint(this%log_average_control%weight &
                                        /this%log_current_control%weight)&
                                        *this%log_current_control%weight


        end subroutine rewind_K!this,time_setting,average_step


            function current_time_K(this) result(current_time)


                  implicit none


                  type(time(KK)),intent(in   )::this

                  real(KK)::current_time


                  current_time=this%log_average_control%weight


        end function current_time_K!this


            function current_step_K(this) result(current_step)


                  implicit none


                  type(time(KK)),intent(in   )::this

                  real(KK)::current_step


                  current_step=this%log_current_control%weight


        end function current_step_K!this


            logical function time_left_K(this)


                  implicit none


                  type(time(KK)),intent(inout)::this


                  if(this%current_time() &
                    <this%closing_time ) then

                     if(this%current_time(                ) &
                       <this%closing_time-time_tolerance_K) then

                        time_left_K=.true.

                     else

                        time_left_K=.false.

              end    if!this%current_time(                ) &
!                      <this%closing_time-time_tolerance_K

                  else

                     time_left_K=.false.

              end if!this%current_time() &
!                   <this%closing_time

                  if(.not.time_left_K) then

                     call this%zero_time()

              end if!.not.time_left_K


        end         function time_left_K!this


            subroutine zero_time_K(this)


                  implicit none


                  type(time(KK)),intent(inout)::this


                  this%closing_time&
                 =this%closing_time&
                 +this%time_setting


        end subroutine zero_time_K!this


            subroutine static_push_time_K(this)


                  implicit none


                  type(time(KK)),intent(inout)::this


                  this%log_average_control&
                 =this%log_average_control+average(this%current_step())


        end subroutine static_push_time_K!this


            subroutine dynamic_push_time_K(this,current_control)


                  implicit none


                  type(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::current_control
                  real(KK)              ::current_step


                                                   current_step=this%average_step*(exp(this%log_average_control%value)&
                                                                                 /              current_control      )
                  this%log_current_control=average(current_step,&
                       log(current_control))

                  this%log_average_control&
                 =this%log_average_control&
                 +this%log_current_control


        end subroutine dynamic_push_time_K!this,current_control


            subroutine  read_time_K(unit,this)


                  implicit none


                  integer       ,intent(in   )::unit
                  type(time(KK)),intent(inout)::this


                  call  read(unit,this%log_average_control)
                  call  read(unit,this%log_current_control)


        end subroutine  read_time_K!unit,this


            subroutine write_time_K(unit,this)


                  implicit none


                  integer       ,intent(in   )::unit
                  type(time(KK)),intent(in   )::this


                  call write(unit,this%log_average_control)
                  call write(unit,this%log_current_control)


        end subroutine write_time_K!unit,this


  end module time_type


#  endif
