#     ifndef TIME_F90
#     define TIME_F90

#     include "system/precision.F90"

#     include "monte_carlo/average.F90"


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

               real(precision),private::current_time=+.00000e+0_KK
               real(precision),private::time_setting
               real(precision),private::closing_time
               real(precision),private::average_step
               real(precision),private::current_step

               type(average(precision)),private::log_average_control
               type(average(precision)),private::log_current_control

            contains

               procedure,public::make_time_K;generic::make_time=>make_time_K
               procedure,public::load_time_K;generic::load_time=>load_time_K
               procedure,public::save_time_K;generic::save_time=>save_time_K
               procedure,public::zero_time_K;generic::zero_time=>zero_time_K
               procedure,public::time_left_K;generic::time_left=>time_left_K
               procedure,public::time_step_K;generic::time_step=>time_step_K

               procedure,public::        push_time_K;generic::push_time=>        push_time_K
               procedure,public::dynamic_push_time_K;generic::push_time=>dynamic_push_time_K

        end type  time


            interface  read

               module procedure  read_time_K

        end interface  read

            interface write

               module procedure write_time_K

        end interface write


      contains


            subroutine make_time_K(this,time_setting,average_step)


                  implicit none


                  type(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step


                  this%current_time=+.00000e+0_KK

                  this%time_setting&
                      =time_setting
                  this%closing_time&
                 =this%current_time&
                 +this%time_setting

                  this%average_step&
                      =average_step
                  this%current_step&
                 =this%average_step

                  this%log_average_control=average(this%current_time)
                  this%log_current_control=average(this%current_step)


        end subroutine make_time_K!this,closing_time,average_step


            subroutine load_time_K(this,time_setting,average_step)


                  implicit none


                  type(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step

                  integer::unit


                  call this%make_time(time_setting,average_step)

                        open(newunit=unit,file=time_file_name)
                  call  read(        unit,               this)
                        read(        unit,                  *)
                       close(        unit                    )



        end subroutine load_time_K!this,time_setting,average_step


            subroutine save_time_K(this)


                  implicit none


                  type(time(KK)),intent(inout)::this

                  integer::unit


                        open(newunit=unit,file=time_file_name)
                  call write(        unit,               this)
                       write(        unit,                  *)
                       close(        unit                    )


        end subroutine save_time_K!this


            subroutine zero_time_K(this)


                  implicit none


                  type(time(KK)),intent(inout)::this


                  this%closing_time&
                 =this%closing_time&
                 +this%time_setting


        end subroutine zero_time_K!this


            logical function time_left_K(this)


                  implicit none


                  type(time(KK)),intent(inout)::this


                  if(this%current_time&
                    <this%closing_time) then

                     if(this%current_time&
                       <this%closing_time-time_tolerance_K) then

                        time_left_K=.true.

                     else

                        time_left_K=.false.

              end    if!this%current_time&
            !          <this%closing_time-time_tolerance_K

                  else

                     time_left_K=.false.

              end if!this%current_time&
            !       <this%closing_time

                  if(.not.time_left_K) then

                     call this%zero_time()

              end if!.not.time_left_K


        end         function time_left_K!this


            function time_step_K(this) result(this_current_step)


                  implicit none


                  type(time(KK)),intent(in   )::this

                  real(KK)::this_current_step


                  this_current_step&
                 =this%current_step


        end function time_step_K!this


            subroutine push_time_K(this)


                  implicit none


                  type(time(KK)),intent(inout)::this


            !     this%current_time&
            !    =this%current_time+this%time_step()

                  this%log_average_control&
                 =this%log_average_control+average(this%time_step())

                  this%current_time=this%log_average_control%weight


        end subroutine push_time_K!this,current_control


            subroutine dynamic_push_time_K(this,current_control)


                  implicit none


                  type(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::current_control


                  this%current_step&
                 =this%average_step*(exp(this%log_average_control%value)&
                                   /              current_control      )

                  this%log_current_control=average(this%current_step,&
                       log(current_control))

                  this%log_average_control&
                 =this%log_average_control&
                 +this%log_current_control

                  this%current_time=this%log_average_control%weight


        end subroutine dynamic_push_time_K!this,current_control


            subroutine  read_time_K(unit,this)


                  implicit none


                  integer               ,intent(in   )::unit
                  type(time(KK)),intent(inout)::this


                  call  read(unit,                            this%log_average_control)
                        read(unit,format_time_K,advance="no") this%current_step


        end subroutine  read_time_K!unit,this


            subroutine write_time_K(unit,this)


                  implicit none


                  integer               ,intent(in   )::unit
                  type(time(KK)),intent(in   )::this


                  call write(unit,                            this%log_average_control)
                       write(unit,format_time_K,advance="no") this%current_step


        end subroutine write_time_K!unit,this


  end module time_type


#  endif
