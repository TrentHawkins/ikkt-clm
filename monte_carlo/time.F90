#     ifndef MONTE_CARLO_TIME_F90
#     define MONTE_CARLO_TIME_F90

#     include "precision.F90"

#     include "monte_carlo/average.F90"


      module time_type


            use average_type


            implicit none


            character(*),private,parameter::           format_time_K=REALGK
            character(*),private,parameter::text_field_format_time_K=REALAK

            character(*),parameter::           format_step=INTEGERG2
            character(*),parameter::text_field_format_step=INTEGERG2

            type::time(precision)

               integer,kind::precision

               real(precision),private::current_time=00.00
               real(precision),private::closing_time
               real(precision),private::average_step

            contains

               procedure,public::make_time_K;generic::make_time=>make_time_K
               procedure,public::load_time_K;generic::load_time=>load_time_K
               procedure,public::save_time_K;generic::save_time=>save_time_K
               procedure,public::time_left_K;generic::time_left=>time_left_K
               procedure,public::time_step_K;generic::time_step=>time_step_K
               procedure,public::push_time_K;generic::push_time=>push_time_K

               procedure,private:: read_time_K;generic:: read(formatted)=> read_time_K
               procedure,private::write_time_K;generic::write(formatted)=>write_time_K

        end type  time

            type,extends(time)::dynamic_time

               real(precision),private::current_step

               type(average(precision)),private::log_average_control
               type(average(precision)),private::log_current_control

            contains

               procedure,public::        make_time_K                           =>dynamic_make_time_K
                                                     generic::dynamic_make_time=>        make_time_K
               procedure,public::        time_step_K                           =>dynamic_time_step_K
               procedure,public::dynamic_push_time_K;generic::        push_time=>dynamic_push_time_K

               procedure,private:: read_time_K=> read_dynamic_time_K
               procedure,private::write_time_K=>write_dynamic_time_K

        end type                dynamic_time


            interface         time

               module procedure         time_constructor_K

        end interface         time

            interface dynamic_time

               module procedure dynamic_time_constructor_K

        end interface dynamic_time

            interface assignment(=)

               module procedure time_assignment_K

        end interface assignment(=)


      contains


            subroutine make_time_K(this,time_setting,average_step)


                  implicit none


                  class(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step


                  this%current_time=00.00
                  this%closing_time=time_setting&
                 +this%current_time
                  this%average_step=average_step


        end subroutine make_time_K!this,closing_time,average_step


            function time_constructor_K(time_setting,average_step) result(that)


                  implicit none


                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step

                  type(time(KK))::that


                  call that%make_time(time_setting,average_step)


        end function time_constructor_K!closing_time,average_step


            subroutine dynamic_make_time_K(this,time_setting,average_step)


                  implicit none


                  class(dynamic_time(KK)),intent(inout)::this

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step


                  call this%time%make_time(time_setting,average_step)

                  this%current_step&
                 =this%average_step

                  this%log_average_control=average(this%current_step)
                  this%log_current_control&
                 =this%log_average_control


        end subroutine dynamic_make_time_K!this,closing_time,average_step


            function dynamic_time_constructor_K(time_setting,average_step) result(that)


                  implicit none


                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step

                  type(dynamic_time(KK))::that


                  call that%dynamic_make_time(time_setting,average_step)


        end function dynamic_time_constructor_K!closing_time,average_step


            subroutine time_assignment_K(this,that)


                  implicit none


                  class(time(KK)),intent(inout)::this
                  class(time(KK)),intent(in   )::that


            !     select type(this)

            !     class is(dynamic_time(KK))

            !        select type(that)

            !        class is(dynamic_time(KK))

            !           this%current_step=that%current_step

            !           this%log_average_control=that%log_average_control
            !           this%log_current_control=that%log_current_control

            ! end    select!type(that)

                  this%current_time=that%current_time
            !     this%closing_time=that%closing_time
            !     this%average_step=that%average_step

            ! end select!type(this)


        end subroutine time_assignment_K!this,that


            subroutine load_time_K(this,time_setting,average_step,time_file_name)


                  implicit none


                  class(time(KK)),intent(inout)::this

                  real(KK),intent(in   )::time_setting
                  real(KK),intent(in   )::average_step

                  character(*),intent(in   )::time_file_name

                  integer::unit


                  call this%make_time(time_setting,average_step)

                   open(newunit=unit,file=time_file_name)
                   read(        unit,                  *) this
                  close(        unit                    )



        end subroutine load_time_K!this,time_setting,average_step,time_file_name


            subroutine save_time_K(this,time_file_name)


                  implicit none


                  class(time(KK)),intent(in   )::this

                  character(*),intent(in   )::time_file_name

                  integer::unit


                   open(newunit=unit,file=time_file_name)
                  write(        unit,                  *) this
                  close(        unit                    )


        end subroutine save_time_K!this,time_file_name


            logical function time_left_K(this)


                  implicit none


                  class(time(KK)),intent(in   )::this


                  if(this%current_time>this%closing_time) then

                     time_left_K=.false.

                  else

                     time_left_K=.true.

              end if!this%current_time>this%closing_time


        end         function time_left_K!this


            function time_step_K(this) result(this_average_step)


                  implicit none


                  class(time(KK)),intent(in   )::this

                  real(KK)::this_average_step


                  this_average_step&
                 =this%average_step


        end function time_step_K!this


            function dynamic_time_step_K(this) result(this_current_step)


                  implicit none


                  class(dynamic_time(KK)),intent(in   )::this

                  real(KK)::this_current_step


                  this_current_step&
                 =this%current_step


        end function dynamic_time_step_K!this


            subroutine push_time_K(this)


                  implicit none


                  class(time(KK)),intent(inout)::this


                  this%current_time&
                 =this%current_time+this%time_step()


        end subroutine push_time_K!this


            subroutine dynamic_push_time_K(this,current_control)


                  implicit none


                  class(dynamic_time(KK)),intent(inout)::this

                  real(KK),intent(in   )::current_control


                  this%current_step&
                 =this%average_step*(exp(value(this%log_average_control))&
                                                       /current_control)

                  this%log_current_control=average(this%current_step,&
                       log(current_control))

                  this%log_average_control&
                 =this%log_average_control&
                 +this%log_current_control

                  this%current_time=weight(this%log_average_control)


        end subroutine dynamic_push_time_K!this,current_drift


            subroutine  read_time_K(dtv,unit,iotype,v_list,iostat,iomsg)


                  implicit none


                  class(time(KK)),intent(inout)::dtv
                  integer        ,intent(in   )::unit
                  character(   *),intent(in   )::iotype
                  integer        ,intent(in   )::v_list(:)
                  integer        ,intent(  out)::iostat
                  character(   *),intent(inout)::iomsg


                  if(iotype=='listdirected') then

                      read(unit=unit,fmt=format_time_K,iostat=iostat,iomsg=iomsg) dtv%current_time

                  else

                     iostat=1
                     iomsg='read(time): specific input format or namelist not supported'

              end if!iotype=='listdirected'


        end subroutine  read_time_K!dtv,unit,iotype,v_list,iostat,iomsg


            subroutine write_time_K(dtv,unit,iotype,v_list,iostat,iomsg)


                  implicit none


                  class(time(KK)),intent(in   )::dtv
                  integer        ,intent(in   )::unit
                  character(   *),intent(in   )::iotype
                  integer        ,intent(in   )::v_list(:)
                  integer        ,intent(  out)::iostat
                  character(   *),intent(inout)::iomsg


                  if(iotype=='listdirected') then

                     write(unit=unit,fmt=format_time_K,iostat=iostat,iomsg=iomsg,advance='no')         dtv%average_step
                     write(unit=unit,fmt=            *                                       ) average(dtv%current_time)

                  else

                     iostat=1
                     iomsg='write(time): specific input format or namelist not supported'

              end if!iotype=='listdirected'


        end subroutine write_time_K!dtv,unit,iotype,v_list,iostat,iomsg


            subroutine  read_dynamic_time_K(dtv,unit,iotype,v_list,iostat,iomsg)


                  implicit none


                  class(dynamic_time(KK)),intent(inout)::dtv
                  integer                ,intent(in   )::unit
                  character(           *),intent(in   )::iotype
                  integer                ,intent(in   )::v_list(:)
                  integer                ,intent(  out)::iostat
                  character(           *),intent(inout)::iomsg


                  if(iotype=='listdirected') then

                      read(unit=unit,fmt=format_time_K,iostat=iostat,iomsg=iomsg,advance='no') dtv%current_step
                      read(unit=unit,fmt=            *                                       ) dtv%log_average_control

                  else

                     iostat=1
                     iomsg='read(time): specific input format or namelist not supported'

              end if!iotype=='listdirected'


        end subroutine  read_dynamic_time_K!dtv,unit,iotype,v_list,iostat,iomsg


            subroutine write_dynamic_time_K(dtv,unit,iotype,v_list,iostat,iomsg)


                  implicit none


                  class(dynamic_time(KK)),intent(in   )::dtv
                  integer                ,intent(in   )::unit
                  character(           *),intent(in   )::iotype
                  integer                ,intent(in   )::v_list(:)
                  integer                ,intent(  out)::iostat
                  character(           *),intent(inout)::iomsg


                  if(iotype=='listdirected') then

                     write(unit=unit,fmt=format_time_K,iostat=iostat,iomsg=iomsg,advance='no') dtv%current_step
                     write(unit=unit,fmt=            *                                       ) dtv%log_average_control

                  else

                     iostat=1
                     iomsg='write(time): specific input format or namelist not supported'

              end if!iotype=='listdirected'


        end subroutine write_dynamic_time_K!dtv,unit,iotype,v_list,iostat,iomsg


  end module time_type


#  endif
