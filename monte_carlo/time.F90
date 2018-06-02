#     ifndef MONTE_CARLO_TIME_F90
#     define MONTE_CARLO_TIME_F90

#     include "../system/precision.F90"

#     include "average.F90"


      module time_type


            use::average_type


            implicit none


            character(*),private,parameter::           format_time_K=REALGK
            character(*),private,parameter::text_field_format_time_K=REALAK

            character(:),allocatable,public::time_file_name

            logical,public::auto_save_field_conf=.false.
            logical,public::measure_time_skipped=.false.
            logical,public::timestep_is_adaptive=.false.

!           type::time(precision)

!              integer,kind::precision

!              type(average(precision)),private::m_current_time
!              type(average(precision)),private::m_current_step

!              real(precision),private::m_life_time
!              real(precision),private::m_time_step

!           contains

!              procedure,public::time_K;generic::time=>time_K
!              procedure,public::step_K;generic::step=>step_K

!              procedure,public::make_time_K;generic::make_time=>make_time_K
!              procedure,public::load_time_K;generic::load_time=>load_time_K
!              procedure,public::save_time_K;generic::save_time=>save_time_K
!              procedure,public::time_left_K;generic::time_left=>time_left_K
!              procedure,public::push_time_K;generic::push_time=>push_time_K

!              procedure,public::agnostically_adapt_time_step_K;generic::adapt_time_step=>agnostically_adapt_time_step_K

!       end type  time


!           interface  read

!              module procedure  read_time_K

!       end interface  read

!           interface write

!              module procedure write_time_K

!       end interface write


!     contains


!           function time_K(this) result(time)


!                 implicit none


!                 type(time(KK))::this

!                 real(KK)::time


!                 time=this%m_current_time%weight()


!       end function time_K!this


!           function step_K(this) result(step)


!                 implicit none


!                 type(time(KK))::this

!                 real(KK)::step


!                 step=this%m_current_step%weight()


!       end function step_K!this


!           subroutine make_time_K(this,life_time,time_step)


!                 implicit none


!                 type(time(KK))::this

!                 real(KK)::life_time
!                 real(KK)::time_step


!                 this%m_current_time=average(         )
!                 this%m_current_step=average(time_step)

!                 this%m_life_time=life_time+this%m_current_time%weight()
!                 this%m_time_step=time_step


!       end subroutine make_time_K!this,life_time,time_step


!           subroutine load_time_K(this,life_time,time_step)


!                 implicit none


!                 type(time(KK))::this

!                 real(KK)::life_time
!                 real(KK)::time_step


!                  open(unit=time_unit,file=time_file_name)

!                 call  read(time_unit,this%m_current_time)
!                 call  read(time_unit,this%m_current_step)

!                 close(     time_unit                    )

!                 this%m_life_time=life_time+this%m_current_time%weight()
!                 this%m_time_step=time_step


!       end subroutine load_time_K!this,life_time,time_step


!           subroutine save_time_K(this)


!                 implicit none


!                 type(time(KK))::this


!                  open(unit=time_unit,file=time_file_name)

!                 call write(unit,this%m_current_time)
!                 call write(unit,this%m_current_step)

!                 close(     time_unit                    )


!       end subroutine save_time_K!this


!           function time_left_K(this) result(time_left)


!                 implicit none


!                 type(time(KK))::this

!                 logical::time_left


!                 if(this%m_current_time<=this%m_life_time) then

!                    if(this%m_current_step>this%m_life_time-this%m_current_time%weight()) &
!                       this%m_current_step=this%m_life_time-this%m_current_time%weight()

!                    time_left=.true.

!                 else

!                    this%m_life_time&
!                   =this%m_life_time+this%m_current_time%weight()

!                    time_left=.false.

!             end if!this%m_current_time<=this%m_life_time


!       end         function time_left_K!this


!           subroutine push_time_K(this)


!                 implicit none


!                 type(time(KK))::this


!                 this%m_current_time&
!                =this%m_current_time&
!                +this%m_current_step


!       end subroutine push_time_K!this


!           subroutine agnostically_adapt_time_step_K(this,exp_current_step_value)


!                 implicit none


!                 type(time(KK))::this

!                 real(KK),intent(inout)::exp_current_step_value

!                 real(KK)::current_step_value


!                 call this%m_current_step%set_average(this%m_time_step*exp(this%m_current_time%value())/exp_current_step_value,&
!                                                                                                    log(exp_current_step_value))


!       end subroutine agnostically_adapt_time_step_K!this,exp_current_step_value


!           subroutine  read_time_K(unit,this)


!                 implicit none


!                 integer       ,intent(inout)::unit
!                 type(time(KK)),intent(inout)::this


!                 call  read(unit,this%m_current_time)
!                 call  read(unit,this%m_current_step)


!       end subroutine  read_time_K!unit,this


!           subroutine write_time_K(unit,this)


!                 implicit none


!                 integer       ,intent(inout)::unit
!                 type(time(KK)),intent(in   )::this


!                 call write(unit,this%m_current_time)
!                 call write(unit,this%m_current_step)


!       end subroutine write_time_K!unit,this


  end module time_type


#  endif
