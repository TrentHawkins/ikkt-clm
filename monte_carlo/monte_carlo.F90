#     ifndef MONTE_CARLO_MONTE_CARLO_F90
#     define MONTE_CARLO_MONTE_CARLO_F90

#     include "../system/precision.F90"
#     include "../system/text_format.F90"

#     include "../monte_carlo/random_number_generator.F90"
#     include "../monte_carlo/average/average.F90"

#     define   clock average
#     define    time weight
#     define control value


      module monte_carlo


            use::text_formatting

            use::random_number_generator
            use::average_type
            use::time_type


            implicit none


            real(KK),public::life_time=+.00000e+0_KK
            real(KK),public::auto_save=+.00000e+0_KK
            real(KK),public::time_skip=+.00000e+0_KK
            real(KK),public::time_step=+.00000e+0_KK

            type(clock(KK))::auto
            type(clock(KK))::skip
            type(clock(KK))::life
            type(clock(KK))::step

            public::make_monte_carlo_K
            public::load_monte_carlo_K
            public::save_monte_carlo_K

            private:: read_time_parameters
            private::check_time_parameters

            public::agnostically_adapt_time_step

            public::print_time_K


            interface make_monte_carlo

               module procedure make_monte_carlo_K

        end interface make_monte_carlo

            interface load_monte_carlo

               module procedure load_monte_carlo_K

        end interface load_monte_carlo

            interface save_monte_carlo

               module procedure save_monte_carlo_K

        end interface save_monte_carlo

            interface agnostically_adapt_time_step

               module procedure agnostically_adapt_time_step_K

        end interface agnostically_adapt_time_step

            interface print

               module procedure print_time_K

        end interface print


      contains


            subroutine make_monte_carlo_K(start_control)


                  implicit none


                  real(KK),intent_in


                  call read_time_parameters()

                  call make_seed()

                  if(auto_save_field_conf) auto=clock(              )
                  if(measure_time_skipped) skip=clock(              )
                                           step=clock(time=time_step)

                  if(timestep_is_adaptive) then

                     life=clock(control=start_control)

                  else

                     life=clock(                     )


              end if!timestep_is_adaptive


        end subroutine make_monte_carlo_K!start_control


            subroutine load_monte_carlo_K(file_name)


                  implicit none


                  integer::unit


                  call read_time_parameters()

                  call load_seed()

                   open(newunit=unit,file=trim(file_name))

                                                                   read(unit,*)
                                                                   read(unit,*)
                                                                   read(unit,*)

                                           call  read(unit,life); life_time&
                                                                 =life_time+time(life)
                  if(auto_save_field_conf) call  read(unit,auto); auto_save&
                                                                 =auto_save+time(auto)
                  if(measure_time_skipped) call  read(unit,skip); time_skip&
                                                                 =time_skip+time(skip)
                  if(timestep_is_adaptive) then

                                           call  read(unit,step)

                  else

                     step=clock(time=time_step)

              end if!timestep_is_adaptive


                  close(        unit                     )


        end subroutine load_monte_carlo_K!


            subroutine save_monte_carlo_K()


                  implicit none


                  integer::unit


                  call save_seed()

                   open(newunit=unit,file=time_file_name)

                                                                  write(unit,*) "LAST TIME-STAMP OF SIMULATION"
                                                                  write(unit,*) "___________","___________","___________",&
                                                                                "___________","___________","___________",&
                                                                                "___________","___________","___________",&
                                                                                "___________","___________","___________"
                                                                  write(unit,*)

                                           call write(unit,life); write(unit,*) " simulation"
                  if(auto_save_field_conf) call write(unit,auto); write(unit,*) " auto-saved configurations"
                  if(measure_time_skipped) call write(unit,skip); write(unit,*) " measurements"
                  if(timestep_is_adaptive) call write(unit,step); write(unit,*) " time-step"

                  close(        unit                    )

        end subroutine save_monte_carlo_K!


            subroutine read_time_parameters()


                  implicit none


                  write(*,"(2a)",advance="no") "life time                  : ",t_yellow
                   read(*,   *               )  life_time
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,"(2a)",advance="no") "auto save (0 for disabling): ",t_yellow
                   read(*,   *               )  auto_save
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,"(2a)",advance="no") "time skip (0 for disabling): ",t_yellow
                   read(*,   *               )  time_skip
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,"(2a)",advance="no") "time step (0 for disabling): ",t_yellow
                   read(*,   *               )  time_step
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,   *               )

                  call check_time_parameters()


        end subroutine read_time_parameters!


            subroutine check_time_parameters()


                  implicit none


                  if(time_skip>time_step*10) then

                     if(auto_save>time_skip*10) then

                        if(life_time<auto_save*10) stop "Error: insufficient life time"

                        auto_save_field_conf=.true.

                     else

                        if(life_time<time_skip*10) stop "Error: insufficient life time"

              end    if!auto_save>time_skip*10) then

                     measure_time_skipped=.true.

                  else

                     if(auto_save>time_step*10) then

                        if(life_time<auto_save*10) stop "Error: insufficient life time"

                        auto_save_field_conf=.true.

                     else

                        if(life_time<time_step*10) stop "Error: insufficient life time"

              end    if!auto_save>time_step*10

              end if!time_skip>time_step*10


        end subroutine check_time_parameters!


           subroutine agnostically_adapt_time_step_K(exp_control_step)


                  implicit none


                  real(KK)::exp_control_step


                  call step%clock(time_step*exp(control(life))/exp_control_step,&
                                                           log(exp_control_step))

                  if(life>=life_time-sqrt(time(step))&
                                         -time(step)) call step%time(life_time-time(life))


        end subroutine agnostically_adapt_time_step_K!exp_control_step


            subroutine print_time_K(unit,time,tag)


                  implicit none


                  integer        ,intent(inout)         ::unit
                  type(clock(KK)),intent(in   )         ::time
                  character(*   ),intent(in   ),optional::tag


                  if(present(tag)) call write(unit,tag )
                                   call write(unit,time)


        end subroutine print_time_K!unit,time,tag


  end module monte_carlo


#  endif
