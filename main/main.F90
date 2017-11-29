!     ifort v2017.4

!     without BLAS:

!     ifort -module "ifort/modules" -fast -w main/main.F90
!     -mkl=sequential -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl
!
!     with BLAS:
!
!     ifort -module "ifort/modules" -fast -w -DBLAS main/main.F90
!     -mkl=sequential -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl

!     You may need the option "-mcmodel=medium" if the stored arrays are too large, which conflicts with the "-fast" option.
!     Later when the optimized version using commutators is fixed, it will not be necessary, as the fermion matrix is bypassed.

#     include "../main/mathematical_constants.F90"

#     include "../system/getopts.F90"
#     include "../system/signals.F90"
#     include "../system/text_format.F90"
#     include "../system/precision.F90"
#     include "../system/version.F90"

!     include "../tensor/tensor.F90"

!     include "../monte_carlo/random_number_generator.F90"
!     include "../monte_carlo/average.F90"
!     include "../monte_carlo/time.F90"
#     include "../monte_carlo/monte_carlo.F90"

!     include "../tools/insert_sort.F90"
!     include "../tools/brent_minimization.F90"

#     ifdef OPTIMAL

!     include "../ikkt/conjugate_gradient.F90"
!     include "../ikkt/optimal_toolset.F90"

#     else

!     include "../tools/conjugate_gradient.F90"

#  endif

!     include "../ikkt/constants.F90"
!     include "../ikkt/fields.F90"
!     include "../ikkt/gauge_cooling.F90"
#     include "../ikkt/complex_langevin.F90"
!     include "../ikkt/observables.F90"

!     define BLAS
!     define OPTIMAL


      program main


!           use::mathematical_constants

            use::get_options
!           use::text_formatting
            use::version

!           use::tensor_type

!           use::random_number_generator
!           use::average_type
!           use::time_type
            use::monte_carlo

!           use::insertion_sort
!           use::brent_minimization
!           use::conjugate_gradient_method

#           ifdef OPTIMAL

!           use::optimal_toolset

#        endif

!           use::constants
!           use::fields
            use::complex_langevin
            use::gauge_cooling
!           use::observables


            implicit none



            character(*),parameter::data_path_name="data/"

            character(:),allocatable::base_file_name


!           call signal_actions(eject_main)

            call   begin_ikkt_simulation()
            call perform_ikkt_simulation()
            call     end_ikkt_simulation()


      contains


            subroutine begin_ikkt_simulation()


                  implicit none


                  call read_options_and_arguments()
                  call boot_langevin()

                   open(newunit=meas_unit,file=meas_file_name)
                   open(newunit=auto_unit,file=auto_file_name)


        end subroutine begin_ikkt_simulation!


            subroutine perform_ikkt_simulation()


                  implicit none


                  integer::unit


                  do while(life<=life_time)

                     if(timestep_is_adaptive) call agnostically_adapt_time_step(drift_norm())

                     life&
                    =life+step

                     call langevin_step()

                     if(measure_time_skipped) then

                        if(skip<=life) then

                           skip&
                          =skip+time_skip

                           go to 231

              end       if!skip<=life

                     else

              231       call print_observables()

              end    if!measure_time_skipped

                     if(auto_save_field_conf) then

                        if(auto<=life) then

                           skip&
                          =skip+time_skip

                           go to 232

              end       if!auto<=life

                     else

              232       call add_to_connfigurations()

              end    if!auto_save_field_conf

              end do!while(life<=life_time)

                  close(        unit                    )


        end subroutine perform_ikkt_simulation!


            subroutine end_ikkt_simulation


                  implicit none


                  integer::unit


                  call wrap_langevin()
                  call write_version()


        end subroutine end_ikkt_simulation!


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin=>input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=>error_unit


                  implicit none


                  character             ::option_character ! currently parsed option
                  character(len_argname)::option_argument  ! currently parsed argument value if not an option
                  integer               ::argument_length  ! currently parsed argument true length
                  integer               ::status           ! stat of parsing option
                  integer               ::argument_index   ! current argument running index starting where the options end
                  integer               ::argument_number  ! number of remaining indices options aside

                  type(option)::options(0:6) ! list of long options

                  integer::index,time_stamp(8)


                  options(1)=option("variable-timestep",.false.,'t',"Use variable timestep with fixed timestep as average. This &
                                                                    &alelviates drift divergence due to instabilities in either a &
                                                                    &poorly chosen starting configuration or a large timestep.","")
                  options(2)=option("noisy-start-field",.false.,'a',"Start with a (gausssian) noisy (hot) initial configuration &
                                                                    &instead of the default zero (cold) initial configuration. &
                                                                    &Overriden by "//t_bold//"-c"//s_bold//" when loading a &
                                                                    &previously saved configuration.","")
                  options(3)=option(    "load-save"    ,.false.,'c',"Load simulation stats and field configuration from a previous &
                                                                    &simulation. This helps in storing thermalized configurations, &
                                                                    &and reusing instead of repeating thermalizations. Overrides "&
                                                                    &//t_bold//"-a"//s_bold//" when not starting a simulation from &
                                                                    &scratch.","")
                  options(4)=option( "run-observables" ,.false.,'o',"Load a sample of configurations for re-calculating &
                                                                    &observables, to prevent re-runnning simulations for new &
                                                                    &calculations.","configuration file")
                  options(5)=option("fermions-included",.false.,'f',"Include fermions in simulation, for simulating the full IKKT &
                                                                    &model.","")
                  options(6)=option(  "gauge-cooling"  ,.false.,'g',"Applythe standard gauge-cooling post-processing to &
                                                                    &configurations after each langevin step. This alleviates &
                                                                    &simulation journeys in the imaginary direction.","")
                  options(7)=option("mass-deformations",.false.,'m',"Include massive deformations in modifying the IKKT drift used &
                                                                    &in the Complex Langevin method. In particular for the boson &
                                                                    &model, a new coupling parameter is added, along with masses &
                                                                    &for the gauge bosons. In the full model with fermions, a &
                                                                    &fermionic mass is added as well.","")
                  options(0)=option("help"             ,.false.,'h',"Print this help screen.","")

                  do

                     call getopt(options="taco:fgmh"     ,&
                                longopts=options         ,&
                                 optchar=option_character,&
                                  optarg=option_argument ,&
                                  arglen=argument_length ,&
                                    stat=status          ,&
                                  offset=argument_index  ,&
                                  remain=argument_number)

                     if(status==1) exit

                     select case(option_character)

                     case('t'); timestep_is_adaptive=.true.
                     case('a'); start_field_is_noisy=.true.
                     case('c'); configuration_loaded=.true.; call read_version()
                     case('o'); running_measurements=.true.; call read_version()
                     case('f'); fermions_included_in=.true.
                     case('g'); gauge_cooling_active=.true.
                     case('m'); massive_deformations=.true.

                     case default

                        write(*,    *   )
                        write(*,"(*(a))") "Usage: ",                          t_bold,"BINARY_NAME",     s_bold,&
                                          " [",                               t_bold,"OPTIONS",         s_bold,&
                                          "... ] ",                           t_bold,"BASE_OUTPUT_NAME",s_bold
                        write(*,    *   )
                        write(*,"(*(a))") "Simulation compiled with ",        t_bold,"-o BINARY_NAME",  s_bold," plus preprocessor."
                        write(*,"(*(a))") "Simulation file names start with ",t_bold,"BASE_OUTPUT_NAME",s_bold," plus extension."
                        write(*,    *   )
                        write(*,"(*(a))") "Mandatory arguments to long options are mandatory for short options too."
                        write(*,    *   )

                        do index=1,size(options)-1,+1

                           call print_opt(options(index),stderr)

              end       do!index=1,size(options)-1,+1

                        write(*,"(*(a))") "All options are optional switches for the simulation with default meaning in"
                        write(*,"(*(a))") "their absence. If you run simulation via shell script that ports all available"
                        write(*,"(*(a))") "options, use shell script's name as ",t_bold,"BINARY_NAME",s_bold," instead."
                        write(*,    *   )
                        write(*,"(*(a))") "If you wish to measure in every step of the simulation, input zero ",t_bold,&
                                          "average_skip"                                                       ,s_bold,"."
                        write(*,    *   )

                        stop

              end    select!case(option_character)

              end do

                  write(*,*)

                  call get_command_argument(argument_index+1,option_argument,argument_length,status)

                  select case(status)

                  case(-1)

                     stop "Error: argument was trimmed"

                  case( 0)

                     continue

                  case default

                     stop "Error: a base file name must be provided to store simulation status"

              end select!case(status)

                  base_file_name=trim(adjustl(option_argument))

                  call execute_command_line("mkdir --parents -- "//trim(data_path_name))

                  base_file_name=trim(data_path_name)//base_file_name

                  call date_and_time(values=time_stamp)

                  base_file_name=trim(base_path_name)//"."//trim(time_stamp(1))&
                                                     //"-"//trim(time_stamp(2))&
                                                     //"-"//trim(time_stamp(3))&
                                                     //"."//trim(time_stamp(5))&
                                                     //":"//trim(time_stamp(6))&
                                                     //":"//trim(time_stamp(7))

                  seed_file_name=base_file_name//".seed";!write(*,"(a)") seed_file_name
                  time_file_name=base_file_name//".time";!write(*,"(a)") time_file_name
                  conf_file_name=base_file_name//".conf";!write(*,"(a)") conf_file_name
                  meas_file_name=base_file_name//".meas";!write(*,"(a)") meas_file_name
                  save_file_name=base_file_name//".save";!write(*,"(a)") meas_file_name


        end subroutine read_options_and_arguments!


            subroutine eject_main()


                  implicit none

                  call eject_complex_langevin()

                  call end_ikkt_simulation()

                  print *
                  print *,"Used memory deallocated."

                  stop


        end subroutine eject_main!


  end program main
