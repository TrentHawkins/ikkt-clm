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

#     include "system/text_format.F90"
#     include "system/signals.F90"
#     include "system/getopts.F90"
#     include "system/precision.F90"

!     include "tensor/tensor.F90"

!     include "monte_carlo/random_number_generator.F90"
!     include "monte_carlo/average.F90"
!     include "monte_carlo/time.F90"
#     include "monte_carlo/monte_carlo.F90"

!     include "tools/brent_minimization.F90"
!     include "tools/conjugate_gradient.F90"
!     include "tools/insert_sort.F90"

!     include "ikkt/constants.F90"
!     include "ikkt/fields.F90"
!     include "ikkt/tools/optimal_toolset.F90"
!     include "ikkt/tools/conjugate_gradient.F90"
!     include "ikkt/tools/gauge_cooling.F90"
#     include "ikkt/complex_langevin.F90"
#     include "ikkt/observables.F90"

!     define BLAS
!     define OPTIMAL


      program main


            use::get_options

      !     use::tensor_type

      !     use::random_number_generator
      !     use::average_type
      !     use::time_type
            use::monte_carlo

      !     use::brent_minimization
      !     use::conjugate_gradient_method

      !     use::constants
      !     use::fields
      !     use::complex_langevin
            use::gauge_cooling
            use::observables


            implicit none


            character(:),allocatable::base_file_name


      !     call signal_actions(eject_main)

            call   begin_ikkt_simulation()
            call perform_ikkt_simulation()
            call     end_ikkt_simulation()


      contains


            subroutine begin_ikkt_simulation()


                  implicit none


                  call read_options_and_arguments()
                  call boot_langevin()


        end subroutine begin_ikkt_simulation!


            subroutine perform_ikkt_simulation()


                  implicit none


                  integer::unit!s_counter,&
            !                   t_counter


                   open(newunit=unit,file=meas_file_name)

                  if(measure_time_skipped) then

                     do while(s%time_left())

                        do while(t%time_left())

                           call langevin_step()

              end       do!while(t%time_left())

                        call print_observables(unit)

              end    do!while(s%time_left())

                  else

                     do while(t%time_left())

                        call langevin_step()
                        call print_observables(unit)

              end    do!while(t%time_left())

              end if!measure_time_skipped

                  close(unit)


        end subroutine perform_ikkt_simulation!


            subroutine end_ikkt_simulation


                  implicit none


                  call save_monte_carlo()
                  call save_fields()

                  call print_gamma()

#                 ifndef OPTIMAL

                  call print_fermion_matrix()

#              endif


        end subroutine end_ikkt_simulation!


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin=>input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=>error_unit


                  implicit none


                  character     ::option_character ! currently parsed option
                  character(500)::option_argument  ! currently parsed argument value if not an option
                  integer       ::argument_length  ! currently parsed argument true length
                  integer       ::status           ! stat of parsing option
                  integer       ::argument_index   ! current argument running index starting where the options end
                  integer       ::argument_number  ! number of remaining indices options aside

                  type(option)::options(0:7) ! list of long options

                  integer::index


                  options(1)=option(    "skip-time"    ,.false.,'s',"Use a fixed time to skip before measuring observables. &
                                                                    &This is supposed tobe at least an order of magnitude &
                                                                    &larger than the average timestep, and usually is &
                                                                    &comparable to the autocorrelation time of the time &
                                                                    &history.","")

                  options(2)=option("variable-timestep",.false.,'t',"Use variable timestep with fixed timestep as average. This &
                                                                    &alelviates drift divergence due to instabilities in either a &
                                                                    &poorly chosen starting configuration or a large timestep.","")

                  options(3)=option("noisy-start-field",.false.,'a',"Start with a (gausssian) noisy (hot) initial configuration &
                                                                    &instead of the default zero (cold) initial configuration. &
                                                                    &Overriden by "//t_bold//"-c"//s_bold//" when loading a &
                                                                    &previously saved configuration.","")

                  options(4)=option(    "load-save"    ,.false.,'c',"Load simulation stats and field configuration from a previous &
                                                                    &simulation. This helps in storing thermalized configurations, &
                                                                    &and reusing instead of repeating thermalizations. Overrides "&
                                                                    &//t_bold//"-a"//s_bold//" when not starting a simulation from &
                                                                    &scratch.","")

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

                     call getopt(options="stacfgmh"       ,&
                                longopts=options         ,&
                                 optchar=option_character,&
                                  optarg=option_argument ,&
                                  arglen=argument_length ,&
                                    stat=status          ,&
                                  offset=argument_index  ,&
                                  remain=argument_number)

                     if(status==1) exit

                     select case(option_character)

                     case('s'); measure_time_skipped=.true.
                     case('t'); timestep_is_variable=.true.
                     case('a'); start_field_is_noisy=.true.
                     case('c'); configuration_loaded=.true.
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

                        stop

              end    select!case(option_character)

              end do

                  call get_command_argument(argument_index+1,option_argument,status)

                  select case(status)

                  case(:-1 )

                     stop "Error: bad argment index"

                  case( +1:)

                     stop "Error: a base file name must be provided to store simulation status"

                  default

                     continue 100

              end select!case(status)

              100 allocate(character(len(trim(adjustl(option_argument))))::base_file_name)
                                                                           base_file_name=trim(adjustl(option_argument))

                  call execute_command_line("mkdir --parents -- "//'"'//"${PWD#'ifort/bin'}"//simulation_path//'"')

                  allocate(character(len(base_file_name)+5)::seed_file_name)
                                                             seed_file_name=simulation_path//trim(adjustl(base_file_name))//".seed"
                  allocate(character(len(base_file_name)+5)::time_file_name)
                                                             time_file_name=simulation_path//trim(adjustl(base_file_name))//".time"
                  allocate(character(len(base_file_name)+5)::stat_file_name)
                                                             stat_file_name=simulation_path//trim(adjustl(base_file_name))//".stat"
                  allocate(character(len(base_file_name)+5)::conf_file_name)
                                                             conf_file_name=simulation_path//trim(adjustl(base_file_name))//".conf"
                  allocate(character(len(base_file_name)+5)::meas_file_name)
                                                             meas_file_name=simulation_path//trim(adjustl(base_file_name))//".meas"

                  write(*,*)


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
