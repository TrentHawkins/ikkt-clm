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

#     include "main/signals.F90"
#     include "main/getopts.F90"
#     include "main/precision.F90"

!     include "tensor/tensor.F90"

!     include "monte_carlo/random_number_generator.F90"
!     include "monte_carlo/average.F90"
!     include "monte_carlo/time.F90"
#     include "monte_carlo/monte_carlo.F90"

!     include "tools/brent_minimization.F90"
!     include "tools/conjugate_gradient.F90"

!     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"
#     include "ikkt/complex_langevin.F90"
#     include "ikkt/gauge_cooling.F90"
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


            call signal_actions(eject_main)

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


                  integer::unit


                   open(newunit=unit,file=meas_file_name)

            !     call print_observables(unit,t)

                  do while(t%time_left())

                     call langevin_step()
                     call print_observables(unit)

              end do!while(t%time_left())

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

                  type(option)::options(1:6) ! list of long options


                  options(1)=option(    "load-save"    ,.false.,'c',"Load previous configuration."                     ,"        ")
                  options(2)=option(   "start-field"   ,.true. ,'a',"New field configuration setting."                 ,"hot-cold")
                  options(3)=option("variable-timestep",.false.,'t',"Use variable timestep."                           ,"        ")
                  options(4)=option("fermions-included",.false.,'f',"Include fermions in simulation."                  ,"        ")
                  options(5)=option("  gauge-cooling  ",.false.,'g',"Apply gauge-cooling to configuratio"              ,"        ")
                  options(6)=option("mass-deformations",.false.,'m',"Include massive deformations in Complex Langevin.","        ")

                  do

                     call getopt(options="ca:tfgm"       ,&
                                longopts=options         ,&
                                 optchar=option_character,&
                                  optarg=option_argument ,&
                                  arglen=argument_length ,&
                                    stat=status          ,&
                                  offset=argument_index  ,&
                                  remain=argument_number)

                     if(status==1) exit

                     select case(option_character)

                     case('c')

                        configuration_loaded=.true.

                     case('a')

                        select case(option_argument)

                        case('0')

                           start_field_is_noisy=.false.

                        case('1')

                           start_field_is_noisy=.true.

                        case default

                           call print_opt(options(2),stderr)

              end       select!case(option_argument)

                     case('t')

                        timestep_is_variable=.true.

                     case('f')

                        fermions_included   =.true.

                     case('g')

                        gauge_cooling_active=.true.

                     case('m')

                        massive_deformations=.true.

                     case default

                        call print_opt(options(1),stderr)
                        call print_opt(options(2),stderr)
                        call print_opt(options(3),stderr)
                        call print_opt(options(4),stderr)
                        call print_opt(options(5),stderr)
                        call print_opt(options(6),stderr)
                        stop

              end    select!case(option_character)

              end do

                  call get_command_argument(argument_index+1,option_argument,status)

                  select case(status)

                  case(-1)

                     write(*,*) "Warning: base file name provided is too long and was truncated"

                  case( 0)
                  case(+1)

                     stop "Error: a base file name must be provided to store simulation status"

              end select!case(status)

                  allocate(character(len(trim(adjustl(option_argument))))::base_file_name)
                                                                           base_file_name=trim(adjustl(option_argument))

                  allocate(character(len(base_file_name)+5)::seed_file_name);seed_file_name=trim(adjustl(base_file_name))//".seed"
                  allocate(character(len(base_file_name)+5)::time_file_name);time_file_name=trim(adjustl(base_file_name))//".time"
                  allocate(character(len(base_file_name)+5)::stat_file_name);stat_file_name=trim(adjustl(base_file_name))//".stat"
                  allocate(character(len(base_file_name)+5)::conf_file_name);conf_file_name=trim(adjustl(base_file_name))//".conf"
                  allocate(character(len(base_file_name)+5)::meas_file_name);meas_file_name=trim(adjustl(base_file_name))//".meas"


        end subroutine read_options_and_arguments!


            subroutine eject_main()

                  implicit none


                  call eject_complex_langevin()

                  call end_ikkt_simulation()

                  print *
                  print *,"Used memory deallocated."

                  stop


        end subroutine eject_main!


            subroutine signal_actions(handler)


                  implicit none


                  external::handler


                  call signal(SIGHUP,handler)
                  call signal(SIGINT,handler)
                  call signal(SIGFPE,handler)

                  call signal(SIGQUIT,handler)
                  call signal(SIGABRT,handler)


        end subroutine signal_actions!handler


  end program main
