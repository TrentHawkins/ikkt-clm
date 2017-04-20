!     ifort v2017.2

!     ifort -module "ifort/modules" -w main.F90 -mkl=sequential -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl

#     include "getopts.F90"
#     include "precision.F90"
#     include "interface.F90"

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
#     include "ikkt/observables.F90"


      program main


            use::get_options
            use::interface

      !     use::tensor_type

      !     use::random_number_generator
      !     use::average_type
      !     use::time_type
            use::monte_carlo

      !     use::brent_minimization
      !     use::conjugate_gradient_method

      !     use::constants
            use::fields
            use::complex_langevin
            use::observables


            implicit none


            call   begin_ikkt_simulation()
            call perform_ikkt_simulation()
            call     end_ikkt_simulation()


      contains


            subroutine begin_ikkt_simulation()


                  implicit none


                  call read_options_and_arguments()
                  call read_time_parameters()

                  if(configuration_loaded) then

                     call load_monte_carlo(time_setting,average_step,ac_time_skip,timestep_is_variable,seed_file_name,time_file_name)
                     call load_fields(conf_file_name)

                  else

                     call make_monte_carlo(time_setting,average_step,ac_time_skip,timestep_is_variable)
                     call make_fields(start_field_is_hot)

              end if!configuration_loaded


        end subroutine begin_ikkt_simulation!base_file_name


            subroutine perform_ikkt_simulation()


                  implicit none


                  integer::unit


                   open(newunit=unit,file=meas_file_name)

                  do while(s%time_left())

                     do while(t%time_left())

                        call langevin_step()

                        call t%push_time(drift_norm())

              end    do!while(t%time_left())

                     call print_observables(unit,t)

                     call t%zero_time()
                     call s%push_time(drift_norm())

              end do!while(s%time_left())

                  close(unit)


        end subroutine perform_ikkt_simulation


            subroutine end_ikkt_simulation


                  implicit none


                  call save_monte_carlo(seed_file_name,time_file_name)
                  call save_fields(conf_file_name)


        end subroutine end_ikkt_simulation


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin=>input_unit,stdout=>output_unit,stderr=>error_unit


                  implicit none


                  character     ::optchar ! currently parsed option
                  character(500)::optarg  ! currently parsed argument value if not an option
                  integer       ::arglen  ! currently parsed argument true length
                  integer       ::stat    ! stat of parsing option
                  integer       ::argind  ! current argument running index starting where the options end
                  integer       ::argnum  ! number of remaining indices options aside

                  type(option)::options(3) ! list of long options


                  options(1)=option(    'load-save'    ,.false.,'c' ,'Load previous configuration.'           ,                '')
                  options(2)=option('variable-timestep',.false.,'t' ,'Use variable timestep.'                 ,                '')
                  options(3)=option(   'start-field'   ,.true. ,'a','Define start configuration temperature.','<0(cold)/1(hot)>')

                  do

                     call getopt(options='cta:'  ,&
                                longopts=options,&
                                 optchar=optchar,&
                                  optarg=optarg ,&
                                  arglen=arglen ,&
                                    stat=stat   ,&
                                  offset=argind ,&
                                  remain=argnum)

                     if(stat==1)then

                        exit

              end    if!stat==1

                     select case(optchar)

                     case('c')

                        configuration_loaded=.true.

                     case('t')

                        timestep_is_variable=.true.

                     case('a')

                        select case(optarg)

                        case('0')

                           start_field_is_hot=.false.

                        case('1')

                           start_field_is_hot=.true.

                        case default

                           call print_opt(options(3),stderr)
                           stop

              end       select!case(optarg)

                     case default

                        call print_opt(options(1),stderr)
                        call print_opt(options(2),stderr)
                        call print_opt(options(3),stderr)
                        stop

              end    select!case(optchar)

              end do

                  call get_command_argument(argind+1,optarg,stat)

                  select case(stat)

                  case(-1)

                     write(*,*) 'Warning: base file name provided is too long and was truncated'

                  case( 0)
                  case(+1)

                     stop 'Error: a base file name must be provided to store simulation status'

              end select!case(optchar)

                  allocate(character(len(trim(adjustl(optarg))))::base_file_name);base_file_name=trim(adjustl(optarg))

                  allocate(character(len(base_file_name)+5)::seed_file_name);seed_file_name=trim(adjustl(base_file_name))//'.seed'
                  allocate(character(len(base_file_name)+5)::time_file_name);time_file_name=trim(adjustl(base_file_name))//'.time'
                  allocate(character(len(base_file_name)+5)::conf_file_name);conf_file_name=trim(adjustl(base_file_name))//'.conf'
                  allocate(character(len(base_file_name)+5)::meas_file_name);meas_file_name=trim(adjustl(base_file_name))//'.meas'

        end subroutine read_options_and_arguments


            subroutine read_time_parameters()


                  implicit none


                  write(*,'(a)',advance='no') 'time_setting: '
                   read(*,  *               )  time_setting
                  write(*,  *               )

                  write(*,'(a)',advance='no') 'average_step: '
                   read(*,  *               )  average_step
                  write(*,  *               )

                  write(*,'(a)',advance='no') 'ac_time_skip: '
                   read(*,  *               )  ac_time_skip
                  write(*,  *               )


        end subroutine read_time_parameters


  end program main
