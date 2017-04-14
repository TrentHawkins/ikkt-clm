!     ifort v2017.2

!     ifort -module "ifort/modules" -w main.F90 -mkl=sequential -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl

#     include "getopts.F90"
#     include "precision.F90"
#     include "interface.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/random_number_generator.F90"
#     include "monte_carlo/average.F90"
#     include "monte_carlo/time.F90"
#     include "monte_carlo/monte_carlo.F90"

#     include "tools/brent_minimization.F90"
#     include "tools/conjugate_gradient.F90"

#     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"
#     include "ikkt/complex_langevin.F90"
#     include "ikkt/observables.F90"


      program main


            use get_options
            use interface

            use tensor_type

            use random_number_generator
            use average_type
            use time_type
            use monte_carlo

            use brent_minimization
            use conjugate_gradient_method

            use constants
            use fields
            use complex_langevin
            use observables


            implicit none


            call begin_ikkt_simulation()


      contains


            subroutine begin_ikkt_simulation()


                  implicit none


                  call read_options()
                  call set_file_names()

                  if(configuration_loaded) then

                     call load_monte_carlo(time_setting,average_step,time_skip,timestep_is_variable,seed_file_name,time_file_name)

                  else

                     call make_monte_carlo(time_setting,average_step,time_skip,timestep_is_variable)

              end if!configuration_loaded


        end subroutine begin_ikkt_simulation!base_file_name


            subroutine read_options()


                  implicit none


                  character     ::optchar ! currently parsed option
                  character(500)::optarg  ! currently parsed argument value if not an option
                  integer       ::arglen  ! currently parsed argument true length
                  integer       ::stat    ! stat of parsing option
                  integer       ::argind  ! current argument running index starting where the options end
                  integer       ::argnum  ! number of remaining indices options aside

                  type(option)::options(3) ! list of long options


                  options(1)=option(    'load-save'    ,.false.,'c','Load previous configuration and continue simulation.'   ,'')
                  options(2)=option('variable-timestep',.false.,'t','Use variable timestep to remedy simulation travelling.' ,'')
                  options(3)=option(   'start-field'   ,.true. ,'a','Define start configuration temperature: cold(0), hot(1)','')

                  do

                     call getopt(options='cta'  ,&
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

                           stop 'Error: an initial field configuration temperature must be explicitely defined'

              end       select!case(optarg)

                     case default

              end    select!case(optchar)

              end do

                  call get_command_argument(argind+1,optarg,stat)

                  select case(stat)

                  case(-1)

                     print *,'Warning: file base name provided is too long and was truncated'

                     base_file_name=trim(adjustl(optarg))

                  case( 0)

                     base_file_name=trim(adjustl(optarg))

                  case(+1)

                     stop 'Error: a file base name must be provided to store simulation stat'

              end select!case(optchar)


        end subroutine read_options


            subroutine set_file_names()


                  implicit none


                  seed_file_name=trim(adjustl(base_file_name))//'.seed'
                  time_file_name=trim(adjustl(base_file_name))//'.time'
                  conf_file_name=trim(adjustl(base_file_name))//'.conf'


        end subroutine set_file_names


  end program main
