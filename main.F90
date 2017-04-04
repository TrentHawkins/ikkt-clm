!     ifort v2017.2

!     ifort -module "ifort/module" -w main.F90 -mkl=sequential -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl

#     include "getopts.F90"
#     include "precision.F90"

#     include "tensor/tensor.F90"

#     include "monte_carlo/random_number_generator.F90"
#     include "monte_carlo/average.F90"
#     include "monte_carlo/time.F90"
#     include "monte_carlo/monte_carlo.F90"

#     include "tools/brent_minimization.F90"
#     include "tools/conjugate_gradient.F90"

#     include "ikkt/interface.F90"
#     include "ikkt/constants.F90"
#     include "ikkt/fields.F90"
#     include "ikkt/complex_langevin.F90"
#     include "ikkt/observables.F90"


      program main


            use get_options

            use interface
            use constants
            use fields
            use complex_langevin
            use observables


            implicit none


            logical::configuration_loaded
            logical::timestep_is_variable
            logical::start_field_is_hot


      contains


            subroutine begin_ikkt_simulation(file_base_name,argind,argnum)


                  implicit none


                  character(len=500),intent(  out)::file_base_name

                  character                       ::opchar ! currently parsed option
                  character(len=500)              ::optarg ! currently parsed argument value if not an option
                  integer                         ::arglen ! currently parsed argument true length
                  integer                         ::status ! status of parsing option
                  integer           ,intent(  out)::argind ! current argument running index starting where the options end
                  integer           ,intent(  out)::argnum ! number of remaining indices options aside

                  type(option)::options(3) ! list of long options


                  options(1)=option(    'load-save'    ,.false.,'c','Load previous configuration and continue simulation.'   ,'')
                  options(2)=option('variable-timestep',.false.,'t','Use variable timestep to remedy simulation travelling.' ,'')
                  options(3)=option(   'start-field'   ,.true. ,'a','Define start configuration temperature: cold(0), hot(1)','')

                  do

                     call getopt(options='cta'  ,&
                                longopts=options,&
                                 optchar=opchar ,&
                                  optarg=optarg ,&
                                  arglen=arglen ,&
                                    stat=status ,&
                                  offset=argind ,&
                                  remain=argnum)

                     if(status==1)then

                        exit

              end    if!status==1

                     select case(opchar)

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

                           stop 'Error: You must define initial field configuration temperature (not implied).'

              end       select!case(optarg)

                     case default

              end    select!case(opchar)

              end do

                  call get_command_argument(argind+1,optarg,status)

                  select case(status)

                  case(-1)

                     print *,'Warning: file base-name too long and was truncated'

                     file_base_name=optarg

                  case( 0)

                     file_base_name=optarg

                  case(+1)

                     stop 'Error: file base-name must be provided to store simulation status'

              end select!case(opchar)


        end subroutine begin_ikkt_simulation!file_base_name


  end program main
