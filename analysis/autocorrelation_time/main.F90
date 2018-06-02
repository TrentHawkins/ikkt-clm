#     include "../../system/getopts.F90"
#     include "../../system/precision.F90"

#     include "../../tools/constants.F90"

#     include "../../analysis/autocorrelation_time/autocorrelation_time.F90"


      program main


            use::mathematical_constants

            use::autocorrelation_time


            implicit none


            character(*),parameter::           format_time_single_K=REALGK
            character(*),parameter::text_field_format_time_single_K=REALAK

            character(*),parameter::           format_time_double_K=COMPLEXGK
            character(*),parameter::text_field_format_time_double_K=COMPLEXAK


            character(:),allocatable::record_name
            character(:),allocatable::output_name

            integer::record_size =0
            integer::record_line

            integer::record_unit
            integer::output_unit=6

            real(KK)::           autocorrelation_function
            real(KK)::           autocorrelation_function_normalization
            real(KK)::integrated_autocorrelation_time

            real(        KK ),dimension(:),allocatable::record_data


            call read_options_and_arguments()

             open(   unit=output_unit,file=output_name)
             open(newunit=record_unit,file=record_name)

            do

               read(record_unit,format_jack_single_K,err=100)

               record_size&
              =record_size+1

        end do

        100 continue

            close(        record_unit)

             open(newunit=record_unit,file=record_name,position='rewind')

              allocate(record_data(record_size))

            autocorrelation_function_normalization=autocorrelation(record_size,record_data,0)

            integrated_autocorrelation_time=nil

            do record_line=1,record_size,+1

                read(record_unit,format_time_single_K) record_data(record_line)

               autocorrelation_function=autocorrelation(record_size,&
                                                        record_data,&
                                                        record_line)/autocorrelation_function_normalization

               integrated_autocorrelation_time&
              =integrated_autocorrelation_time+autocorrelation_function

               write(output_unit,format_time_double_K) autocorrelation_function,integrated_autocorrelation_time

        end do!record_line=1,record_size,+1

            deallocate(record_data)

            close(        record_unit)
            close(        output_unit)


      contains


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin => input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=> error_unit


                  implicit none


                  character      ::optchar ! currently parsed option
                  character(CHAR)::optarg  ! currently parsed argument value if not an option
                  integer        ::arglen  ! currently parsed argument true length
                  integer        ::stat    ! stat of parsing option
                  integer        ::argind  ! current argument running index starting where the options end
                  integer        ::argnum  ! number of remaining indices options aside

                  type(option)::options(1) ! list of long options


                  options(1)=option("output-file",.true. ,'o',"Output file-name."         ,"file-name")

                  do

                     call getopt(options="o:" ,&
                                longopts=options,&
                                 optchar=optchar,&
                                  optarg=optarg ,&
                                  arglen=arglen ,&
                                    stat=stat   ,&
                                  offset=argind ,&
                                  remain=argnum)

                     if(stat==1) exit

                     select case(optchar)

                     case('o')

                        allocate(character(len(trim(adjustl(optarg))))::output_name)
                                   output_name=trim(adjustl(optarg))

                        output_unit=16

                     case default

                        call print_opt(options(1),stderr)

                        stop

              end    select!case(optchar)

              end do

                  call get_command_argument(argind+1,optarg)

                  allocate(character(len(trim(adjustl(optarg))))::record_name)
                             record_name=trim(adjustl(optarg))


        end subroutine read_options_and_arguments!


  end program main
