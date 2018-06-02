#     include "../../system/getopts.F90"
#     include "../../system/precision.F90"

#     include "../../analysis/jack_knife/jack_knife.F90"


      program main


            use::getptions

            use::jack_knife


            implicit none


            character(*),parameter::           format_jack_single_K=REALGK
            character(*),parameter::text_field_format_jack_single_K=REALAK

            character(*),parameter::           format_jack_double_K=COMPLEXGK
            character(*),parameter::text_field_format_jack_double_K=COMPLEXAK


            character(:),allocatable::record_name
            character(:),allocatable::output_name

            integer::jack_bins=1

            logical::weighted_averages=.false.
            logical::default_jack_bins=.true.

            integer::record_index=0
            integer::record_size =0

            integer::record_unit
            integer::output_unit=6

            real(KK)::average,error

            real(        KK ),dimension(:),allocatable::record_data

            integer::record_line


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

            if(default_jack_bins) jack_bins=int(sqrt(real(record_size,kind=KK)))

             open(newunit=record_unit,file=record_name,position='rewind')

              allocate(record_data(record_size))

            do record_line=0,record_size-1,+1

               read(record_unit,format_jack_single_K             ) record_data(record_line)

        end do!record_line=0,record_size-1,+1

            call jack(jack_bins,record_size,record_data,average,error)

            write(output_unit,format_jack_double_K) average,error
            write(output_unit,                   *)

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

                  type(option)::options(2) ! list of long options


                  options(1)=option("  jack-bins",.true. ,'j',"Number of jack-knife bins.","jack-bins")
                  options(2)=option("output-file",.true. ,'o',"Output file-name."         ,"file-name")

                  do

                     call getopt(options="j:o:" ,&
                                longopts=options,&
                                 optchar=optchar,&
                                  optarg=optarg ,&
                                  arglen=arglen ,&
                                    stat=stat   ,&
                                  offset=argind ,&
                                  remain=argnum)

                     if(stat==1) exit

                     select case(optchar)

                     case('j')

                        jack_bins=int(optarg)

                        default_jack_bins=.false.

                     case('o')

                        allocate(character(len(trim(adjustl(optarg))))::output_name)
                                   output_name=trim(adjustl(optarg))

                        output_unit=16

                     case default

                        call print_opt(options(1),stderr)
                        call print_opt(options(2),stderr)

                        stop

              end    select!case(optchar)

              end do

                  call get_command_argument(argind+1,optarg)

                  allocate(character(len(trim(adjustl(optarg))))::record_name)
                             record_name=trim(adjustl(optarg))


        end subroutine read_options_and_arguments!


  end program main
