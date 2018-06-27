#     include "../../system/getopts.F90"
#     include "../../system/precision.F90"

#     include "../../tools/constants.F90"


      program main


            use::get_options

            use::mathematical_constants


            implicit none


            character(*),parameter::           format_hist_single_K=REALGK
            character(*),parameter::text_field_format_hist_single_K=REALAK

            character(*),parameter::           format_hist_double_K=COMPLEXGK
            character(*),parameter::text_field_format_hist_double_K=COMPLEXAK


            character(:),allocatable::record_name
            character(:),allocatable::output_name

            logical::weighted_averages=.false.
            logical::default_hist_bins=.true.

            integer::record_size=0
            integer::record_line

            integer::record_unit
            integer::output_unit=6

            integer::hist_bins=1,bin_index

            real(KK)::bin_width
            real(KK)::bin_data
            real(KK)::bin_min
            real(KK)::bin_max

            real(KK),dimension(0:             ),allocatable::histogram


            call read_options_and_arguments()

             open(   unit=output_unit,file=output_name)
             open(newunit=record_unit,file=record_name)

            bin_min=+huge(bin_data)
            bin_max=-huge(bin_data)

            do

               read(record_unit,format_jack_single_K,err=100)

               if(bin_min>bin_data) &
                  bin_min=bin_data

               if(bin_max<bin_data) &
                  bin_max=bin_data

               record_size&
              =record_size+1

        end do

        100 continue

            close(        record_unit)

            if(default_hist_bins) hist_bins=int(sqrt(real(record_size,kind=KK)) -
                                             epsilon(real(record_size,kind=KK)))+1

             open(newunit=record_unit,file=record_name,position='rewind')

            bin_width=(bin_max&
                      -bin_min)/hist_bins

              allocate(histogram(hist_bins))

            histogram=zero

            do record_line=1,record_size,+1

                read(record_unit,format_jack_single_K) bin_data

               bin_index=int((bin_data-bin_min)/bin_width)

               histogram(bin_index)&
              =histogram(bin_index)+1

        end do!record_line=1,record_size,+1

            do bin_index=0,hist_bins-1,+1

               write(record_unit,format_jack_single_K,advance="no") bin_min
               write(record_unit,format_jack_single_K,advance="no") bin_max
               write(record_unit,format_jack_single_K,advance="no") histogram(bin_index)
               write(record_unit,format_jack_single_K,advance="no") histogram/record_size
               write(record_unit,format_jack_single_K             ) histogram/record_size/bin_width

        end do!bin_index=0,hist_bins-1,+1

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


                  options(1)=option("  hist-bins",.true. ,'j',"Number of jack-knife bins.","hist-bins")
                  options(2)=option("output-file",.true. ,'o',"Output file-name."         ,"file-name")

                  do

                     call getopt(options="h:o:" ,&
                                longopts=options,&
                                 optchar=optchar,&
                                  optarg=optarg ,&
                                  arglen=arglen ,&
                                    stat=stat   ,&
                                  offset=argind ,&
                                  remain=argnum)

                     if(stat==1) exit

                     select case(optchar)

                     case('h')

                        read(optarg,*) hist_bins

                        default_hist_bins=.false.

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

                  call get_command_argument(argind+2,optarg)

                  allocate(character(len(trim(adjustl(optarg))))::output_name)
                             output_name=trim(adjustl(optarg))


        end subroutine read_options_and_arguments!


  end program main
