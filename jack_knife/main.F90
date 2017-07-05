#     include "main/getopts.F90"
#     include "main/precision.F90"

#     include "jack_knife.F90"


      program main


            use::get_options

            use::jack_knife


            implicit none


            character(*),parameter::           format_jack_K=REALGK
            character(*),parameter::text_field_format_jack_K=REALAK

            character(*),parameter::           format_jack_weighted_K=COMPLEXGK
            character(*),parameter::text_field_format_jack_weighted_K=COMPLEXAK


            integer::weight_column=0
            integer:: value_column=1

            logical::susceptibility_in=.false.

            character(:),allocatable::file_name

            integer::jack_bins=10,record_index=0,&
                                  record_size =0,&
                                  record_unit

            real(KK)::average_o,error_o,&
                      average_x,error_x

            real(KK),dimension(:),allocatable::record_values ,&
                                               record_weights

            integer::i,j


            call read_options_and_arguments()

             open(newunit=record_unit,file=file_name)

            do

                read(     record_unit,format_jack_K,end=100)

               record_size&
              =record_size+1

        end do

        100 continue

            close(        record_unit)

             open(newunit=record_unit,file=file_name,position='rewind')

            if(weighted_averages) then

               allocate(record_values (record_size))
               allocate(record_weights(record_size))

               do i=1,record_size-1,+1

                   read(  record_unit,format_jack_weighted_K) record_values(i),record_weights(i)

        end    do! i=1,record_size-1,+1

               if(susceptibility_in) then

                  call jack(record_size,jack_bins,record_values ,&
                                                  record_weights,average_o,error_o,&
                                                                 average_x,error_x)

                  write(*,format_jack_weighted_K,advance='no') average_o,error_o
                  write(*,format_jack_weighted_K,advance='no') average_x,error_x
                  write(*,                     *             )

               else

                  call jack(record_size,jack_bins,record_values ,&
                                                  record_weights,average_o,error_o)

                  write(*,format_jack_weighted_K,advance='no') average_o,error_o
                  write(*,                     *             )

           end if!susceptibility_in

            else

               allocate(record_values (record_size))

               do i=1,record_size-1,+1

                   read(  record_unit,format_jack_K) record_values(i)

        end    do! i=1,record_size-1,+1

               if(susceptibility_in) then

                  call jack(record_size,jack_bins,record_values,average_o,error_o,&
                                                                average_x,error_x)

                  write(*,format_jack_K,advance='no') average_o
                  write(*,format_jack_K,advance='no')   error_o
                  write(*,format_jack_K,advance='no') average_x
                  write(*,format_jack_K,advance='no')   error_x
                  write(*,            *             )

               else

                  call jack(record_size,jack_bins,record_values,average_o,error_o)

                  write(*,format_jack_K,advance='no') average_o
                  write(*,format_jack_K,advance='no')   error_o
                  write(*,            *             )

           end if!susceptibility_in

        end if!weighted_averages

            close(        record_unit)


      contains


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin=>input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=>error_unit


                  implicit none


                  character     ::optchar ! currently parsed option
                  character(500)::optarg  ! currently parsed argument value if not an option
                  integer       ::arglen  ! currently parsed argument true length
                  integer       ::stat    ! stat of parsing option
                  integer       ::argind  ! current argument running index starting where the options end
                  integer       ::argnum  ! number of remaining indices options aside

                  type(option)::options(3) ! list of long options


                  options(1)=option("    jack-bins    ",.true. ,'j',"Number of jack-knife bins."                      ,"jack_bins")
                  options(2)=option("weighted-averages",.false.,'w',"Use weighted averages on variable timestep data.","")
                  options(3)=option("susceptibility-in",.false.,'x',"Include the calculation of susceptibility."      ,"")

                  do

                     call getopt(options="j:w:v:x",&
                                longopts=options ,&
                                 optchar=optchar ,&
                                  optarg=optarg  ,&
                                  arglen=arglen  ,&
                                    stat=stat    ,&
                                  offset=argind  ,&
                                  remain=argnum)

                     if(stat==1)then

                        exit

              end    if!stat==1

                     select case(optchar)

                     case('j')

                         read(optarg,*) jack_bins

                     case('w')

                        weighted_averages=.true.

                     case('x')

                        susceptibility_in=.true.

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

                     write(*,*) "Warning: base file name provided is too long and was truncated"

                  case( 0)
                  case(+1)

                     stop "Error: a base file name must be provided to store simulation status"

              end select!case(optchar)

                  allocate(character(len(trim(adjustl(optarg))))::file_name);file_name=trim(adjustl(optarg))


        end subroutine read_options_and_arguments!


  end program main
