#     ifndef MONTE_CARLO_RANDOM_NUMBER_GENERATOR_F90
#     define MONTE_CARLO_RANDOM_NUMBER_GENERATOR_F90

#     include "../system/precision.F90"
#     include "../system/text_format.F90"


      module random_number_generator


            use::text_formatting


            implicit none


            character(*),private,parameter::           format_seed=INTEGERG2
            character(*),private,parameter::text_field_format_seed=INTEGERA2

            character(:),allocatable,public::seed_file_name

            integer,                                     private::size_seed
            integer,allocatable,dimension( :           ),private::     seed

            public::make_seed
            public::load_seed
            public::save_seed

            private::prepare_seed

            public:: read_seed
            public::write_seed
            public::print_seed


      contains


            subroutine make_seed()


                  implicit none


                  call random_seed()


        end subroutine make_seed!


            subroutine load_seed(file_name)


                  implicit none


                  character(*),intent(in   )::file_name

                  integer::unit


                  call prepare_seed()
                  call random_seed(put=seed)

                   open(newunit=unit,file=trim(file_name))

                   read(unit,          *)
                   read(unit,          *)
                   read(unit,          *)
                   read(unit,format_seed) seed

                  close(        unit                     )

                  if(allocated(seed)) deallocate(seed)


        end subroutine load_seed!file_name


            subroutine save_seed(file_name)


                  implicit none


                  character(*),intent(in   )::file_name

                  integer::unit


                  call prepare_seed()
                  call random_seed(get=seed)

                   open(newunit=unit,file=trim(file_name))

                  write(unit,          *) "LAST RANDOM NUMBER GENERATOR SEED"
                  write(unit,          *) "___________","___________","___________","___________","___________","___________",&
                                          "___________","___________","___________","___________","___________","___________"
                  write(unit,          *)
                  write(unit,format_seed) seed

                  close(        unit                     )

                  if(allocated(seed)) deallocate(seed)


        end subroutine save_seed!file_name


            subroutine prepare_seed()


                  implicit none


                  call random_seed(size=size_seed)

                  if(allocated(seed)) then

                     if(size(seed)/=size_seed) then

                        deallocate(seed)

              end    if!size(seed)/=size_seed

                  else

                     allocate(seed(1:size_seed))

              end if!allocated(seed)


        end subroutine prepare_seed!


            subroutine  read_seed(unit,iostat)


                  implicit none


                  integer,intent(inout)::unit

                  integer::index


                  if(allocated(seed)) then

                     do index=1,size(seed),+1

                         read(unit,format_seed,advance="no") seed(index)

              end    do!index=1,size(seed),+1

              end if!allocated(seed)


        end subroutine  read_seed!unit,iostat


            subroutine write_seed(unit,iostat)


                  implicit none


                  integer,intent(inout)::unit

                  integer::index


                  if(allocated(seed)) then

                     do index=1,size(seed),+1

                        write(unit,format_seed,advance="no") seed(index)

              end    do!index=1,size(seed),+1

              end if!allocated(seed)


        end subroutine write_seed!unit,iostat


            subroutine print_seed(unit,tag)


                  implicit none


                  integer        ,intent(inout)         ::unit
                  character(*   ),intent(in   ),optional::tag


                  if(present(tag)) call write     (unit,tag )
                                   call write_seed(unit)


        end subroutine print_seed!unit,tag


  end module random_number_generator


#  endif
