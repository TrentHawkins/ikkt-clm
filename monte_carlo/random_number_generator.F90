#     ifndef RANDOM_NUMBER_GENERATOR_F90
#     define RANDOM_NUMBER_GENERATOR_F90

#     include "../system/precision.F90"


      module random_number_generator


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


      contains


            subroutine make_seed()


                  implicit none


                  call random_seed()


        end subroutine make_seed!


            subroutine load_seed(file_name)


                  implicit none


                  integer::unit


                  call prepare_seed()
                  call random_seed(put=seed)

                   open(newunit=unit,file=seed_file_name)

                   read(unit,          *)
                   read(unit,          *)
                   read(unit,          *)
                   read(unit,format_seed) seed

                  close(        unit                    )

                  if(allocated(seed)) deallocate(seed)


        end subroutine load_seed!


            subroutine save_seed()


                  implicit none


                  integer::unit


                  call prepare_seed()
                  call random_seed(get=seed)

                   open(newunit=unit,file=seed_file_name)

                  write(unit,          *) "LAST RANDOM NUMBER GENERATOR SEED"
                  write(unit,          *) "___________","___________","___________","___________","___________","___________",&
                                          "___________","___________","___________","___________","___________","___________"
                  write(unit,          *)
                  write(unit,format_seed) seed

                  close(        unit                    )

                  if(allocated(seed)) deallocate(seed)


        end subroutine save_seed!


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


  end module random_number_generator


#  endif
