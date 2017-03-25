#     ifndef MONTE_CARLO_RANDOM_NUMBER_GENERATOR_F90
#     define MONTE_CARLO_RANDOM_NUMBER_GENERATOR_F90

#     include "precision.F90"


      module random_number_generator


            implicit none


            character(*),parameter::           format_integer=INTEGERG2
            character(*),parameter::text_field_format_integer=INTEGERA2

            integer            ,private::       size_seed
            integer,allocatable,private::seed( :         )


      contains


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


        end subroutine prepare_seed


            subroutine make_seed()


                  implicit none


                  call random_seed()


        end subroutine make_seed


            subroutine load_seed(file_name)


                  implicit none


                  character(*),intent(in   )::file_name

                  integer::unit


                  call prepare_seed()

                   open(newunit=unit,file=file_name)
                   read(        unit,format_integer) seed
                  close(        unit)

                  call random_seed(put=seed)

                  deallocate(seed)


        end subroutine load_seed!file_name


            subroutine save_seed(file_name)


                  implicit none


                  character(*),intent(in   )::file_name

                  integer::unit


                  call prepare_seed()

                  call random_seed(get=seed)

                   open(newunit=unit,file=file_name)
                  write(        unit,format_integer) seed
                  close(        unit)

                  deallocate(seed)


        end subroutine save_seed!unit,file_name


  end module random_number_generator


#  endif