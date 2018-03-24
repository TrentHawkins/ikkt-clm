#     ifndef AUTOSAVE_F90
#     define AUTOSAVE_F90

#     include "../system/precision.F90"
#     include "../system/text_format.F90"

#     include "../monte_carlo/monte_carlo.F90"

#     include "../ikkt/fields.F90"


      module autosaving


            use::text_formatting

            use::monte_carlo

            use::fields


            implicit none


            logical,private::old_field_preloading

            character(:),allocatable,public::auto_file_name
            integer                 ,public::auto_unit

            public::autosave_field
            public::autoload_field


      contains


            subroutine autoload_field(unit,iostat)


                  implicit none


                  integer,intent(inout)::unit
                  integer,intent(  out)::iostat


                  call  read(unit,auto,iostat=iostat);  read(unit,*,iostat=iostat,end=120)
                                                        read(unit,*,iostat=iostat,end=120)

                  call  read_field(unit)

              120 return


        end subroutine autoload_field!unit,iostat


            subroutine autosave_field(unit,iostat)


                  implicit none


                  integer,intent(inout)::unit


                  call write(unit,auto,iostat=iostat); write(unit,*,iostat=iostat,end=121)
                                                       write(unit,*,iostat=iostat,end=121)

                  call write_field(unit,iostat=iostat)

              121 return


        end subroutine autosave_field!unit,iostat


            subroutine calculate_observables()


                  implicit none


                  do while


        end subroutine calculate_observables!


  end module autosaving


#  endif
