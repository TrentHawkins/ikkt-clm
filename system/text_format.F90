#     ifndef SYSTEM_TEXT_FORMAT_F90
#     define SYSTEM_TEXT_FORMAT_F90


      module text_formatting


            implicit none


            character(2),parameter::escape0x1B=achar(27)//'['
            character(1),parameter::terminator=           'm'

            character(4),parameter::t_normal   =escape0x1B// "0"//terminator
            character(4),parameter::t_bold     =escape0x1B// "1"//terminator
            character(4),parameter::t_italics  =escape0x1B// "3"//terminator
            character(4),parameter::t_underline=escape0x1B// "4"//terminator
            character(4),parameter::t_blink    =escape0x1B// "5"//terminator
            character(4),parameter::t_highlight=escape0x1B// "7"//terminator
            character(4),parameter::t_hide     =escape0x1B// "8"//terminator

            character(5),parameter::s_bold     =escape0x1B//"22"//terminator
            character(5),parameter::s_italics  =escape0x1B//"23"//terminator
            character(5),parameter::s_underline=escape0x1B//"24"//terminator
            character(5),parameter::s_blink    =escape0x1B//"25"//terminator
            character(5),parameter::s_highlight=escape0x1B//"27"//terminator
            character(5),parameter::s_hide     =escape0x1B//"28"//terminator

            character(5),parameter::t_black    =escape0x1B//"30"//terminator
            character(5),parameter::t_red      =escape0x1B//"31"//terminator
            character(5),parameter::t_green    =escape0x1B//"32"//terminator
            character(5),parameter::t_yellow   =escape0x1B//"33"//terminator
            character(5),parameter::t_blue     =escape0x1B//"34"//terminator
            character(5),parameter::t_purple   =escape0x1B//"35"//terminator
            character(5),parameter::t_cyan     =escape0x1B//"36"//terminator
            character(5),parameter::t_white    =escape0x1B//"37"//terminator


            interface write

               module procedure write_tag
               module procedure write_par

        end interface write


      contains


            subroutine write_tag(unit,text)


                  implicit none


                  integer     ,intent(inout)::unit
                  character(*),intent(in   )::text

                  integer::text_width


                  text_width=len(trim(text))

                  write(unit,"(a<text_width>)",advance="no") text


        end subroutine write_tag!unit,text


            subroutine write_par(unit,text_width,text)


                  implicit none


                  integer     ,intent(inout)::unit
                  integer     ,intent(in   )::text_width
                  character(*),intent(in   )::text

                  integer::c0
                  integer::c1

                  integer::text_length


                  text_length=len(text)

                  c0=1

                  do

                     if(c0>text_length) then

                        exit

              end    if!c0>l

!                    print at maximum <text_width> characters

                     c1=min(c0+text_width,text_length)

!                    if not at the end of the whole string

                     if(c1/=text_length) then

!                       find the end of a word

                        do

                           if(text(c1:c1)==" ") then

                              exit

              end          if!text(c1:c1)==" "

                           c1=c1-1

              end       do

              end    if!c1/=text_length

                     write(unit,"(2x,a)") text(c0:c1-1)
                                               c0=c1+1

              end do


        end subroutine write_par!unit,text_width,text


  end module text_formatting


#  endif
