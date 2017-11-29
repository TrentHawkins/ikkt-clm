# 1 "main/main.F90"
!     ifort v2017.4

!     without BLAS:

!     ifort -module "ifort/modules" -fast -w main/main.F90
!     -mkl=sequential -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl
!
!     with BLAS:
!
!     ifort -module "ifort/modules" -fast -w -DBLAS main/main.F90
!     -mkl=sequential -lmkl_blas95_lp64 -lmkl_lapack95_lp64 -lmkl_core -lpthread -lm -ldl

!     You may need the option "-mcmodel=medium" if the stored arrays are too large, which conflicts with the "-fast" option.
!     Later when the optimized version using commutators is fixed, it will not be necessary, as the fermion matrix is bypassed.

# 1 "main/../main/mathematical_constants.F90" 1



# 1 "main/../system/precision.F90" 1





!                                             +127 i04.03
!                                           +32767 i06.05
!                                      +2147483647 i11.10
!                             +9223372036854775807 i20.19
!         +170141183460469231731687303715884105727 i40.39













!                                     +__.__       f06.04
!                                    +._____e+_    e10.05e1
!                                +.340282347e+39   e15.09e2
!                        +.17976931348623157e+309  e24.17e3
!     +.118973149535723176508575932662800702e+4933 e44.36e4














































# 5 "main/../main/mathematical_constants.F90" 2


      module mathematical_constants


            implicit none


            complex(08),parameter,public::   zero=(+.00000e+0_08,&
                                                   +.00000e+0_08)
            complex(08),parameter,public::re_unit=(+.10000e+1_08,&
                                                   +.00000e+0_08)
            complex(08),parameter,public::im_unit=(+.00000e+0_08,&
                                                   +.10000e+1_08)

            real(08),parameter,public::pi    =acos(-.10000e+1_08)


            contains


            function log2(x) result(y)


               implicit none


               real(08),intent(inout)::x
               real(08)              ::y


               y=log10(x)/log10(.20000e+1_08)


        end function log2!x


  end module mathematical_constants


# 17 "main/main.F90" 2

# 1 "main/../system/getopts.F90" 1



# 1 "main/../system/text_format.F90" 1




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


      contains


            subroutine write_par(unit,text_width,text)


                  implicit none


                  integer     ,intent(inout)::unit
                  integer     ,intent(inout)::text_width
                  character(*),intent(inout)::text

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



# 5 "main/../system/getopts.F90" 2


      module get_options


            use::text_formatting


            implicit none


            integer,parameter::len_name   =  32
            integer,parameter::len_descr  =1024
            integer,parameter::len_argname=  32

            type option

               character(len_name)::name ! Long name.
               logical::has_arg ! Does the option require an argument?
               character::chr ! Corresponding short name.
               character(len_descr)::descr ! Description.
               character(len_argname)::argname ! Argument name,if required.

            contains

               procedure::print=>print_opt

        end type


      contains


!     Parse command line options. Options and their arguments must come before all non-option arguments. Short options have the form
!     "-X", long options have the form "--XXXX..." where "X" is any character. Parsing can be stopped with the option "--".
!     The following code snippet illustrates the intended use:

!           do
!              call getopt (...,optchar=c,...)
!              if(stat/=0) then
!     !           optional error handling
!                 exit
!       end    if
!              select case (c)
!     !           process options
!       end    select
!       end do


            subroutine getopt(options,longopts,optchar,optarg,arglen,stat,offset,remain,err)


                  use::iso_fortran_env,only:error_unit


                  implicit none


!                 String containing the characters that are valid short options. If present, command line arguments are scanned for
!                 those options. If a character is followed by a colon (:) its corresponding option requires an argument.
!                 E.g. "vn:" defines two options -v and -n with -n requiring an argument.

                  character(*),intent(in),optional::options

!                 Array of long options. If present,options of the form "--XXXX..." are recognised. Each option has an associated
!                 option character. This can be any character of default kind,it is just an identifier. It can, but doesn"t have to,
!                 match any character in the options argument. In fact itis possible to only pass long options and no short options
!                 at all. Only name,has_arg and chr need to be set.

                  type(option),intent(in),optional::longopts(:)

!                 If stat is not 1,optchar contains the option character that was parsed. Otherwise its value is undefined.

                  character,intent(out),optional::optchar

!                 If stat is 0 and the parsed option requires an argument,optarg contains the first len(optarg) (but at most
!                 len_descr) characters of that argument. Otherwise its value is undefined. If the arguments length exceeds
!                 len_descr characters and err is .true.,a warning is issued.

                  character(*),intent(out),optional::optarg

!                 If stat is 0 and the parsed option requires an argument,arglen contains the actual length of that argument
!                 Otherwise its value is undefined. This can be used to make sure the argument was not truncated by the limited
!                 length of optarg.

                  integer,intent(out),optional::arglen

!                 Status indicator. Can have the following values:
!                  0: An option was successfully parsed.
!                  1: Parsing stopped successfully because a non-option or "--" was encountered.
!                 -1: An unrecognised option was encountered.
!                 -2: A required argument was missing.
!                 Its value is never undefined.

                  integer,intent(out),optional::stat

!                 If stat is 1, offset contains the number of the argument before the first non-option argument, i.e. offset+n is
!                 the nth non-option argument. If stat is not 1, offset contains the number of the argument that would be parsed in
!                 the next call to getopt. This number can be greater than the actual number of arguments.

                  integer,intent(out),optional::offset

!                 If stat is 1, remain contains the number of remaining non-option arguments, i.e. the non-option arguments are in
!                 the range (offset+1:offset+remain). If stat is not 1, remain is undefined.

                  integer,intent(out),optional::remain

!                 If err is present and .true., getopt prints messages to the standard error unit if an error is encountered
!                 (i.e. whenever stat would be set to a negative value).

                  logical,intent(in),optional::err

                  integer,save::pos=1
                  integer,save::cnt=0
                  character(len_descr),save::arg

                  integer::chrpos
                  integer::length
                  integer::st
                  integer::id
                  character::chr
                  logical::long


                  if(cnt==0) then

                     cnt=command_argument_count()

              end if!cnt==0

                  long=.false.

!                 no more arguments left

                  if(pos>cnt) then

                     pos=pos-1
                     st=1
                     goto 10

              end if!pos>cnt

                  call get_command_argument(pos,arg,length)

!                 is argument an option?

                  if(arg(1:1)=="-") then

                     chr=arg(2:2)

!                 too long ("-xxxx...") for one dash?

                     if(chr/="-".and.len_trim(arg)>2) then

                        st=-1
                        goto 10

              end    if!chr/="-".and.len_trim(arg)>2

!                    forced stop ("--")

                     if(chr=="-".and.arg(3:3)==" ") then

                        st=1
                        goto 10

              end    if!chr=="-".and.arg(3:3)==" "

!                    long option ("--xxx...")

                     if(chr=="-") then

                        long=.true.

!                       check if valid

                        id=lookup(arg(3:))

!                       option is invalid, stop

                        if(id==0) then

                           st=-1
                           goto 10

              end       if!id==0

                        chr=longopts(id)%chr

!                       check if option requires an argument

                        if(.not.longopts(id)%has_arg) then

                           st=0
                           goto 10

              end       if!.not.longopts(id)%has_arg

!                       check if there are still arguments left

                        if(pos==cnt) then

                           st=-2
                           goto 10

              end       if!pos==cnt

!                       go to next position

                        pos=pos+1

!                       get argument

                        call get_command_argument(pos,arg,length)

!                       make sure it is not an option

                        if(arg(1:1)=="-") then

                           st=-2
                           pos=pos-1
                           goto 10

              end       if!arg(1:1)=="-"

              end    if!chr=="-"

!                    short option

!                    check if valid

                     if(present(options)) then

                        chrpos=scan(options,chr)

                     else

                        chrpos=0

              end    if!present(options)

!                    option is invalid, stop

                     if(chrpos==0) then

                        st=-1
                        goto 10

              end    if!chrpos==0

!                    look for argument requirement

                     if(chrpos<len_trim(options)) then

                        if(options(chrpos+1:chrpos+1)==":") then

!                          check if there are still arguments left

                           if(pos==cnt) then

                              st=-2
                              goto 10

              end          if!pos==cnt

!                          go to next position

                           pos=pos+1

!                          get argument

                           call get_command_argument(pos,arg,length)

!                          make sure it is not an option

                           if(arg(1:1)=="-") then

                              st=-2
                              pos=pos-1
                              goto 10

              end          if!arg(1:1)=="-"

              end       if!options(chrpos+1:chrpos+1)==":"

              end    if!chrpos<len_trim(options)

!                    if we get to this point, no error happened
!                    return option and the argument (if there is one)

                     st=0
                     goto 10

              end if!arg(1:1)=="-"

!                 not an option, parsing stops

                  st=1

!                 we are already at the first non-option argument
!                 go one step back to the last option or option argument

                  pos=pos-1

!                 error handling and setting of return values

   10             continue

                  if(present(err)) then

                     if(err) then

                        select case(st)

                               case(-1)

                           write (error_unit,*) "error: unrecognised option: "//trim(arg)

                               case(-2)

                           if(.not.long) then

                              write (error_unit,*) "error: option -"//chr//" requires an argument"

                           else

                              write (error_unit,*) "error: option --"//trim(longopts(id)%name)//" requires an argument"

              end          if!.not.long

              end       select!case(st)

              end    if!err

              end if!present(err)

                  if(present(optchar)) then

                     optchar=chr

              end if!present(optchar)

                  if(present(optarg)) then

                     optarg=arg

              end if!present(optarg)

                  if(present(arglen)) then

                     arglen=length

              end if!present(arglen)

                  if(present(stat)) then

                     stat=st

              end if!present(stat)

                  if(present(offset)) then

                     offset=pos

              end if!present(offset)

                  if(present(remain)) then

                     remain=cnt-pos

              end if!present(remain)

!                 setup pos for next call to getopt

                  pos=pos+1


            contains


                  integer function lookup(name)


                        implicit none


                        character(*),intent(in)::name
                        integer::i

!                       if there are no long options,skip the loop

                        if(.not.present(longopts)) then

                           goto 20

                    end if!.not.present(longopts)

                        do i=1,size(longopts)

                           if(name==longopts(i)%name) then

                              lookup=i
                              return

                    end    if!name==longopts(i)%name

                    end do!i=1,size(longopts)

!                       if we get to this point,the option was not found

   20                   lookup=0


              end function


        end subroutine


!           Print an option in the style of a man page.
!           -o arg, --option arg  [description]


            subroutine print_opt(opt,unit)


                  implicit none


!                 the option

                  class(option),intent(in)::opt

!                 logical unit number

                  integer,intent(in)::unit
                  integer::l,c1,c2,line_count,&
                                   name_width,&
                                   side_width,&
                                   term_width

                  if(opt%has_arg) then

                     write(unit,"(2x, '-',a,','  )",advance="no")      opt%chr
                     write(unit,"(   '--',a,'=',a)",advance="no") trim(opt%name)   ,&
                                                                  trim(opt%argname)

                     name_width=len_trim(opt%chr    )&
                               +len_trim(opt%name   )&
                               +len_trim(opt%argname)+7

                  else

                     write(unit,"(2x, '-',a,',')",advance="no")      opt%chr
                     write(unit,"(   '--',a    )",advance="no") trim(opt%name)

                     name_width=len_trim(opt%chr    )&
                               +len_trim(opt%name   )+6

              end if!opt%has_arg

                  side_width=32
                  term_width=80

                  if(name_width<side_width) then

                     write(unit,"(<side_width-name_width>x)",advance="no")
                                   name_width=side_width

                  else

                     write(unit,"(2x)",advance="no")

              end if!name_width<side_width

                  l=len_trim(opt%descr)

!                 c1 is the first character of the line
!                 c2 is one past the last character of the line

                  c1=1

                  line_count=1

                  do

                     if(c1>l) then

                        exit

              end    if!c1>l

!                    print at maximum <term_width> characters

                     if(line_count>1) then

                        write(unit,"(<side_width>x)",advance="no")

                        c2=min(c1+term_width  &
                                 -side_width  ,len_descr)

                     else

                        c2=min(c1+term_width  &
                                 -name_width-2,len_descr)

              end    if!line_count>1

!                    if not at the end of the whole string

                     if(c2/=len_descr) then

!                       find the end of a word

                        do

                           if(opt%descr(c2:c2)==" ") then

                              exit

              end          if!opt%descr(c2:c2)==" "

                           c2=c2-1

              end       do

              end    if!c2/=len_descr

                     write(unit,"(a)") opt%descr(c1:c2-1)
                                                 c1=c2+1
                     line_count&
                    =line_count+1

              end do

                  write(unit,*)


        end subroutine


  end module get_options



# 19 "main/main.F90" 2
# 1 "main/../system/signals.F90" 1



!            SIGHUP     Term    Hangup detected on controlling terminal or death of controlling process
!            SIGINT     Term    Interrupt from keyboard
!            SIGQUIT    Core    Quit from keyboard
!            SIGILL     Core    Illegal Instruction
!            SIGABRT    Core    Abort signal from abort(3)
!            SIGFPE     Core    Floating-point exception
!            SIGKILL    Term    Kill signal
!            SIGSEGV    Core    Invalid memory reference
!            SIGPIPE    Term    Broken pipe: write to pipe with noreaders; see pipe(7)












      subroutine signal_actions(handler)


            implicit none


            external::handler


!           call signal(1,handler)
            call signal(2,handler)
!           call signal(8,handler)

!           call signal(3,handler)
!           call signal(6,handler)


  end subroutine signal_actions!handler



# 20 "main/main.F90" 2
# 1 "main/../system/text_format.F90" 1
# 106

# 21 "main/main.F90" 2
# 1 "main/../system/precision.F90" 1
# 74

# 22 "main/main.F90" 2
# 1 "main/../system/version.F90" 1



# 1 "main/../system/text_format.F90" 1
# 106

# 5 "main/../system/version.F90" 2


      module version


            use::text_formatting


            implicit none


            character(15),parameter,private::this="v2.0"
            character(15),          private::that

            public:: read_version
            public::write_version


      contains


            subroutine  read_version()


                  implicit none


                  integer::unit
                  integer::stat


                   open(newunit=unit,file="ifort/bin/version",status="old",iostat=stat)
                   read(        unit,                                "(a)"            ) that
                  close(        unit                                                  )

                  if(that/=this) stop t_red//"Error: cannot load files generated by incompatible version of the program"//t_normal
                  if(stat/=0000) stop t_yellow//"Warning: when running the program for the first time, no"//t_bold//"-c"//t_normal


        end subroutine  read_version!


            subroutine write_version()


                  implicit none


                  integer::unit


                   open(newunit=unit,file="ifort/bin/version"             )
                  write(        unit,                                "(a)") this
                  close(        unit                                      )


        end subroutine write_version!


  end module version



# 23 "main/main.F90" 2

!     include "../tensor/tensor.F90"

!     include "../monte_carlo/random_number_generator.F90"
!     include "../monte_carlo/average.F90"
!     include "../monte_carlo/time.F90"
# 1 "main/../monte_carlo/monte_carlo.F90" 1



# 1 "main/../system/precision.F90" 1
# 74

# 5 "main/../monte_carlo/monte_carlo.F90" 2
# 1 "main/../monte_carlo/../system/text_format.F90" 1
# 106

# 6 "main/../monte_carlo/monte_carlo.F90" 2

# 1 "main/../monte_carlo/../monte_carlo/random_number_generator.F90" 1



# 1 "main/../monte_carlo/../system/precision.F90" 1
# 74

# 5 "main/../monte_carlo/../monte_carlo/random_number_generator.F90" 2


      module random_number_generator


            implicit none


            character(*),private,parameter::           format_integer="(sp,  x,i11.10   )"
            character(*),private,parameter::text_field_format_integer="(     x,a11      )"

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


            subroutine load_seed()


                  implicit none


                  integer::unit


                  call prepare_seed()

                   open(newunit=unit,file=seed_file_name)

                   read(unit,             *)
                   read(unit,             *)
                   read(unit,             *)
                   read(unit,format_integer) seed

                  close(        unit                    )

                  if(.not.allocated(seed)) stop "NOPUT"; call random_seed(put=seed)

                  if(allocated(seed)) deallocate(seed)


        end subroutine load_seed!


            subroutine save_seed()


                  implicit none


                  integer::unit


                  call prepare_seed()

                  if(.not.allocated(seed)) stop "NOGET"; call random_seed(get=seed)

                   open(newunit=unit,file=seed_file_name)

                  write(unit,             *) "LAST RANDOM NUMBER GENERATOR SEED"
                  write(unit,             *) "___________","___________","___________","___________","___________","___________",&
                                             "___________","___________","___________","___________","___________","___________"
                  write(unit,             *)
                  write(unit,format_integer) seed

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



# 8 "main/../monte_carlo/monte_carlo.F90" 2
# 1 "main/../monte_carlo/../monte_carlo/average.F90" 1



# 1 "main/../monte_carlo/../system/precision.F90" 1
# 74

# 5 "main/../monte_carlo/../monte_carlo/average.F90" 2


      module average_type


            implicit none


            character(*),private,parameter::           format_average_K="(sp,  x,e24.17e3 )"
            character(*),private,parameter::text_field_format_average_K="(     x,a24      )"

            real(08),private,parameter::average_tolerance=.00000000001

            type::average(precision)

               integer,kind::precision

               real(precision),private::m_weight=+.00000e+0_08
               real(precision),private::m_value =+.00000e+0_08

            contains

               procedure,public::average_set_K;generic::average=>average_set_K
               procedure,public:: weight_set_K;generic:: weight=> weight_set_K
               procedure,public::  value_set_K;generic::  value=>  value_set_K

               procedure,private::                is_equal_to_K;generic::                is_equal_to=>                is_equal_to_K
               procedure,private::   is_less_than_or_equal_to_K;generic::   is_less_than_or_equal_to=>   is_less_than_or_equal_to_K
               procedure,private::               is_less_than_K;generic::               is_less_than=>               is_less_than_K
               procedure,private::            is_not_equal_to_K;generic::            is_not_equal_to=>            is_not_equal_to_K
               procedure,private::is_greater_than_or_equal_to_K;generic::is_greater_than_or_equal_to=>is_greater_than_or_equal_to_K
               procedure,private::            is_greater_than_K;generic::            is_greater_than=>            is_greater_than_K

        end type  average!precision


            interface average

               module procedure average_constructor_K

        end interface average

            interface  weight

               module procedure  weight_get_K

        end interface  weight

            interface   value

               module procedure   value_get_K

        end interface   value

            interface assignment(=)

               module procedure average_real_assignment_K

        end interface assignment(=)

            interface operator(+)

               module procedure real_average_average_plus_K
               module procedure average_real_average_plus_K

               module procedure average_average_average_plus_K
               module procedure         average_average_plus_K

        end interface operator(+)

            interface operator(-)

               module procedure real_average_average_minus_K
               module procedure average_real_average_minus_K

               module procedure average_average_average_minus_K
               module procedure         average_average_minus_K

        end interface operator(-)

            interface sum

               module procedure average_sum_K

        end interface sum

            interface operator(*)

               module procedure real_average_average_times_value_K
               module procedure average_real_average_times_value_K
               module procedure average_average_real_times_value_K

        end interface operator(*)

            interface operator(/)

               module procedure average_real_average_division_by_K
               module procedure average_average_real_division_by_K

        end interface operator(/)

            interface product

               module procedure average_product_K

        end interface product

            interface operator(==)

               module procedure real_average_equal_to_K
               module procedure average_real_equal_to_K

               module procedure average_average_equal_to_K

        end interface operator(==)

            interface operator(<=)

               module procedure real_average_less_than_or_equal_to_K
               module procedure average_real_less_than_or_equal_to_K

               module procedure average_average_less_than_or_equal_to_K

        end interface operator(<=)

            interface operator(< )

               module procedure real_average_less_than_K
               module procedure average_real_less_than_K

               module procedure average_average_less_than_K

        end interface operator(< )

            interface operator(/=)

               module procedure real_average_not_equal_to_K
               module procedure average_real_not_equal_to_K

               module procedure average_average_not_equal_to_K

        end interface operator(/=)

            interface operator(>=)

               module procedure real_average_greater_than_or_equal_to_K
               module procedure average_real_greater_than_or_equal_to_K

               module procedure average_average_greater_than_or_equal_to_K

        end interface operator(>=)

            interface operator(> )

               module procedure real_average_greater_than_K
               module procedure average_real_greater_than_K

               module procedure average_average_greater_than_K

        end interface operator(> )

            interface  read

               module procedure  read_average_K

        end interface  read

            interface write

               module procedure write_average_K

        end interface write


      contains


            subroutine average_set_K(this,weight,value)


                  implicit none


                  type(average(08)),intent(inout)         ::this
                  real(        08 ),intent(inout),optional::weight
                  real(        08 ),intent(inout),optional::value


                  if(present(weight)) then

                     this%m_weight=weight

                     if(present(value)) then

                        this%m_value=value

              end    if!present(value)

              end if!present(weight)


        end subroutine average_set_K!this,weight,value


            subroutine weight_set_K(this,weight)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 )              ::weight


                  this%m_weight=weight


        end subroutine weight_set_K!this,weight


            subroutine value_set_K(this,value)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 )              ::value


                  this%m_value=value


        end subroutine value_set_K!this,value


            function average_constructor_K(weight,value) result(that)


                  implicit none


                  real(        08 ),intent(inout),optional::weight
                  real(        08 ),intent(inout),optional::value
                  type(average(08))                       ::that


                  if(present(weight)) then

                     if(present(value)) then

                        call that%average(weight,value)

                     else

                        call that%average(weight)

              end    if!present(value)

                  else

                     call that%average()

              end if!present(weight)


        end function average_constructor_K!weight,value


            function weight_get_K(this) result(weight)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 )              ::weight


                  weight=this%m_weight


        end function weight_get_K!this


            function value_get_K(this) result(value)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 )              ::value


                  value=this%m_value


        end function value_get_K!this


            subroutine average_real_assignment_K(average0,real1)


               implicit none


               type(average(08)),intent(inout)::average0
               real(        08 ),intent(inout)::   real1


               average0%m_weight=real1


        end subroutine average_real_assignment_K!average0,real1


            function real_average_average_plus_K(real1,average2) result(average_)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2
                  type(average(08))              ::average_


                  average_%m_weight=real1&
                 +average2%m_weight


        end function real_average_average_plus_K!real1,average2


            function average_real_average_plus_K(average1,real2) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2
                  type(average(08))              ::average_


                  average_%m_weight&
                 =average1%m_weight+real2


        end function average_real_average_plus_K!average1,real2


            function average_average_average_plus_K(average1,average2) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2
                  type(average(08))              ::average_


                  average_%m_weight&
                 =average1%m_weight&
                 +average2%m_weight

                  average_%m_value &
                 =average1%m_weight&
                 *average1%m_value &
                 +average2%m_weight&
                 *average2%m_value

                  average_%m_value &
                 =average_%m_value &
                 /average_%m_weight


        end function average_average_average_plus_K!average1,average2


            function average_average_plus_K(average0) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average0
                  type(average(08))              ::average_


                  average_%m_weight&
                =+average0%m_weight

                  average_%m_value &
                =+average0%m_value


        end function average_average_plus_K!average0


            function real_average_average_minus_K(real1,average2) result(average_)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2
                  type(average(08))              ::average_


                  average_%m_weight=real1&
                 -average2%m_weight


        end function real_average_average_minus_K!real1,average2


            function average_real_average_minus_K(average1,real2) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2
                  type(average(08))              ::average_


                  average_%m_weight&
                 =average1%m_weight-real2


        end function average_real_average_minus_K!average1,real2


            function average_average_average_minus_K(average1,average2) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2
                  type(average(08))              ::average_


                  average_%m_weight&
                 =average1%m_weight&
                 -average2%m_weight

                  average_%m_value &
                 =average1%m_weight&
                 *average1%m_value &
                 -average2%m_weight&
                 *average2%m_value

                  average_%m_value &
                 =average_%m_value &
                 /average_%m_weight


        end function average_average_average_minus_K!average1,average2


            function average_average_minus_K(average0) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average0
                  type(average(08))              ::average_


                  average_%m_weight&
                =-average0%m_weight

                  average_%m_value &
                =+average0%m_value


        end function average_average_minus_K!average0


            function average_sum_K(average0) result(average_)


                  implicit none


                  type(average(08)),dimension(:),intent(inout)::average0
                  type(average(08))                           ::average_

                      average_%m_weight&
                 =sum(average0%m_weight)

                      average_%m_value &
                 =sum(average0%m_weight&
                 *    average0%m_value)

                      average_%m_value &
                 =    average_%m_value &
                 /    average_%m_weight


        end function average_sum_K!average0


            function real_average_average_times_value_K(real1,average2) result(average_)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2
                  type(average(08))              ::average_


                  average_%m_weight=real1&
                 *average2%m_weight

                  average_%m_value &
                 =average2%m_value


        end function real_average_average_times_value_K!real1,average2


            function average_real_average_times_value_K(average1,real2) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2
                  type(average(08))              ::average_


                  average_%m_weight&
                 =average1%m_weight*real2

                  average_%m_value &
                 =average1%m_value


        end function average_real_average_times_value_K!average1,real2


            function average_average_real_times_value_K(average1,average2) result(real_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2
                  real(        08 )              ::   real_


                  real_=average1%m_value&
                       *average2%m_value


        end function average_average_real_times_value_K!average1,average2


            function average_real_average_division_by_K(average1,real2) result(average_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2
                  type(average(08))              ::average_


                  average_%m_weight&
                 =average1%m_weight/real2

                  average_%m_value &
                 =average1%m_value


        end function average_real_average_division_by_K!average1,real2


            function average_average_real_division_by_K(average1,average2) result(real_)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2
                  real(        08 )              ::   real_


                 real_=average1%m_value&
                      /average2%m_value


        end function average_average_real_division_by_K!average1,average2


            function average_product_K(average0) result(real_)


                  implicit none


                  type(average(08)),dimension(:),intent(inout)::average0
                  real(        08 )                           ::   real_


                  real_=product(average0%m_value)


        end function average_product_K!average0


          function is_equal_to_K(this,weight) result(is_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 ),intent(inout)::weight

                  logical::is_equal_to


                  if(abs(this%m_weight-weight)<average_tolerance) then

                     is_equal_to=.true.

                  else

                     is_equal_to=.false.

              end if!abs(this%m_weight-weight)<average_tolerance


        end function is_equal_to_K!this,weight


            function real_average_equal_to_K(real1,average2) result(real_average_equal_to)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2

                  logical::real_average_equal_to


                  real_average_equal_to=average2%is_equal_to(real1)


        end function real_average_equal_to_K!real1,average2


            function average_real_equal_to_K(average1,real2) result(average_real_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2

                  logical::average_real_equal_to


                  average_real_equal_to=average1%is_equal_to(real2)


        end function average_real_equal_to_K!average1,real2


            function average_average_equal_to_K(average1,average2) result(average_average_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2

                  logical::average_average_equal_to


                  if(abs(average1%m_weight-average2%m_weight)<average_tolerance) then

                     average_average_equal_to=.true.

                  else

                     average_average_equal_to=.false.

              end if!abs(average1%m_weight-average2%m_weight)<average_tolerance


        end function average_average_equal_to_K!average1,average2


            function is_less_than_or_equal_to_K(this,weight) result(is_less_than_or_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 ),intent(inout)::weight

                  logical::is_less_than_or_equal_to


                  if(this%m_weight<=weight+average_tolerance) then

                     is_less_than_or_equal_to=.true.

                  else

                     is_less_than_or_equal_to=.false.

              end if!this%m_weight<weight+average_tolerance


        end function is_less_than_or_equal_to_K!this,weight


            function real_average_less_than_or_equal_to_K(real1,average2) result(real_average_less_than_or_equal_to)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2

                  logical::real_average_less_than_or_equal_to


                  real_average_less_than_or_equal_to=average2%is_less_than_or_equal_to(real1)


        end function real_average_less_than_or_equal_to_K!real1,average2


            function average_real_less_than_or_equal_to_K(average1,real2) result(average_real_less_than_or_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2

                  logical::average_real_less_than_or_equal_to


                  average_real_less_than_or_equal_to=average1%is_less_than_or_equal_to(real2)


        end function average_real_less_than_or_equal_to_K!average1,real2


            function average_average_less_than_or_equal_to_K(average1,average2) result(average_average_less_than_or_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2

                  logical::average_average_less_than_or_equal_to


                  if(average1%m_weight<=average2%m_weight+average_tolerance) then

                     average_average_less_than_or_equal_to=.true.

                  else

                     average_average_less_than_or_equal_to=.false.

              end if!average1%m_weight<average2%m_weight+average_tolerance


        end function average_average_less_than_or_equal_to_K!average1,average2


            function is_less_than_K(this,weight) result(is_less_than)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 ),intent(inout)::weight

                  logical::is_less_than


                  if(this%m_weight<=weight-average_tolerance) then

                     is_less_than=.true.

                  else

                     is_less_than=.false.

              end if!this%m_weight<=weight-average_tolerance


        end function is_less_than_K!this,weight


            function real_average_less_than_K(real1,average2) result(real_average_less_than)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2

                  logical::real_average_less_than


                  real_average_less_than=average2%is_less_than(real1)


        end function real_average_less_than_K!real1,average2


            function average_real_less_than_K(average1,real2) result(average_real_less_than)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2

                  logical::average_real_less_than


                  average_real_less_than=average1%is_less_than(real2)


        end function average_real_less_than_K!average1,real2


            function average_average_less_than_K(average1,average2) result(average_average_less_than)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2

                  logical::average_average_less_than


                  if(average1%m_weight<=average2%m_weight-average_tolerance) then

                     average_average_less_than=.true.

                  else

                     average_average_less_than=.false.

              end if!average1%m_weight<=average2%m_weight-average_tolerance


        end function average_average_less_than_K!average1,average2


            function is_not_equal_to_K(this,weight) result(is_not_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 ),intent(inout)::weight

                  logical::is_not_equal_to


                  if(abs(this%m_weight-weight)>=average_tolerance) then

                     is_not_equal_to=.true.

                  else

                     is_not_equal_to=.false.

              end if!abs(this%m_weight-weight)>=average_tolerance


        end function is_not_equal_to_K!this,weight


            function real_average_not_equal_to_K(real1,average2) result(real_average_not_equal_to)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2

                  logical::real_average_not_equal_to


                  real_average_not_equal_to=average2%is_not_equal_to(real1)


        end function real_average_not_equal_to_K!real1,average2


            function average_real_not_equal_to_K(average1,real2) result(average_real_not_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2

                  logical::average_real_not_equal_to


                  average_real_not_equal_to=average1%is_not_equal_to(real2)


        end function average_real_not_equal_to_K!average1,real2


            function average_average_not_equal_to_K(average1,average2) result(average_average_not_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2

                  logical::average_average_not_equal_to


                  if(abs(average1%m_weight-average2%m_weight)>=average_tolerance) then

                     average_average_not_equal_to=.true.

                  else

                     average_average_not_equal_to=.false.

              end if!abs(average1%m_weight-average2%m_weight)>=average_tolerance


        end function average_average_not_equal_to_K!average1,average2


            function is_greater_than_or_equal_to_K(this,weight) result(is_greater_than_or_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 ),intent(inout)::weight

                  logical::is_greater_than_or_equal_to


                  if(this%m_weight>=weight-average_tolerance) then

                     is_greater_than_or_equal_to=.true.

                  else

                     is_greater_than_or_equal_to=.false.

              end if!this%m_weight>=weight-average_tolerance


        end function is_greater_than_or_equal_to_K!this,weight


            function real_average_greater_than_or_equal_to_K(real1,average2) result(real_average_greater_than_or_equal_to)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2

                  logical::real_average_greater_than_or_equal_to


                  real_average_greater_than_or_equal_to=average2%is_greater_than_or_equal_to(real1)


        end function real_average_greater_than_or_equal_to_K!real1,average2


            function average_real_greater_than_or_equal_to_K(average1,real2) result(average_real_greater_than_or_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2

                  logical::average_real_greater_than_or_equal_to


                  average_real_greater_than_or_equal_to=average1%is_greater_than_or_equal_to(real2)


        end function average_real_greater_than_or_equal_to_K!average1,real2


            function average_average_greater_than_or_equal_to_K(average1,average2) result(average_average_greater_than_or_equal_to)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2

                  logical::average_average_greater_than_or_equal_to


                  if(average1%m_weight>=average2%m_weight-average_tolerance) then

                     average_average_greater_than_or_equal_to=.true.

                  else

                     average_average_greater_than_or_equal_to=.false.

              end if!average1%m_weight>=average2%m_weight-average_tolerance


        end function average_average_greater_than_or_equal_to_K!average1,average2


            function is_greater_than_K(this,weight) result(is_greater_than)


                  implicit none


                  type(average(08)),intent(inout)::this
                  real(        08 ),intent(inout)::weight

                  logical::is_greater_than


                  if(this%m_weight>=weight+average_tolerance) then

                     is_greater_than=.true.

                  else

                     is_greater_than=.false.

              end if!this%m_weight>=weight+average_tolerance


        end function is_greater_than_K!this,weight


            function real_average_greater_than_K(real1,average2) result(real_average_greater_than)


                  implicit none


                  real(        08 ),intent(inout)::   real1
                  type(average(08)),intent(inout)::average2

                  logical::real_average_greater_than


                  real_average_greater_than=average2%is_greater_than(real1)


        end function real_average_greater_than_K!real1,average2


            function average_real_greater_than_K(average1,real2) result(average_real_greater_than)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  real(        08 ),intent(inout)::   real2

                  logical::average_real_greater_than


                  average_real_greater_than=average1%is_greater_than(real2)


        end function average_real_greater_than_K!average1,real2


            function average_average_greater_than_K(average1,average2) result(average_average_greater_than)


                  implicit none


                  type(average(08)),intent(inout)::average1
                  type(average(08)),intent(inout)::average2

                  logical::average_average_greater_than


                  if(average1%m_weight>=average2%m_weight+average_tolerance) then

                     average_average_greater_than=.true.

                  else

                     average_average_greater_than=.false.

              end if!average1%m_weight>=average2%m_weight+average_tolerance


        end function average_average_greater_than_K!average1,average2


            subroutine  read_average_K(unit,this)


                  implicit none


                  integer          ,intent(inout)::unit
                  type(average(08)),intent(inout)::this


                   read(unit,format_average_K,advance="no") this%m_weight
                   read(unit,format_average_K,advance="no") this%m_value


        end subroutine  read_average_K!unit,this


            subroutine write_average_K(unit,this)


                  implicit none


                  integer          ,intent(inout)::unit
                  type(average(08)),intent(inout)::this


                  write(unit,format_average_K,advance="no") this%m_weight
                  write(unit,format_average_K,advance="no") this%m_value


        end subroutine write_average_K!unit,this


  end module average_type



# 9 "main/../monte_carlo/monte_carlo.F90" 2
# 1 "main/../monte_carlo/../monte_carlo/time.F90" 1



# 1 "main/../monte_carlo/../system/precision.F90" 1
# 74

# 5 "main/../monte_carlo/../monte_carlo/time.F90" 2

# 1 "main/../monte_carlo/../monte_carlo/../monte_carlo/average.F90" 1
# 1196

# 7 "main/../monte_carlo/../monte_carlo/time.F90" 2


      module time_type


            use::average_type


            implicit none


            character(*),private,parameter::           format_time_K="(sp,  x,e24.17e3 )"
            character(*),private,parameter::text_field_format_time_K="(     x,a24      )"

            character(:),allocatable,public::time_file_name

            logical,public::auto_save_field_conf=.false.
            logical,public::measure_time_skipped=.false.
            logical,public::timestep_is_adaptive=.false.

!           type::time(precision)

!              integer,kind::precision

!              type(average(precision)),private::m_current_time
!              type(average(precision)),private::m_current_step

!              real(precision),private::m_life_time
!              real(precision),private::m_time_step

!           contains

!              procedure,public::time_K;generic::time=>time_K
!              procedure,public::step_K;generic::step=>step_K

!              procedure,public::make_time_K;generic::make_time=>make_time_K
!              procedure,public::load_time_K;generic::load_time=>load_time_K
!              procedure,public::save_time_K;generic::save_time=>save_time_K
!              procedure,public::time_left_K;generic::time_left=>time_left_K
!              procedure,public::push_time_K;generic::push_time=>push_time_K

!              procedure,public::agnostically_adapt_time_step_K;generic::adapt_time_step=>agnostically_adapt_time_step_K

!       end type  time


!           interface  read

!              module procedure  read_time_K

!       end interface  read

!           interface write

!              module procedure write_time_K

!       end interface write


!     contains


!           function time_K(this) result(time)


!                 implicit none


!                 type(time(08))::this

!                 real(08)::time


!                 time=this%m_current_time%weight()


!       end function time_K!this


!           function step_K(this) result(step)


!                 implicit none


!                 type(time(08))::this

!                 real(08)::step


!                 step=this%m_current_step%weight()


!       end function step_K!this


!           subroutine make_time_K(this,life_time,time_step)


!                 implicit none


!                 type(time(08))::this

!                 real(08)::life_time
!                 real(08)::time_step


!                 this%m_current_time=average(         )
!                 this%m_current_step=average(time_step)

!                 this%m_life_time=life_time+this%m_current_time%weight()
!                 this%m_time_step=time_step


!       end subroutine make_time_K!this,life_time,time_step


!           subroutine load_time_K(this,life_time,time_step)


!                 implicit none


!                 type(time(08))::this

!                 real(08)::life_time
!                 real(08)::time_step

!                 integer::unit


!                  open(newunit=unit,file=time_file_name)

!                 call  read(unit,this%m_current_time)
!                 call  read(unit,this%m_current_step)

!                 close(        unit                    )

!                 this%m_life_time=life_time+this%m_current_time%weight()
!                 this%m_time_step=time_step


!       end subroutine load_time_K!this,life_time,time_step


!           subroutine save_time_K(this)


!                 implicit none


!                 type(time(08))::this

!                 integer::unit


!                  open(newunit=unit,file=time_file_name)

!                 call write(unit,this%m_current_time)
!                 call write(unit,this%m_current_step)

!                 close(        unit                    )


!       end subroutine save_time_K!this


!           function time_left_K(this) result(time_left)


!                 implicit none


!                 type(time(08))::this

!                 logical::time_left


!                 if(this%m_current_time<=this%m_life_time) then

!                    if(this%m_current_step>this%m_life_time-this%m_current_time%weight()) &
!                       this%m_current_step=this%m_life_time-this%m_current_time%weight()

!                    time_left=.true.

!                 else

!                    this%m_life_time&
!                   =this%m_life_time+this%m_current_time%weight()

!                    time_left=.false.

!             end if!this%m_current_time<=this%m_life_time


!       end         function time_left_K!this


!           subroutine push_time_K(this)


!                 implicit none


!                 type(time(08))::this


!                 this%m_current_time&
!                =this%m_current_time&
!                +this%m_current_step


!       end subroutine push_time_K!this


!           subroutine agnostically_adapt_time_step_K(this,exp_current_step_value)


!                 implicit none


!                 type(time(08))::this

!                 real(08),intent(inout)::exp_current_step_value

!                 real(08)::current_step_value


!                 call this%m_current_step%set_average(this%m_time_step*exp(this%m_current_time%value())/exp_current_step_value,&
!                                                                                                    log(exp_current_step_value))


!       end subroutine agnostically_adapt_time_step_K!this,exp_current_step_value


!           subroutine  read_time_K(unit,this)


!                 implicit none


!                 integer       ,intent(inout)::unit
!                 type(time(08)),intent(inout)::this


!                 call  read(unit,this%m_current_time)
!                 call  read(unit,this%m_current_step)


!       end subroutine  read_time_K!unit,this


!           subroutine write_time_K(unit,this)


!                 implicit none


!                 integer       ,intent(inout)::unit
!                 type(time(08)),intent(inout)::this


!                 call write(unit,this%m_current_time)
!                 call write(unit,this%m_current_step)


!       end subroutine write_time_K!unit,this


  end module time_type



# 10 "main/../monte_carlo/monte_carlo.F90" 2






      module monte_carlo


            use::text_formatting

            use::random_number_generator
            use::average_type
            use::time_type


            implicit none


            real(08),public::life_time=+.00000e+0_08
            real(08),public::auto_save=+.00000e+0_08
            real(08),public::time_skip=+.00000e+0_08
            real(08),public::time_step=+.00000e+0_08

!           type(weight(08))::auto
!           type(weight(08))::skip
!           type(weight(08))::life

            type(average(08))::auto
            type(average(08))::skip
            type(average(08))::life
            type(average(08))::step

            public::make_monte_carlo_K
            public::load_monte_carlo_K
            public::save_monte_carlo_K

            private::read_time_parameters

            public::agnostically_adapt_time_step


            interface make_monte_carlo

               module procedure make_monte_carlo_K

        end interface make_monte_carlo

            interface load_monte_carlo

               module procedure load_monte_carlo_K

        end interface load_monte_carlo

            interface save_monte_carlo

               module procedure save_monte_carlo_K

        end interface save_monte_carlo


      contains


            subroutine make_monte_carlo_K()


                  implicit none


                  call read_time_parameters()

                  call make_seed()

!                 if(auto_save_field_conf) then

!                    call auto%make_time(life_time,auto_save)

!                    if(measure_time_skipped) then

!                       call skip%make_time(auto_save,time_skip)
!                       call life%make_time(time_skip,time_step)

!                    else

!                       call life%make_time(auto_save,time_step)

!             end    if!measure_time_skipped

!                 else

!                    if(measure_time_skipped) then

!                       call skip%make_time(life_time,time_skip)
!                       call life%make_time(time_skip,time_step)

!                    else

!                       call life%make_time(life_time,time_step)

!             end    if!measure_time_skipped

!             end if!auto_save_field_conf

                                           life=average(         )
                  if(auto_save_field_conf) auto=average(         )
                  if(measure_time_skipped) skip=average(         )
                                           step=average(time_step)


        end subroutine make_monte_carlo_K!


            subroutine load_monte_carlo_K()


                  implicit none


                  integer::unit


                  call read_time_parameters()

                  call load_seed()

                   open(newunit=unit,file=time_file_name)

                                                            read(unit,*)
                                                            read(unit,*)
                                                            read(unit,*)

!                 if(auto_save_field_conf) then

!                    call auto%load_time(life_time,auto_save); read(unit,*)

!                    if(measure_time_skipped) then

!                       call skip%load_time(auto_save,time_skip); read(unit,*)
!                       call life%load_time(time_skip,time_step); read(unit,*)

!                    else

!                       call life%load_time(auto_save,time_step); read(unit,*)

!             end    if!measure_time_skipped

!                 else

!                    if(measure_time_skipped) then

!                       call skip%load_time(life_time,time_skip); read(unit,*)
!                       call life%load_time(time_skip,time_step); read(unit,*)

!                    else

!                       call life%load_time(life_time,time_step); read(unit,*)

!             end    if!measure_time_skipped

!             end if!auto_save_field_conf

                                           call  read(unit,life); life_time&
                                                                 =life_time+weight(life)
                  if(auto_save_field_conf) call  read(unit,auto); auto_save&
                                                                 =auto_save+weight(auto)
                  if(measure_time_skipped) call  read(unit,skip); time_skip&
                                                                 =time_skip+weight(skip)
                  if(timestep_is_adaptive) then

                                           call  read(unit,step)

                  else

                     step=average(time_step)

              end if!timestep_is_adaptive

                  close(        unit                    )


        end subroutine load_monte_carlo_K!


            subroutine save_monte_carlo_K()


                  implicit none


                  integer::unit


                  call save_seed()

                   open(newunit=unit,file=time_file_name)

                                                                 write(unit,*) "LAST TIME-STAMP OF SIMULATION"
                                                                 write(unit,*) "___________","___________","___________",&
                                                                               "___________","___________","___________",&
                                                                               "___________","___________","___________",&
                                                                               "___________","___________","___________"
                                                                 write(unit,*)

!                if(auto_save_field_conf) call auto%save_time(); write(unit,*) " configuration auto-save weight scale"
!                if(measure_time_skipped) call skip%save_time(); write(unit,*) " measurment    weight-skip      scale"
!                                         call life%save_time(); write(unit,*) "               weight-step           "

                                           call write(unit,life); write(unit,*) " simulation"
                  if(auto_save_field_conf) call write(unit,auto); write(unit,*) " auto-saved configurations"
                  if(measure_time_skipped) call write(unit,skip); write(unit,*) " measurements"
                  if(timestep_is_adaptive) call write(unit,step); write(unit,*) " time-step"

                  close(        unit                    )

        end subroutine save_monte_carlo_K!


            subroutine read_time_parameters()


                  implicit none


                  write(*,"(2a)",advance="no") "life time                  : ",t_yellow
                   read(*,   *               )  life_time
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,"(2a)",advance="no") "auto save (0 for disabling): ",t_yellow
                   read(*,   *               )  auto_save
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,"(2a)",advance="no") "time skip (0 for disabling): ",t_yellow
                   read(*,   *               )  time_skip
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,"(2a)",advance="no") "time step (0 for disabling): ",t_yellow
                   read(*,   *               )  time_step
                  write(*,"(2a)",advance="no")                                 t_normal
                  write(*,   *               )


                  if(time_skip>time_step*10) then

                     if(auto_save>time_skip*10) then

                        if(life_time<auto_save*10) stop "Error: insufficient life time"

                        auto_save_field_conf=.true.

                     else

                        if(life_time<time_skip*10) stop "Error: insufficient life time"

              end    if!auto_save>time_skip*10) then

                     measure_time_skipped=.true.

                  else

                     if(auto_save>time_step*10) then

                        if(life_time<auto_save*10) stop "Error: insufficient life time"

                        auto_save_field_conf=.true.

                     else

                        if(life_time<time_step*10) stop "Error: insufficient life time"

              end    if!auto_save>time_step*10

              end if!time_skip>time_step*10


        end subroutine read_time_parameters!


           subroutine agnostically_adapt_time_step(exp_control_step)


                  implicit none


                  real(08)::exp_control_step


                  call step%average(time_step*exp(value(life))/exp_control_step,&
                                                           log(exp_control_step))

                  if(life>=life_time-sqrt(weight(step))&
                                         -weight(step)) call step%weight(life_time-weight(life))


        end subroutine agnostically_adapt_time_step!exp_control_step


  end module monte_carlo



# 30 "main/main.F90" 2

!     include "../tools/insert_sort.F90"
!     include "../tools/brent_minimization.F90"

# 39


!     include "../tools/conjugate_gradient.F90"



!     include "../ikkt/constants.F90"
!     include "../ikkt/fields.F90"
!     include "../ikkt/gauge_cooling.F90"
# 1 "main/../ikkt/complex_langevin.F90" 1



# 1 "main/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/complex_langevin.F90" 2

# 1 "main/../ikkt/../tensor/tensor.F90" 1



# 1 "main/../ikkt/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/../tensor/tensor.F90" 2

# 1 "main/../ikkt/../tensor/../tools/insert_sort.F90" 1



# 1 "main/../ikkt/../tensor/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/../tensor/../tools/insert_sort.F90" 2


      module insertion_sort


            implicit none


      contains


            subroutine sort_one(list,n)


                  implicit none


                  real(08),dimension(:),intent(inout)::list
                  real(08)                           ::temp

                  integer,intent(in)::n
                  integer           ::i


                  temp=list(n);i=n-1

                  do while((i.ge.0).and.(list(i).gt.temp))

                     list(i+1)=list(i);i=i-1

              end do!while((i.ge.0).and.(list(i).gt.temp))

                  list(i+1)=temp


        end subroutine sort_one!list,n


            subroutine sort(list)


                  implicit none


                  real(08),dimension(:),intent(inout)::list
                  real(08)                           ::temp

                  integer::n


                  do n=lbound(list,dim=1),ubound(list,dim=1),+1

                     call sort_one(list,n)

              end do!n=lbound(list,dim=1),ubound(list,dim=1),+1


        end subroutine sort!list


            recursive function sort_sum(list) result(list_sum)


                  implicit none


                  real(08),dimension(:),intent(inout)::list

                  real(08)::temp_sum
                  real(08)::list_sum

                  if(size(list)==1)then

                     list_sum=list(ubound(list,dim=1))

                  else

                     temp_sum=sort_sum(list(lbound(list,dim=1):ubound(list,dim=1)-1))
                     list_sum=temp_sum+(list(ubound(list,dim=1))-temp_sum)/real(size(list),kind=08)

              end if!size(list)==1


        end           function sort_sum!list


  end module insertion_sort



# 7 "main/../ikkt/../tensor/tensor.F90" 2


      module tensor_type


            use::blas95,only:dotu,&
                             dotc,&
                             gemv,&
                             gemm

            use::insertion_sort


            implicit none


            character(*),private,parameter::           format_tensor_K="(sp,2(x,e24.17e3))"
            character(*),private,parameter::text_field_format_tensor_K="(   2(x,a24     ))"


            interface vector

               module procedure vector_matrix_constructor_K

        end interface vector

            interface matrix

               module procedure matrix_factor_constructor_K
               module procedure matrix_vector_constructor_K

        end interface matrix

            interface operator(.o.)

               module procedure vector_vector_scalar_contraction_real_K

               module procedure vector_matrix_vector_contraction_real_K
               module procedure matrix_vector_vector_contraction_real_K
               module procedure matrix_matrix_matrix_contraction_real_K

        end interface operator(.o.)

            interface operator(.x.)

               module procedure vector_vector_vector_ext_product_real_K

               module procedure matrix_matrix_matrix_ext_product_real_K

        end interface operator(.x.)

            interface operator(.c.)

               module procedure vector_vector_scalar_contraction_complex_K

               module procedure vector_matrix_vector_contraction_complex_K
               module procedure matrix_vector_vector_contraction_complex_K
               module procedure matrix_matrix_matrix_contraction_complex_K

        end interface operator(.c.)

            interface operator(.z.)

               module procedure vector_vector_vector_ext_product_complex_K

               module procedure matrix_matrix_matrix_ext_product_complex_K

        end interface operator(.z.)

            interface operator(.commutation.)

               module procedure matrix_matrix_matrix_commutation_K

        end interface operator(.commutation.)

            interface operator(.anticommutation.)

               module procedure matrix_matrix_matrix_anticommutation_K

        end interface operator(.anticommutation.)

            interface trace

               module procedure matrix_trace_K

        end interface trace

            interface trace_piece

               module procedure matrix_trace_piece_K

        end interface trace_piece

            interface determinant

               module procedure matrix_determinant_K

        end interface determinant

            interface conjugate

               module procedure matrix_conjugate_K

        end interface conjugate

            interface     hermitian

               module procedure matrix_hermitian_K

        end interface     hermitian

            interface antihermitian

               module procedure matrix_antihermitian_K

        end interface antihermitian

            interface norm

               module procedure scalar_norm_squared_K
               module procedure vector_norm_squared_K
               module procedure matrix_norm_squared_K

               module procedure vector_matrix_norm_squared_K

        end interface norm

            interface make_eigenmatrix

               module procedure make_matrix_eigenmatrix_K

        end interface make_eigenmatrix

            interface make_traceless

               module procedure make_matrix_traceless_K

        end interface make_traceless

            interface make_hermitian

               module procedure make_matrix_hermitian_K

        end interface make_hermitian

            interface make_antihermitian

               module procedure make_matrix_antihermitian_K

        end interface make_antihermitian

            interface random_number

               module procedure vector_random_number_K
               module procedure matrix_random_number_K

        end interface random_number

            interface  read

               module procedure  read_vector_K
               module procedure  read_matrix_K

        end interface  read

            interface write

               module procedure write_vector_K
               module procedure write_matrix_K

        end interface write


      contains


            function vector_matrix_constructor_K(this) result(that)


                  implicit none


                  complex(08),dimension(0:                  ,&
                                        0:                  ),intent(inout)::this
                  complex(08),dimension(0:size(this,dim=1)-1)              ::that

                  integer::index


                  do index=0,size(that,dim=1)-1,+1

                     that(index)=this(index,index)

              end do!index=0,size(that,dim=1)-1,+1


        end function vector_matrix_constructor_K!this


            function matrix_factor_constructor_K(size,factor) result(that)


                  implicit none


                  integer    ,                    intent(inout)::size
                  complex(08),                    intent(inout)::factor
                  complex(08),dimension(0:size-1,&
                                        0:size-1)              ::that

                  integer::index


                  that=(+.00000e+0_08,&
                        +.00000e+0_08)

                  do index=0,size-1,+1

                     that(index,index)=factor

              end do!index=0,size-1,+1


        end function matrix_factor_constructor_K!size,factor


            function matrix_vector_constructor_K(this) result(that)


                  implicit none


                  complex(08),dimension(0:                  ),intent(inout)::this
                  complex(08),dimension(0:size(this,dim=1)-1,&
                                        0:size(this,dim=1)-1)              ::that

                  integer::index


                  that=(+.00000e+0_08,&
                        +.00000e+0_08)

                  do index=0,size(that,dim=1)-1,+1

                     that(index,index)=this(index)

              end do!index=0,size(that,dim=1)-1,+1


        end function matrix_vector_constructor_K!this


            function vector_vector_scalar_contraction_real_K(vector0,vector1) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  complex(08),dimension(0:size(vector0,dim=1)-1),intent(inout)::vector1
                  complex(08)                                                 ::scalar_




                  scalar_=sum(vector0*vector1)

# 278



        end function vector_vector_scalar_contraction_real_K!vector0,vector1


            function vector_matrix_vector_contraction_real_K(vector0,matrix1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix1,dim=2)-1)              ::vector_




                  vector_=matmul(      vector0 ,matrix1)

# 304



        end function vector_matrix_vector_contraction_real_K!vector0,matrix1


            function matrix_vector_vector_contraction_real_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1),intent(inout)::vector1
                  complex(08),dimension(0:size(matrix0,dim=1)-1)              ::vector_




                  vector_=matmul(matrix0 ,vector1)

# 330



        end function matrix_vector_vector_contraction_real_K!matrix0,vector1


            function matrix_matrix_matrix_contraction_real_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_




                  matrix_=matmul(matrix0,matrix1)

# 358



        end function matrix_matrix_matrix_contraction_real_K!matrix0,matrix1


            function vector_vector_vector_ext_product_real_K(vector0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  complex(08),dimension(0:                     ),intent(inout)::vector1
                  complex(08),dimension(0:size(vector0,dim=1)   &
                                         *size(vector1,dim=1)-1)              ::vector_

                  integer::i0
                  integer::i1


                  do i0=0,size(vector0,dim=1)-1,+1

                     do i1=0,size(vector1,dim=1)-1,+1

                        vector_(i0*size(vector1,dim=1)+i1)=vector0(i0)&
                                                          *vector1(i1)

              end    do!i1=0,size(vector1,dim=1)-1,+1

              end do!i0=0,size(vector0,dim=1)-1,+1


        end function vector_vector_vector_ext_product_real_K!vector0,vector1


            function matrix_matrix_matrix_ext_product_real_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)   &
                                         *size(matrix1,dim=1)-1,&
                                        0:size(matrix0,dim=2)   &
                                         *size(matrix1,dim=2)-1)              ::matrix_

                  integer::i0,j0
                  integer::i1,j1


                  do j0=0,size(matrix0,dim=2)-1,+1

                     do j1=0,size(matrix1,dim=2)-1,+1

                        do i0=0,size(matrix0,dim=1)-1,+1

                           do i1=0,size(matrix1,dim=1)-1,+1

                              matrix_(i0*size(matrix1,dim=1)+i1,&
                                      j0*size(matrix1,dim=2)+j1)=matrix0(i0,j0)&
                                                                *matrix1(i1,j1)

              end          do!i1=0,size(matrix1,dim=1)-1,+1

              end       do!i0=0,size(matrix0,dim=1)-1,+1

              end    do!j1=0,size(matrix1,dim=2)-1,+1

              end do!j0=0,size(matrix0,dim=2)-1,+1


        end function matrix_matrix_matrix_ext_product_real_K!matrix0,matrix1


            function vector_vector_scalar_contraction_complex_K(vector0,vector1) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  complex(08),dimension(0:size(vector0,dim=1)-1),intent(inout)::vector1
                  complex(08)                                                 ::scalar_




                  scalar_=dot_product(vector0,vector1)

# 456



        end function vector_vector_scalar_contraction_complex_K!vector0,vector1


            function vector_matrix_vector_contraction_complex_K(vector0,matrix1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix1,dim=2)-1)              ::vector_




                  vector_=matmul(conjg(vector0),matrix1)

# 482



        end function vector_matrix_vector_contraction_complex_K!vector0,matrix1


            function matrix_vector_vector_contraction_complex_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1),intent(inout)::vector1
                  complex(08),dimension(0:size(matrix0,dim=1)-1)              ::vector_




                  vector_=matmul(conjugate(matrix0),vector1)

# 508



        end function matrix_vector_vector_contraction_complex_K!matrix0,vector1


            function matrix_matrix_matrix_contraction_complex_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_




                  matrix_=matmul(conjugate(matrix0),matrix1)

# 536



        end function matrix_matrix_matrix_contraction_complex_K!matrix0,matrix1


            function vector_vector_vector_ext_product_complex_K(vector0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  complex(08),dimension(0:                     ),intent(inout)::vector1
                  complex(08),dimension(0:size(vector0,dim=1)   &
                                         *size(vector1,dim=1)-1)              ::vector_

                  integer::i0
                  integer::i1


                  do i0=0,size(vector0,dim=1)-1,+1

                     do i1=0,size(vector1,dim=1)-1,+1

                        vector_(i0*size(vector1,dim=1)+i1)=      vector0(i0) &
                                                          *conjg(vector1(i1))

              end    do!i1=0,size(vector1,dim=1)-1,+1

              end do!i0=0,size(vector0,dim=1)-1,+1


        end function vector_vector_vector_ext_product_complex_K!vector0,vector1


            function matrix_matrix_matrix_ext_product_complex_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)   &
                                         *size(matrix1,dim=1)-1,&
                                        0:size(matrix0,dim=2)   &
                                         *size(matrix1,dim=2)-1)              ::matrix_

                  integer::i0,j0
                  integer::i1,j1


                  do j0=0,size(matrix0,dim=2)-1,+1

                     do j1=0,size(matrix1,dim=2)-1,+1

                        do i0=0,size(matrix0,dim=1)-1,+1

                           do i1=0,size(matrix1,dim=1)-1,+1

                              matrix_(i0*size(matrix1,dim=1)+i1,&
                                      j0*size(matrix1,dim=2)+j1)=      matrix0(i0,j0) &
                                                                *conjg(matrix1(j1,i1))

              end          do!i1=0,size(matrix1,dim=1)-1,+1

              end       do!i0=0,size(matrix0,dim=1)-1,+1

              end    do!j1=0,size(matrix1,dim=2)-1,+1

              end do!j0=0,size(matrix0,dim=2)-1,+1


        end function matrix_matrix_matrix_ext_product_complex_K!matrix0,matrix1


            function matrix_matrix_matrix_commutation_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1,&
                                        0:size(matrix0,dim=1)-1),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


                  matrix_=(matrix0.o.matrix1)&
                         -(matrix1.o.matrix0)


        end function matrix_matrix_matrix_commutation_K!matrix0,matrix1


            function matrix_matrix_matrix_anticommutation_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1,&
                                        0:size(matrix0,dim=1)-1),intent(inout)::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


                  matrix_=(matrix0.o.matrix1)&
                         +(matrix1.o.matrix0)


        end function matrix_matrix_matrix_anticommutation_K!matrix0,matrix1


            function matrix_trace_K(matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08)                                                 ::scalar_

                  integer::index


                  scalar_=(+.00000e+0_08,&
                           +.00000e+0_08)

                  do index=0,size(matrix0,dim=1)-1,+1

                     scalar_&
                    =scalar_+matrix0(index,index)

              end do!index=0,size(matrix0,dim=1)-1,+1

!                 scalar_=sum(vector(matrix0))


        end function matrix_trace_K!matrix0


            function matrix_trace_piece_K(matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08)                                                 ::scalar_

                  real(08):: real_vector_matrix0(0:size(matrix0,dim=1)-1)
                  real(08)::aimag_vector_matrix0(0:size(matrix0,dim=1)-1)


                   real_vector_matrix0= real(vector(matrix0))
                  aimag_vector_matrix0=aimag(vector(matrix0))

                  scalar_=cmplx(sort_sum( real_vector_matrix0),&
                                sort_sum(aimag_vector_matrix0),kind=08)


        end function matrix_trace_piece_K!matrix0


            recursive function matrix_determinant_K(matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08)                                                 ::scalar_

                  integer::k


                  if(size(matrix0)==4) then

                     scalar_=matrix0(0,0)*matrix0(1,1)&
                            -matrix0(0,1)*matrix0(1,0)

                  else

                     scalar_=(+.00000e+0_08,&
                              +.00000e+0_08)

                     do k=0,size(matrix0,dim=1)-1,+1

                        scalar_&
                       =scalar_+matrix0(k,0)*matrix_determinant_K(cofactor(matrix0,k,0))

              end    do!k=0,size(matrix0,dim=1)-1,+1

              end if!size(matrix0,dim=1)-1==0


            contains


                  function cofactor(matrix0,i0,j0) result(matrix_)


                        implicit none


                        complex(08),dimension(0:                     ,&
                                              0:                     ),intent(inout)::matrix0
                        complex(08),dimension(0:size(matrix0,dim=1)-2,&
                                              0:size(matrix0,dim=2)-2)              ::matrix_

                        integer,intent(inout)::i0
                        integer,intent(inout)::j0
                        integer              ::i
                        integer              ::j


                        do j=0,size(matrix_,dim=2)-1,+1

                           do i=0,size(matrix_,dim=1)-1,+1

                              matrix_(i,j)=matrix0(mod(i0+1+i,size(matrix0,dim=1)),mod(j0+j+1,size(matrix0,dim=2)))*sign(i0+j0)

                    end    do!i=0,size(matrix_,dim=1)-1,+1

                    end do!j=0,size(matrix_,dim=2)-1,+1


              end function cofactor!matrix0,i0,j0


                  function sign(exponent)


                        implicit none


                        integer,intent(inout)::exponent
                        integer              ::sign


                        sign=(-1)**exponent


              end function sign!exponent


        end           function matrix_determinant_K!matrix0


            function matrix_conjugate_K(matrix0) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=conjg(matrix0(j,i))

              end    do!i=0,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=conjg(transpose(matrix0))


        end function matrix_conjugate_K!matrix0


            function matrix_hermitian_K(matrix0) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j


                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix_(i,j)=(conjg(matrix0(j,i))+matrix0(i,j))*(+.50000e+0_08,&
                                                                         +.00000e+0_08)

              end    do!i=0,j-1,+1

                     matrix_(j,j)=real(matrix0(i,j))

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=(conjugate(matrix0)+matrix0)*(+.50000e+0_08,&
!                                                       +.00000e+0_08)


        end function matrix_hermitian_K!matrix0


            function matrix_antihermitian_K(matrix0) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix_(i,j)=(conjg(matrix0(j,i))-matrix0(i,j))*(+.00000e+0_08,&
                                                                         +.50000e+0_08)

              end    do!i=0,j-1,+1

                     matrix_(j,j)=aimag(matrix0(i,j))

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=-conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=(conjugate(matrix0)-matrix0)*(+.00000e+0_08,&
!                                                       +.50000e+0_08)


        end function matrix_antihermitian_K!matrix0


            function scalar_norm_squared_K(scalar0) result(scalar_)


                  implicit none


                  complex(08)                                   ,intent(inout)::scalar0
                  real(   08)                                                 ::scalar_


                  scalar_=           conjg(scalar0)* scalar0


        end function scalar_norm_squared_K!scalar0


            function vector_norm_squared_K(vector0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  real(   08)                                                 ::scalar_


                  scalar_=real(  sum(conjg(vector0)* vector0))
!                 scalar_=real(            vector0.c.vector0)


        end function vector_norm_squared_K!vector0


            function matrix_norm_squared_K(matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  real(   08)                                                 ::scalar_


                  scalar_=real(  sum(conjg(matrix0)* matrix0))
!                 scalar_=real(trace(      matrix0.c.matrix0))

        end function matrix_norm_squared_K!matrix0


            function vector_matrix_norm_squared_K(vector0,matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0
                  complex(08),dimension(0:size(vector0,dim=1)-1,&
                                        0:size(vector0,dim=1)-1),intent(inout)::matrix0
                  real(   08)                                                 ::scalar_


                  scalar_=real(vector0.c.(matrix0.o.vector0))


        end function vector_matrix_norm_squared_K!vector0,matrix0


            subroutine make_matrix_eigenmatrix_K(matrix0,eigenvalue)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08)                                   ,intent(inout)::eigenvalue

                  integer::index


                  do index=0,size(matrix0,dim=1)-1,+1

                     matrix0(index,index)=matrix0(index,index)-eigenvalue

              end do!index=0,size(matrix0,dim=1)-1,+1

!                 matrix0=matrix0-matrix(size(matrix0,dim=1),eigenvalue)


        end subroutine make_matrix_eigenmatrix_K!matrix0,eigenvalue


            subroutine make_matrix_traceless_K(matrix0)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0

!                 call make_eigenmatrix(matrix0,trace_piece(matrix0))

!                 call make_eigenmatrix(matrix0,trace(matrix0)/real(size(matrix0,dim=1)))

                  matrix0(size(matrix0,dim=1)-1,size(matrix0,dim=2)-1)&
                 =matrix0(size(matrix0,dim=1)-1,size(matrix0,dim=2)-1)-trace(matrix0)


        end subroutine make_matrix_traceless_K!matrix0


            subroutine make_matrix_hermitian_K(matrix0,factor)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)         ::matrix0
                  real(   08),                                   intent(inout),optional::factor

                  integer::i,j


                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(conjg(matrix0(j,i))+matrix0(i,j))*(+.50000e+0_08,&
                                                                         +.00000e+0_08)

              end    do!i=0,j-1,+1

                     matrix0(j,j)=real(matrix0(i,j))

              end do!j=0,size(matrix0,dim=2)-1,+1

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=j+1,size(matrix0,dim=1)-1,+1

                        matrix0(i,j)=conjg(matrix0(j,i))

              end    do!i=j+1,size(matrix0,dim=1)-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1

                  if(present(factor)) matrix0&
                                     =matrix0*factor


        end subroutine make_matrix_hermitian_K!matrix0,factor


            subroutine make_matrix_antihermitian_K(matrix0,factor)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)         ::matrix0
                  real(   08),                                   intent(inout),optional::factor


                  integer::i,j

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(conjg(matrix0(j,i))-matrix0(i,j))*(+.00000e+0_08,&
                                                                         +.50000e+0_08)

              end    do!i=0,j-1,+1

                     matrix0(j,j)=aimag(matrix0(i,j))

              end do!j=0,size(matrix0,dim=2)-1,+1

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=j+1,size(matrix0,dim=1)-1,+1

                        matrix0(i,j)=-conjg(matrix0(j,i))

              end    do!i=j+1,size(matrix0,dim=1)-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1

                  if(present(factor)) matrix0&
                                     =matrix0*factor


        end subroutine make_matrix_antihermitian_K!matrix0,factor


            subroutine vector_random_number_K(vector0)


                  implicit none


                  complex(08),dimension(0:                     ),intent(inout)::vector0

                  real(08),dimension(0:size(vector0,dim=1)-1):: real_vector0
                  real(08),dimension(0:size(vector0,dim=1)-1)::aimag_vector0


                  call random_number( real_vector0)
                  call random_number(aimag_vector0)


                  vector0=cmplx( real_vector0,&
                                aimag_vector0,kind=08)


        end subroutine vector_random_number_K!vector0


            subroutine matrix_random_number_K(matrix0)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0

                  real(08),dimension(0:size(matrix0,dim=1)-1,&
                                     0:size(matrix0,dim=2)-1):: real_matrix0
                  real(08),dimension(0:size(matrix0,dim=1)-1,&
                                     0:size(matrix0,dim=2)-1)::aimag_matrix0


                  call random_number( real_matrix0)
                  call random_number(aimag_matrix0)


                  matrix0=cmplx( real_matrix0,&
                                aimag_matrix0,kind=08)


        end subroutine matrix_random_number_K!matrix0


            subroutine  read_vector_K(unit,vector0)


                  implicit none


                  integer    ,                                   intent(inout)::unit
                  complex(08),dimension(0:                     ),intent(inout)::vector0

                  integer::i


                  do i=0,size(vector0,dim=1)-1,+1

                      read(unit,format_tensor_K) vector0(i)

              end do!i=0,size(vector0,dim=1)-1,+1

                   read(unit,*)


        end subroutine  read_vector_K!unit,vector0


            subroutine  read_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,                                   intent(inout)::unit
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0

                  integer::i,j


                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,size(matrix0,dim=1)-1,+1

                         read(unit,format_tensor_K) matrix0(i,j)

              end    do!i=0,size(matrix0,dim=1)-1,+1

                      read(unit,*)

              end do!j=0,size(matrix0,dim=2)-1,+1

                   read(unit,*)


        end subroutine  read_matrix_K!unit,matrix0


            subroutine write_vector_K(unit,vector0)


                  implicit none


                  integer    ,                                   intent(inout)::unit
                  complex(08),dimension(0:                     ),intent(inout)::vector0

                  integer::i


                  do i=0,size(vector0,dim=1)-1,+1

                     write(unit,format_tensor_K) vector0(i)

              end do!i=0,size(vector0,dim=1)-1,+1

                  write(unit,*)


        end subroutine write_vector_K!unit,vector0


            subroutine write_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,                                   intent(inout)::unit
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0

                  integer::i
                  integer::j


                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,size(matrix0,dim=1)-1,+1

                        write(unit,format_tensor_K) matrix0(i,j)

              end    do!i=0,size(matrix0,dim=1)-1,+1

                     write(unit,*)

              end do!j=0,size(matrix0,dim=2)-1,+1

                  write(unit,*)


        end subroutine write_matrix_K!unit,matrix0


  end module tensor_type



# 7 "main/../ikkt/complex_langevin.F90" 2

# 1 "main/../ikkt/../monte_carlo/monte_carlo.F90" 1
# 307

# 9 "main/../ikkt/complex_langevin.F90" 2

# 1 "main/../ikkt/../ikkt/constants.F90" 1



# 1 "main/../ikkt/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/../ikkt/constants.F90" 2
# 1 "main/../ikkt/../ikkt/../main/mathematical_constants.F90" 1
# 44

# 6 "main/../ikkt/../ikkt/constants.F90" 2

# 1 "main/../ikkt/../ikkt/../tensor/tensor.F90" 1
# 1274

# 8 "main/../ikkt/../ikkt/constants.F90" 2


      module constants


            use::mathematical_constants

            use::tensor_type


            implicit none


            complex(08),dimension(0:1,&
                                  0:1,&
                                  0:3),parameter,public::sigma=[[[+re_unit,    zero],[    zero,+re_unit]],&
                                                                [[    zero,+re_unit],[+re_unit,    zero]],&
                                                                [[    zero,+im_unit],[-im_unit,    zero]],&
                                                                [[+re_unit,    zero],[    zero,-re_unit]]]

            complex(08),allocatable,dimension( : ,&
                                               : ),public,protected::delta
            complex(08),allocatable,dimension( : ,&
                                               : ,&
                                               : ),public,protected::gamma,&
                                                           conjugate_gamma
            complex(08),allocatable,dimension( : ,&
                                               : ,&
                                               : ,&
                                               : ),public,protected::gamma_core

            private::gamma_size

            private::make_delta
            private::make_gamma
            private::make_gamma_core

            public::determinant_degree

            public:: make_constants
            public::eject_constants


            contains


            function gamma_size(size)


                  implicit none


                  integer,intent(inout)::size

                  integer::gamma_size


                  gamma_size=2**(size/2-1)


        end function gamma_size!size


            subroutine make_delta(size)


                  implicit none


                  integer,intent(inout)::size


                  if(allocated(delta)) return

                     allocate (delta(0:size-1,&
                                     0:size-1))

                  delta=matrix(size,+re_unit)


        end subroutine make_delta!size


            subroutine make_gamma(size)


                  implicit none


                  integer,intent(inout)::size


                  allocate(gamma(0:gamma_size(size)-1,&
                                 0:gamma_size(size)-1,&
                                 0:           size -1))

                  select case(size)

                  case( 4)

                     gamma(:,:,0)=im_unit*(sigma(:,:,1))
                     gamma(:,:,1)=im_unit*(sigma(:,:,2))
                     gamma(:,:,2)=im_unit*(sigma(:,:,3))
                     gamma(:,:,3)=        (sigma(:,:,0))


                  case( 6)

                     gamma(:,:,0)=        (sigma(:,:,1).x.sigma(:,:,2))
                     gamma(:,:,1)=        (sigma(:,:,2).x.sigma(:,:,2))
                     gamma(:,:,2)=        (sigma(:,:,3).x.sigma(:,:,2))
                     gamma(:,:,3)=        (sigma(:,:,0).x.sigma(:,:,1))
                     gamma(:,:,4)=        (sigma(:,:,0).x.sigma(:,:,3))
                     gamma(:,:,5)=im_unit*(sigma(:,:,0).x.sigma(:,:,0))

                  case(10)

                     gamma(:,:,0)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,2))
                     gamma(:,:,1)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,1))
                     gamma(:,:,2)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,3))
                     gamma(:,:,3)=im_unit*(sigma(:,:,2).x.sigma(:,:,1).x.sigma(:,:,2).x.sigma(:,:,0))
                     gamma(:,:,4)=im_unit*(sigma(:,:,2).x.sigma(:,:,3).x.sigma(:,:,2).x.sigma(:,:,0))
                     gamma(:,:,5)=im_unit*(sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,1).x.sigma(:,:,2))
                     gamma(:,:,6)=im_unit*(sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,3).x.sigma(:,:,2))
                     gamma(:,:,7)=im_unit*(sigma(:,:,1).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))
                     gamma(:,:,8)=im_unit*(sigma(:,:,3).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))
                     gamma(:,:,9)=        (sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))

                  case default

                     stop "Error: unavailable spacetime dimension (4,6,10)"

              end select!case(size)


        end subroutine make_gamma!size


            subroutine make_gamma_core(size)


                  implicit none


                  integer,intent(inout)::size

                  integer::mu,nu


                  allocate(conjugate_gamma(0:gamma_size(size)-1,&
                                           0:gamma_size(size)-1,&
                                           0:           size -1))

                  allocate(gamma_core(0:gamma_size(size)-1,&
                                      0:gamma_size(size)-1,&
                                      0:           size -1,&
                                      0:           size -1))

                  do mu=0,size-1,+1

                     conjugate_gamma(:,:,mu)&
                    =conjugate(gamma(:,:,mu))

                     do nu=0,size-1,+1

                        gamma_core(:,:,mu,nu)=conjugate_gamma(:,:,mu).o.gamma(:,:,nu)

              end    do!nu=0,size-1,+1

              end do!mu=0,size-1,+1


        end subroutine make_gamma_core!size


            function determinant_degree(size)


                  implicit none


                  integer,intent(inout)::size

                  real(08)::determinant_degree


                  select case(size)

                  case(4,6)

                     determinant_degree=+.10000e+1_08

                  case(10)

                     determinant_degree=+.50000e+0_08

              end select!case(size)


        end function determinant_degree!d


            subroutine make_constants(inner_size,&
                                      boson_size)


                  implicit none


                  integer,intent(inout)::inner_size
                  integer,intent(inout)::boson_size


                  call make_delta     (inner_size)
                  call make_gamma     (boson_size)
                  call make_gamma_core(boson_size)

!                 call print_gamma()


        end subroutine make_constants!inner_size,
!                                     boson_size


            subroutine eject_constants


                  implicit none


                  if(allocated(          delta     )) deallocate(          delta     )
                  if(allocated(          gamma     )) deallocate(          gamma     )
                  if(allocated(conjugate_gamma     )) deallocate(conjugate_gamma     )
                  if(allocated(          gamma_core)) deallocate(          gamma_core)


        end subroutine eject_constants


            subroutine print_gamma()


                  implicit none


                  integer::unit

                  integer::mu,nu


                   open(newunit=unit,file="gamma.matrix")

                  call write(unit,delta(:,:))

                  do mu=0,size(sigma,dim=3)-1,+1

                     call write(unit,sigma(:,:,mu))

              end do!mu=0,size(sigma,dim=3)-1,+1

                  do mu=0,size(gamma,dim=3)-1,+1

                     call write(unit,gamma(:,:,mu))

              end do!mu=0,size(gamma,dim=3)-1,+1

                  do mu=0,size(gamma_core,dim=3)-1,+1

                     do nu=0,size(gamma_core,dim=3)-1,+1

                        write(unit,*) norm(conjugate(gamma_core(:,:,mu,nu))&
                                                    -gamma_core(:,:,mu,nu))

              end    do!nu=0,size(gamma_core,dim=3)-1,+1

              end do!mu=0,size(gamma_core,dim=3)-1,+1

                  close(        unit                    )


        end subroutine print_gamma!


  end module constants



# 11 "main/../ikkt/complex_langevin.F90" 2
# 1 "main/../ikkt/../ikkt/fields.F90" 1



# 1 "main/../ikkt/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/../ikkt/fields.F90" 2
# 1 "main/../ikkt/../ikkt/../system/text_format.F90" 1
# 106

# 6 "main/../ikkt/../ikkt/fields.F90" 2
# 1 "main/../ikkt/../ikkt/../main/mathematical_constants.F90" 1
# 44

# 7 "main/../ikkt/../ikkt/fields.F90" 2

# 1 "main/../ikkt/../ikkt/../tensor/tensor.F90" 1
# 1274

# 9 "main/../ikkt/../ikkt/fields.F90" 2

# 1 "main/../ikkt/../ikkt/../ikkt/constants.F90" 1
# 294

# 11 "main/../ikkt/../ikkt/fields.F90" 2


      module fields


            use::lapack95,only:geev,&
                               ggev

            use::text_formatting
            use::mathematical_constants

            use::tensor_type

            use::constants


            implicit none


            character(*),private,parameter::           format_stat_K="(sp,  x,i20.19   )"
            character(*),private,parameter::text_field_format_stat_K="(     x,a20      )"

            character(*),private,parameter::           format_mass_K="(sp,  x,e24.17e3 )"
            character(*),private,parameter::text_field_format_mass_K="(     x,a24      )"

            logical,public::configuration_loaded=.false.
            logical,public::start_field_is_noisy=.false.
            logical,public::fermions_included_in=.false.
            logical,public::massive_deformations=.false.

            character(:),allocatable,public::conf_file_name
            character(:),allocatable,public::save_file_name

            integer,public::inner_degrees_of_freedom,n,n_size
            integer,public::boson_degrees_of_freedom,d,a_size
            integer,public::fermi_degrees_of_freedom,p,f_size

            real(08),                                                    public::boson_epsilon

            real(08),dimension( :                          ),allocatable,public::boson_mass
            real(08),                                                    public::fermi_mass

            complex(08),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::a

# 63


            complex(08),dimension( :        ),allocatable,public::f,m_eigenvalues_
            complex(08),dimension( :        ,&
                                   :        ),allocatable,public::m,cm,cmm
            complex(08),dimension( :        ,&
                                   :        , :                          ,&
                                              :                          ,&
                                              :                          ),allocatable,public::ma

            private::make_m
            private::make_cm
            private::make_cmm
            private::make_ma
            private::make_m_eigenvalues

            public::update_fermion_matrix



            public::read_field_parameters

            public::make_fields
            public::load_fields
            public::save_fields
            public::boson_noise
            public::fermi_noise


      contains


            subroutine make_fields()


                  implicit none


                  integer::mu


                  call read_field_parameters()

                  a=boson_noise(.20000e+1_08)


        end subroutine make_fields!


            subroutine load_fields()


                  implicit none


                  integer::unit
                  integer::mu


                  call read_field_parameters()

                   open(newunit=unit,file=conf_file_name)

                  do mu=0,boson_degrees_of_freedom-1,+1

                     call  read(unit,a(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1

                   read(unit,*)

                  close(unit)


        end subroutine load_fields!


            subroutine save_fields()


                  implicit none


                  integer::unit
                  integer::mu


                   open(newunit=unit,file=conf_file_name)



                  do mu=0,boson_degrees_of_freedom-1,+1

                     call write(unit,a(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  write(unit,*)

                  close(unit)


        end subroutine save_fields!


            subroutine  read_field_parameters()


                  implicit none


                  integer::mu

!                 character(*)::temp_file_name


                  write(*,"(2a)",advance="no") "inner_degrees_of_freedom: ",t_yellow
                   read(*,   *               )  inner_degrees_of_freedom
                  write(*,"( a)",advance="no")                              t_normal
                  write(*,"(2a)",advance="no") "boson_degrees_of_freedom: ",t_yellow
                   read(*,   *               )  boson_degrees_of_freedom
                  write(*,"( a)",advance="no")                              t_normal

                                                   fermi_degrees_of_freedom=2 &
                                                **(boson_degrees_of_freedom/2 &
                                                                           -1)
                                           n     = inner_degrees_of_freedom-1
                                           d     = boson_degrees_of_freedom-1
                                           p     = fermi_degrees_of_freedom-1
                                           n_size= inner_degrees_of_freedom&
                                                 * inner_degrees_of_freedom
                                           a_size= boson_degrees_of_freedom* n_size
                                           f_size= fermi_degrees_of_freedom*(n_size-1)

!                 write(temp_file_name,"(2(a2,i2.2))") ":n",inner_degrees_of_freedom,&
!                                                      ":d",boson_degrees_of_freedom

!                 conf_file_name&
!                =conf_file_name//trim(temp_file_name)

!                 if(fermions_included_in) then

!                    write(temp_file_name,"(a2,i2.2)") ":p",inner_degrees_of_freedom

!                    conf_file_name&
!                   =conf_file_name//trim(temp_file_name)

!             end if!fermions_included_in

                  write(*,   *               )

                  if(massive_deformations) then

                     write(*,"(2a)",advance="no") "boson_epsilon: ",t_yellow
                      read(*,   *               )  boson_epsilon
                     write(*,"( a)",advance="no")                   t_normal

!                    write(temp_file_name,"(a2,f4.2,a2)") ":b",boson_epsilon,":m"

!                    conf_file_name&
!                   =conf_file_name//trim(temp_file_name)

                     write(*,   *               )

                     if(allocated(boson_mass)) deallocate(boson_mass)
                                                 allocate(boson_mass(0:boson_degrees_of_freedom-1))

                     do mu=0,boson_degrees_of_freedom-1,+1

                        write(*,"(a,i2.2,2a)",advance="no") "boson_mass[",mu+1,"]: ",t_yellow
                         read(*,          *               )  boson_mass(  mu    )
                        write(*,"(        a)",advance="no")                          t_normal

!                       write(temp_file_name,"(sp,f4.1)") log2(boson_mass(mu))

!                       conf_file_name&
!                      =conf_file_name//trim(temp_file_name)

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     write(*,          *               )

                     if(fermions_included_in) then

                        write(*,"(2a)",advance="no") "fermi_mass: ",t_yellow
                         read(*,   *               )  fermi_mass
                        write(*,"( a)",advance="no")                t_normal

!                       write(temp_file_name,"(a2,f4.2)") ":f",fermi_mass

!                       conf_file_name&
!                      =conf_file_name//trim(temp_file_name)

                        write(*,   *               )

              end    if!fermions_included_in

              end if!massive_deformations

                  if(allocated(a)) deallocate(a)
                                     allocate(a(0:inner_degrees_of_freedom-1,&
                                                0:inner_degrees_of_freedom-1,&
                                                0:boson_degrees_of_freedom-1))

                  if(fermions_included_in) then

# 276


                     if(allocated(f)) deallocate(f)
                                        allocate(f(0:f_size-1))

                     if(allocated(m_eigenvalues_)) deallocate(m_eigenvalues_)
                                                     allocate(m_eigenvalues_(0:f_size-1))

                     if(allocated( m )) deallocate( m )
                                          allocate( m (0:f_size-1,&
                                                       0:f_size-1))
                     if(allocated(cm )) deallocate(cm )
                                          allocate(cm (0:f_size-1,&
                                                       0:f_size-1))
                     if(allocated(cmm)) deallocate(cmm)
                                          allocate(cmm(0:f_size-1,&
                                                       0:f_size-1))

                     if(allocated(ma)) deallocate(ma)
                                         allocate(ma(0:f_size-1,&
                                                     0:f_size-1,0:inner_degrees_of_freedom-1,&
                                                                0:inner_degrees_of_freedom-1,&
                                                                0:boson_degrees_of_freedom-1))



              end if!fermions_included_in


        end subroutine  read_field_parameters!


            function boson_noise(standard_deviation)


                  implicit none


                  real(08),intent(inout)::standard_deviation

                  complex(08),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1)::boson_noise

                  integer::mu


                  do mu=0,boson_degrees_of_freedom-1,+1

                     call random_number(boson_noise(:,:,mu))

                                                                           boson_noise(:,:,mu)   &
                    =standard_deviation*sqrt(                    -log(real(boson_noise(:,:,mu))))&
                                       * exp(.20000e+1_08*pi*im_unit*aimag(boson_noise(:,:,mu)))

                     call make_hermitian(boson_noise(:,:,mu))
                     call make_traceless(boson_noise(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end function boson_noise!standard_deviation


            function fermi_noise(standard_deviation)


                  implicit none


                  real(08),intent(inout)::standard_deviation



                  complex(08),dimension(0:f_size-1)::fermi_noise


                  call random_number(fermi_noise)

                                                                        fermi_noise   &
                 =standard_deviation*sqrt(                    -log(real(fermi_noise)))&
                                    * exp(.20000e+1_08*pi*im_unit*aimag(fermi_noise))

# 380



        end function fermi_noise!standard_deviation





            subroutine make_m()


                  implicit none


                  integer::mu,j2,i2,j1,i1

                  complex(08),dimension(0:n_size-2,&
                                        0:n_size-2)::a_delta

                  m=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     a_delta(:,:)=zero

                     do i2=0,inner_degrees_of_freedom-1,+1

                        do j2=0,inner_degrees_of_freedom-1,+1

                           if(inner_degrees_of_freedom*i2+j2==n_size-1) exit

                           do i1=0,inner_degrees_of_freedom-1,+1

                              do j1=0,inner_degrees_of_freedom-1,+1

                                 if(inner_degrees_of_freedom*i1+j1==n_size-1) exit

                                 a_delta(inner_degrees_of_freedom*i1+j1,&
                                         inner_degrees_of_freedom*i2+j2)= a(j1,i2,mu)*delta(j2,i1)              &
                                                                        - a(j2,i1,mu)*delta(j1,i2)              &
                                                                        -(a(n ,i2,mu)*delta(j2,n )              &
                                                                        - a(j2,n ,mu)*delta(n ,i2))*delta(i1,j1)&
                                                                        -(a(j1,n ,mu)*delta(n ,i1)              &
                                                                        - a(n ,i1,mu)*delta(j1,n ))*delta(i2,j2)

              end             do!j1=0,inner_degrees_of_freedom-1,+1

              end          do!i1=0,inner_degrees_of_freedom-1,+1

              end       do!j2=0,inner_degrees_of_freedom-1,+1

              end    do!i2=0,inner_degrees_of_freedom-1,+1

                     m(:,:)&
                    =m(:,:)+(gamma(:,:,mu).x.a_delta(:,:))

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  if(massive_deformations) then

                     do i2=0,inner_degrees_of_freedom-1,+1

                        do j2=0,inner_degrees_of_freedom-1,+1

                           if(inner_degrees_of_freedom*i2+j2==n_size-1) exit

                           do i1=0,inner_degrees_of_freedom-1,+1

                              do j1=0,inner_degrees_of_freedom-1,+1

                                 if(inner_degrees_of_freedom*i1+j1==n_size-1) exit

                                 a_delta(n*i1+j1,n*i2+j2)=delta(j1,i2)&
                                                         *delta(j2,i1)

              end             do!j1=0,inner_degrees_of_freedom-1,+1

              end          do!i1=0,inner_degrees_of_freedom-1,+1

              end       do!j2=0,inner_degrees_of_freedom-1,+1

              end    do!i2=0,inner_degrees_of_freedom-1,+1

                     select case(boson_degrees_of_freedom)

                     case(6)

                        m(:,:)&
                       =m(:,:)+(gamma(:,:,d).x.a_delta(:,:))*fermi_mass

              end    select!case(boson_degrees_of_freedom)

              end if!massive_deformations


        end subroutine make_m!


            subroutine make_cm()


                  implicit none


                  cm=conjugate(m)


        end subroutine make_cm!


            subroutine make_cmm()


                  implicit none


                  cmm=hermitian(cm.o.m)


        end subroutine make_cmm!


            subroutine make_ma()


                  implicit none


                  integer::i ,j ,mu,j2,i2,a2,j1,i1,a1

                  complex(08),dimension(0:n_size-1,&
                                        0:n_size-1)::delta_3


                  do j=0,inner_degrees_of_freedom-1,+1

                     do i=0,inner_degrees_of_freedom-1,+1

                           do i2=0,inner_degrees_of_freedom-1,+1

                              do j2=0,inner_degrees_of_freedom-1,+1

                                 if(inner_degrees_of_freedom*i2+j2==n_size-1) exit

                                 do i1=0,inner_degrees_of_freedom-1,+1

                                    do j1=0,inner_degrees_of_freedom-1,+1

                                       if(inner_degrees_of_freedom*i1+j1==n_size-1) exit

                                       delta_3(inner_degrees_of_freedom*i1+j1,&
                                               inner_degrees_of_freedom*i2+j2)= delta(j1,j)*delta(i,i2)*delta(j2,i1)              &
                                                                              - delta(j2,j)*delta(i,i1)*delta(j1,i2)              &
                                                                              -(delta(n ,j)*delta(i,i2)*delta(j2,n )              &
                                                                              - delta(j2,j)*delta(i,n )*delta(n ,i2))*delta(i1,j1)&
                                                                              -(delta(j1,j)*delta(i,n )*delta(n ,i1)              &
                                                                              - delta(n ,j)*delta(i,i1)*delta(j1,n ))*delta(i2,j2)

              end                   do!j1=0,inner_degrees_of_freedom-1,+1

              end                do!i1=0,inner_degrees_of_freedom-1,+1

              end             do!j2=0,inner_degrees_of_freedom-1,+1

              end          do!i2=0,inner_degrees_of_freedom-1,+1

                        do mu=0,boson_degrees_of_freedom-1,+1

                           ma(:,:,i,j,mu)=gamma(:,:,mu).x.delta_3(:,:)

              end       do!mu=0,boson_degrees_of_freedom-1,+1

              end    do!i=0,inner_degrees_of_freedom-1,+1

              end do!j=0,inner_degrees_of_freedom-1,+1


        end subroutine make_ma!


            subroutine make_m_eigenvalues


                  implicit none


                  call geev(m,m_eigenvalues_)


        end subroutine make_m_eigenvalues!


            subroutine update_fermion_matrix()


                  implicit none


                  call make_m()
                  call make_cm()
                  call make_cmm()

                  call make_ma()

                  call make_m_eigenvalues()


        end subroutine update_fermion_matrix!





  end module fields



# 12 "main/../ikkt/complex_langevin.F90" 2



# 1 "main/../ikkt/../tools/conjugate_gradient.F90" 1



# 1 "main/../ikkt/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/../tools/conjugate_gradient.F90" 2

# 1 "main/../ikkt/../tools/../tensor/tensor.F90" 1
# 1274

# 7 "main/../ikkt/../tools/conjugate_gradient.F90" 2


      module conjugate_gradient_method


            use::tensor_type


            implicit none


            real(08),parameter::tolerance=.00000000001


            interface conjugate_gradient

                  module procedure conjugate_gradient_K

        end interface conjugate_gradient


      contains


            subroutine conjugate_gradient_K(a,b,x)


                  implicit none


                  complex(08),dimension( :               ),intent(inout)::b
                  complex(08),dimension(0:size(b,dim=1)-1,&
                                        0:size(b,dim=1)-1),intent(inout)::a
                  complex(08),dimension(0:size(b,dim=1)-1),intent(inout)::x

                  complex(08),dimension(0:size(b,dim=1)-1)              ::r,&
                                                                          p

                  real(08)::cb,&
                            ca,norm_r_old,&
                               norm_r_new

                  r=b-(a.o.x)
                  p=       r

                  do

                     norm_r_old=norm(r)

                     if(norm_r_new<tolerance) exit

                     cb=-norm_r_old/norm(r,a)

                     x=x-cb*     p
                     r=r+cb*(a.o.p)

                     ca=norm_r_new&
                       /norm_r_old

                     p=r+ca*     p

              end do

                  print *,"success"

        end subroutine conjugate_gradient_K!a,b,x


  end module conjugate_gradient_method



# 16 "main/../ikkt/complex_langevin.F90" 2

# 22


# 1 "main/../ikkt/../ikkt/gauge_cooling.F90" 1



# 1 "main/../ikkt/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/../ikkt/gauge_cooling.F90" 2

# 1 "main/../ikkt/../ikkt/../tensor/tensor.F90" 1
# 1274

# 7 "main/../ikkt/../ikkt/gauge_cooling.F90" 2

# 1 "main/../ikkt/../ikkt/../ikkt/fields.F90" 1
# 597

# 9 "main/../ikkt/../ikkt/gauge_cooling.F90" 2

# 1 "main/../ikkt/../ikkt/../tools/brent_minimization.F90" 1



# 1 "main/../ikkt/../ikkt/../system/precision.F90" 1
# 74

# 5 "main/../ikkt/../ikkt/../tools/brent_minimization.F90" 2


!     BRENT MINIMIZATION
!
!     Given a function f, and given a bracketing triplet of abscissas ax, bx, cx (such that bx is between ax and cx, and f(bx) is
!     less than both f(ax) and f(cx) ), this routine isolates the minimum to a fractional precision of about tol using brent’s
!     method. The abscissa of the minimum is returned as xmin, and the minimum function value is returned as brent, the returned
!     function value.
!
!     parameters:
!     maximum allowed number of iterations
!     golden ratio
!     a small number that protects against trying to achieve fractional accuracy for a minimum that happens to be exactly zero


      module brent_minimization


            implicit none


            interface brent

               module procedure brent_K

        end interface brent


      contains


            function brent_K(ax,bx,cx,f,tol,xmin)


                  implicit none


                  integer ::itmax;parameter(itmax=100)

                  real(08)::brent_K

                  real(08)::ax
                  real(08)::bx
                  real(08)::cx
                  real(08)::tol
                  real(08)::xmin
                  real(08)::f;external f

                  real(08)::cgold;parameter(cgold=(+.30000e+1_08  &
                                             -sqrt(+.50000e+1_08))&
                                             /     +.20000e+1_08)
                  real(08)::zeps ;parameter(zeps = +.10000e-9_08)
                  integer ::iter
                  real(08)::a
                  real(08)::b
                  real(08)::d
                  real(08)::e
                  real(08)::etemp
                  real(08)::fu
                  real(08)::fv
                  real(08)::fw
                  real(08)::fx
                  real(08)::p
                  real(08)::q
                  real(08)::r
                  real(08)::tol1
                  real(08)::tol2
                  real(08)::u
                  real(08)::v
                  real(08)::w
                  real(08)::x
                  real(08)::xm

!                 a and b must be in ascending order, though the input abscissas need not be.

                  a=min(ax,cx)
                  b=max(ax,cx)
                  v=bx

!                 Initializations...

                  w=v
                  x=v

!                 This will be the distance moved on the step before last.

                  e=+.00000e+0_08
                  fx=f(x)
                  fv=fx
                  fw=fx

!                 Main program loop.

                  do iter=1,itmax

                     xm=+.50000e+0_08*(a+b)
                     tol1=tol*abs(x)+zeps
                     tol2=+.20000e+1_08*tol1

!                    Test for done here.
                     if(abs(x-xm).le.(tol2-.50000e+0_08*(b-a))) then

                        goto 103

              end    if!abs(x-xm).le.(tol2-.50000e+0_08*(b-a))

!                    Construct a trial parabolic fit.

                     if(abs(e).gt.tol1) then

                        r=(x-w)*(fx-fv)
                        q=(x-v)*(fx-fw)
                        p=(x-v)*q-(x-w)*r
                        q=+.20000e+1_08*(q-r)

                        if(q.gt.+.00000e+0_08) then

                           p=-p

              end       if!q.gt.0.0

                        q=abs(q)
                        etemp=e
                        e=d

                        if(abs(p).ge.abs(+.50000e+0_08*q*etemp).or.p.le.q*(a-x).or.p.ge.q*(b-x)) then

                           goto 101

              end       if!abs(p).ge.abs(+.50000e+0_08*q*etemp).or.p.le.q*(a-x).or.p.ge.q*(b-x)

!                       The above conditions determine the acceptability of the parabolic fit. here it is O.3.:
!                       Take the parabolic step.

                        d=p/q
                        u=x+d

                        if(u-a.lt.tol2.or.b-u.lt.tol2) then

                           d=sign(tol1,xm-x)

              end       if!u-a.lt.tol2or.b-u.lt.tol2

!                       Skip over the golden section step.

                        goto 102

              end    if!abs(e).gt.tol1

!                    We arrive here for a golden section step, which we take into the larger of the two segments.

              101    if(x.ge.xm) then

                        e=a-x

                     else

                        e=b-x

              end    if!x.ge.xm

!                    Take the golden section step.

                     d=cgold*e

!                    Arrive here with d computed either from parabolic fit, or else from golden section.

              102    if(abs(d).ge.tol1) then

                        u=x+d

                     else

                        u=x+sign(tol1,d)

              end    if!abs(d).ge.tol1

                     fu=f(u)

!                    This is the one function evaluation per iteration, and now we have to decide what to do with our evaluation.
!                    Housekeeping follows:

                     if(fu.le.fx) then

                        if(u.ge.x) then

                           a=x

                        else

                           b=x

              end       if!u.ge.x

                        v=w
                        fv=fw
                        w=x
                        fw=fx
                        x=u
                        fx=fu

                     else

                        if(u.lt.x) then

                           a=u

                        else

                           b=u

              end       if!u.lt.x

                        if(fu.le.fw.or.w.eq.x) then

                           v=w
                           fv=fw
                           w=u
                           fw=fu

                        else

                           if(fu.le.fv.or.v.eq.x.or.v.eq.w) then

                              v=u
                              fv=fu

              end          if!fu.le.fv.or.v.eq.x.or.v.eq.w

              end       if!fu.le.fw.or.w.eq.x

              end    if!fu.le.fx

!                    Done with housekeeping. Back for another iteration.

              end do!iter=1,itmax

!                 Pause ’brent exceed maximum iterations’.
!                 Arrive here ready to exit with best values.

              103 xmin=x
                  brent_K=fx


                  return


        end function brent_K


  end module brent_minimization



# 11 "main/../ikkt/../ikkt/gauge_cooling.F90" 2


      module gauge_cooling


            use::lapack95,only:heev

            use::tensor_type

            use::fields

            use::brent_minimization


            implicit none


            character(*),parameter,private::           format_cooling="(sp,2(x,e24.17e3))"
            character(*),parameter,private::text_field_format_cooling="(   2(x,a24     ))"

            real(08),parameter,private::cooling_tolerance=.00000000001

            real(08),private::min_alpha=.00000000001*1
            real(08),private::mid_alpha=.00000000001*10
            real(08),private::max_alpha=.00000000001*100

            logical,public::gauge_cooling_active=.false.

            complex(08),dimension( :                          ,&
                                   :                          ),allocatable,private::h
            real(   08),dimension( :                          ),allocatable,private::h_eigenvalues

            private::make_cooler
            private::h_diagonalized
            private::       hermiticity_norm
            private::guided_hermiticity_norm
            private::find_cooling_range

            public::apply_cooling

            public::print_guided_hermiticity_norm


      contains


            subroutine make_cooler()


                  implicit none


                  integer::mu


                  if(     allocated(h).and.(size(h,dim=1)/=inner_degrees_of_freedom&
                                        .or.size(h,dim=2)/=inner_degrees_of_freedom)) deallocate(h)
                  if(.not.allocated(h)) allocate(h(      0:inner_degrees_of_freedom-1,&
                                                         0:inner_degrees_of_freedom-1))

                  if(     allocated(h_eigenvalues).and. size(h_eigenvalues)/=inner_degrees_of_freedom) deallocate(h_eigenvalues)
                  if(.not.allocated(h_eigenvalues)) allocate(h_eigenvalues(0:inner_degrees_of_freedom-1))

                  h=+.00000e+0_08

                  do mu=0,boson_degrees_of_freedom-1,+1

                     h(:,:)&
                    =h(:,:)+(conjugate(a(:,:,mu)).commutation.a(:,:,mu))/inner_degrees_of_freedom

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  call heev(h,h_eigenvalues,jobz='v')


        end subroutine make_cooler!


            function h_diagonalized(alpha)


                  implicit none


                  real(08),intent(inout)::alpha

                  complex(08),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1)::h_diagonalized


                  h_diagonalized=h.o.matrix(cmplx(exp(alpha*h_eigenvalues),kind=08)).o.conjugate(h)


        end function h_diagonalized!alpha


            function hermiticity_norm()


                  implicit none


                  real(08)::hermiticity_norm

                  integer::mu


                  hermiticity_norm=+.00000e+0_08

                  do mu=0,boson_degrees_of_freedom-1,+1

                     hermiticity_norm&
                    =hermiticity_norm+norm(a(:,:,mu)-conjugate(a(:,:,mu)))*inner_degrees_of_freedom/4

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end function hermiticity_norm!


            function guided_hermiticity_norm(alpha)


                  implicit none


                  real(08),intent(inout)::alpha

                  real(08)::guided_hermiticity_norm

                  integer::mu


                  guided_hermiticity_norm=+.00000e+0_08

                  do mu=0,boson_degrees_of_freedom-1,+1

                     guided_hermiticity_norm&
                    =guided_hermiticity_norm+norm((h_diagonalized(+alpha).o.          a(:,:,mu) .o.h_diagonalized(-alpha)) &
                                                 -(h_diagonalized(-alpha).o.conjugate(a(:,:,mu)).o.h_diagonalized(+alpha)))&
                    *inner_degrees_of_freedom/4

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end function guided_hermiticity_norm!alpha


            subroutine find_cooling_range()


                  implicit none


                  do while(guided_hermiticity_norm(mid_alpha)>guided_hermiticity_norm(max_alpha))

                     min_alpha=mid_alpha
                     mid_alpha=max_alpha
                     max_alpha=max_alpha*10

              end do!while(guided_hermiticity_norm(mid_alpha)>guided_hermiticity_norm(max_alpha))


        end subroutine find_cooling_range!


            subroutine apply_cooling()


                  implicit none


                  real(08)::alpha_min
                  real(08)::min_hermiticity_norm

                  integer::mu


                  call make_cooler()
                  call find_cooling_range()

                  call print_guided_hermiticity_norm(100)

                  min_hermiticity_norm=brent(min_alpha,&
                                             mid_alpha,&
                                             max_alpha,guided_hermiticity_norm,cooling_tolerance,alpha_min)

                   open(newunit=mu,file="hermiticity.norm.minimum")
                  write(        mu,                 format_cooling) alpha_min,min_hermiticity_norm
                  close(        mu)

                  do mu=0,boson_degrees_of_freedom-1,+1

                     a(:,:,mu)=h_diagonalized(+alpha_min).o.a(:,:,mu).o.h_diagonalized(-alpha_min)

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine apply_cooling!


            subroutine print_guided_hermiticity_norm(step_100)


                  implicit none


                  integer,intent(inout)::step_100

                  integer::unit,i,steps

                  real(08)::step

                  steps=(nint(max_alpha/min_alpha)+1)*step_100;step=min_alpha/step_100

                   open(unit,file="hermiticity.norm")

                  do i=0,steps-1,+1

                     write(unit,format_cooling) step*i,guided_hermiticity_norm(step*i)

              end do!i=0,steps-1,+1

                  write(unit,format_cooling) step*i,guided_hermiticity_norm(max_alpha)
                  close(unit)


        end subroutine print_guided_hermiticity_norm!step_100


            subroutine eject_gauge_cooler()


                  implicit none


                  if(allocated(h)) deallocate(h)

                  if(allocated(h_eigenvalues)) deallocate(h_eigenvalues)


        end subroutine eject_gauge_cooler!


  end module gauge_cooling



# 25 "main/../ikkt/complex_langevin.F90" 2


      module complex_langevin


            use::tensor_type

            use::monte_carlo

            use::conjugate_gradient_method

            use::constants
            use::fields

# 43


            use::gauge_cooling


            implicit none


            complex(08),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::drift
            complex(08),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::noise

            public::boot_langevin
            public::langevin_step
            public::wrap_langevin

            public::make_drift
            public::make_noise

            public::drift_norm


      contains


            subroutine boot_langevin()


                  implicit none


                  if(configuration_loaded) then

                     call load_monte_carlo()
                     call load_fields()

                  else

                     call make_monte_carlo()
                     call make_fields()

              end if!configuration_loaded

                  call make_constants(inner_degrees_of_freedom,&
                                      boson_degrees_of_freedom)

                  if(allocated(drift)) deallocate(drift)
                                         allocate(drift(0:inner_degrees_of_freedom-1,&
                                                        0:inner_degrees_of_freedom-1,&
                                                        0:boson_degrees_of_freedom-1))
                  if(allocated(noise)) deallocate(noise)
                                         allocate(noise(0:inner_degrees_of_freedom-1,&
                                                        0:inner_degrees_of_freedom-1,&
                                                        0:boson_degrees_of_freedom-1))


        end subroutine boot_langevin!


            subroutine langevin_step()


                  implicit none


                  call make_drift()
                  call make_noise()

                  a&
                 =a+drift*     weight(step) &
                   +noise*sqrt(weight(step))

                  if(gauge_cooling_active) call apply_cooling()
                  if(timestep_is_adaptive) call agnostically_adapt_time_step(drift_norm())


        end subroutine langevin_step!


            subroutine wrap_langevin()


                  implicit none


                  call save_monte_carlo()
                  call save_fields()


        end subroutine wrap_langevin!


            subroutine make_drift()


                  implicit none


                  integer::mu,nu,i,j


                  drift=zero

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

                        drift(:,:,mu)&
                       =drift(:,:,mu)-((a(:,:,mu).commutation.a(:,:,nu)).commutation.a(:,:,nu))*inner_degrees_of_freedom

              end    do!nu=0,boson_degrees_of_freedom-1,+1

                     if(massive_deformations) then

                        drift(:,:,mu)&
                       =drift(:,:,mu)-boson_mass(mu)*a(:,:,mu)*boson_epsilon*inner_degrees_of_freedom

              end    if!massive_deformations

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  if(fermions_included_in) then

                     f=zero!fermi_noise(.10000e+1_08)



                     call update_fermion_matrix()

                     do mu=0,boson_degrees_of_freedom-1,+1

                        do j=0,inner_degrees_of_freedom-1,+1

                           do i=0,inner_degrees_of_freedom-1,+1

                              call conjugate_gradient(cmm(:,:),cm(:,:).o.fermi_noise(.10000e+1_08),f(:))

                              drift(i,j,mu)&
                             =drift(i,j,mu)&
                             +determinant_degree(boson_degrees_of_freedom)*(fermi_noise(.10000e+1_08).c.ma(:,:,i,j,mu).o.f(:))

              end          do!i=0,inner_degrees_of_freedom-1,+1

              end       do!j=0,inner_degrees_of_freedom-1,+1

              end    do!mu=0,boson_degrees_of_freedom-1,+1

# 200


              end if!fermions_included_in

                  do mu=0,boson_degrees_of_freedom-1,+1

!                    call make_hermitian(drift(:,:,mu))
                     call make_traceless(drift(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_drift!


            subroutine make_noise()


                  implicit none


                  integer::mu


                  noise=boson_noise(.20000e+1_08)


        end subroutine make_noise!


            function drift_norm()


                  implicit none


                  real(08)::drift_norm

                  integer::mu


                  drift_norm=+.00000e+0_08

                  do mu=1,boson_degrees_of_freedom-1,+1

                     drift_norm&
                    =drift_norm+norm(drift(:,:,mu))

              end do!mu= ,boson_degrees_of_freedom-1,+1

                  drift_norm&
                 =drift_norm/a_size


        end function drift_norm!


            subroutine eject_complex_langevin()


                  implicit none


                  call eject_gauge_cooler()
                  call eject_constants()

                  if(allocated(drift)) deallocate(drift)
                  if(allocated(drift)) deallocate(noise)


        end subroutine eject_complex_langevin


  end module complex_langevin



# 49 "main/main.F90" 2
!     include "../ikkt/observables.F90"

!     define BLAS
!     define OPTIMAL


      program main


!           use::mathematical_constants

            use::get_options
!           use::text_formatting
            use::version

!           use::tensor_type

!           use::random_number_generator
!           use::average_type
!           use::time_type
            use::monte_carlo

!           use::insertion_sort
!           use::brent_minimization
!           use::conjugate_gradient_method

# 79


!           use::constants
!           use::fields
!           use::complex_langevin
            use::gauge_cooling
            use::observables


            implicit none



            character(*),parameter::data_path_name="data/"

            character(:),allocatable::base_file_name


!           call signal_actions(eject_main)

            call   begin_ikkt_simulation()
            call perform_ikkt_simulation()
            call     end_ikkt_simulation()


      contains


            subroutine begin_ikkt_simulation()


                  implicit none


                  call read_options_and_arguments()
                  call boot_langevin()


        end subroutine begin_ikkt_simulation!


            subroutine perform_ikkt_simulation()


                  implicit none


                  integer::unit


                   open(newunit=unit,file=meas_file_name)

                  do while(life<=life_time)

                     if(timestep_is_adaptive) call agnostically_adapt_time_step(drift_norm())

                     life&
                    =life+step

                     call langevin_step()

                     if(measure_time_skipped.and.skip<=life) then

                        skip&
                       =skip+time_skip

                        call print_observables()

              end    if!measure_time_skipped.and.skip<=life

                     if(auto_save_field_conf.and.auto<=life) then

                        auto&
                       =auto+time_skip

!                       call auto_save_configuration()

              end    if!auto_save_field_conf.and.auto<=life

              end do!while(life<=life_time)

                  close(        unit                    )


        end subroutine perform_ikkt_simulation!


            subroutine end_ikkt_simulation


                  implicit none


                  integer::unit


                  call wrap_langevin()
                  call write_version()


        end subroutine end_ikkt_simulation!


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin=>input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=>error_unit


                  implicit none


                  character             ::option_character ! currently parsed option
                  character(len_argname)::option_argument  ! currently parsed argument value if not an option
                  integer               ::argument_length  ! currently parsed argument true length
                  integer               ::status           ! stat of parsing option
                  integer               ::argument_index   ! current argument running index starting where the options end
                  integer               ::argument_number  ! number of remaining indices options aside

                  type(option)::options(0:6) ! list of long options

                  integer::index,time_stamp(8)


                  options(1)=option("variable-timestep",.false.,'t',"Use variable timestep with fixed timestep as average. This &
                                                                    &alelviates drift divergence due to instabilities in either a &
                                                                    &poorly chosen starting configuration or a large timestep.","")
                  options(2)=option("noisy-start-field",.false.,'a',"Start with a (gausssian) noisy (hot) initial configuration &
                                                                    &instead of the default zero (cold) initial configuration. &
                                                                    &Overriden by "//t_bold//"-c"//s_bold//" when loading a &
                                                                    &previously saved configuration.","")
                  options(3)=option(    "load-save"    ,.false.,'c',"Load simulation stats and field configuration from a previous &
                                                                    &simulation. This helps in storing thermalized configurations, &
                                                                    &and reusing instead of repeating thermalizations. Overrides "&
                                                                    &//t_bold//"-a"//s_bold//" when not starting a simulation from &
                                                                    &scratch.","")
                  options(4)=option( "run-observables" ,.false.,'o',"Load a sample of configurations for re-calculating &
                                                                    &observables, to prevent re-runnning simulations for new &
                                                                    &calculations.","configuration file")
                  options(5)=option("fermions-included",.false.,'f',"Include fermions in simulation, for simulating the full IKKT &
                                                                    &model.","")
                  options(6)=option(  "gauge-cooling"  ,.false.,'g',"Applythe standard gauge-cooling post-processing to &
                                                                    &configurations after each langevin step. This alleviates &
                                                                    &simulation journeys in the imaginary direction.","")
                  options(7)=option("mass-deformations",.false.,'m',"Include massive deformations in modifying the IKKT drift used &
                                                                    &in the Complex Langevin method. In particular for the boson &
                                                                    &model, a new coupling parameter is added, along with masses &
                                                                    &for the gauge bosons. In the full model with fermions, a &
                                                                    &fermionic mass is added as well.","")
                  options(0)=option("help"             ,.false.,'h',"Print this help screen.","")

                  do

                     call getopt(options="taco:fgmh"     ,&
                                longopts=options         ,&
                                 optchar=option_character,&
                                  optarg=option_argument ,&
                                  arglen=argument_length ,&
                                    stat=status          ,&
                                  offset=argument_index  ,&
                                  remain=argument_number)

                     if(status==1) exit

                     select case(option_character)

                     case('t'); timestep_is_adaptive=.true.
                     case('a'); start_field_is_noisy=.true.
                     case('c'); configuration_loaded=.true.; call read_version()
                     case('o'); running_measurements=.true.; call read_version()
                     case('f'); fermions_included_in=.true.
                     case('g'); gauge_cooling_active=.true.
                     case('m'); massive_deformations=.true.

                     case default

                        write(*,    *   )
                        write(*,"(*(a))") "Usage: ",                          t_bold,"BINARY_NAME",     s_bold,&
                                          " [",                               t_bold,"OPTIONS",         s_bold,&
                                          "... ] ",                           t_bold,"BASE_OUTPUT_NAME",s_bold
                        write(*,    *   )
                        write(*,"(*(a))") "Simulation compiled with ",        t_bold,"-o BINARY_NAME",  s_bold," plus preprocessor."
                        write(*,"(*(a))") "Simulation file names start with ",t_bold,"BASE_OUTPUT_NAME",s_bold," plus extension."
                        write(*,    *   )
                        write(*,"(*(a))") "Mandatory arguments to long options are mandatory for short options too."
                        write(*,    *   )

                        do index=1,size(options)-1,+1

                           call print_opt(options(index),stderr)

              end       do!index=1,size(options)-1,+1

                        write(*,"(*(a))") "All options are optional switches for the simulation with default meaning in"
                        write(*,"(*(a))") "their absence. If you run simulation via shell script that ports all available"
                        write(*,"(*(a))") "options, use shell script's name as ",t_bold,"BINARY_NAME",s_bold," instead."
                        write(*,    *   )
                        write(*,"(*(a))") "If you wish to measure in every step of the simulation, input zero ",t_bold,&
                                          "average_skip"                                                       ,s_bold,"."
                        write(*,    *   )

                        stop

              end    select!case(option_character)

              end do

                  write(*,*)

                  call get_command_argument(argument_index+1,option_argument,argument_length,status)

                  select case(status)

                  case(-1)

                     stop "Error: argument was trimmed"

                  case( 0)

                     continue

                  case default

                     stop "Error: a base file name must be provided to store simulation status"

              end select!case(status)

                  base_file_name=trim(adjustl(option_argument))

                  call execute_command_line("mkdir --parents -- "//trim(data_path_name))

                  base_file_name=trim(data_path_name)//base_file_name

                  call date_and_time(values=time_stamp)

                  base_file_name=trim(base_path_name)//"."//trim(time_stamp(1))&
                                                     //"-"//trim(time_stamp(2))&
                                                     //"-"//trim(time_stamp(3))&
                                                     //"."//trim(time_stamp(5))&
                                                     //":"//trim(time_stamp(6))&
                                                     //":"//trim(time_stamp(7))

                  seed_file_name=base_file_name//".seed";!write(*,"(a)") seed_file_name
                  time_file_name=base_file_name//".time";!write(*,"(a)") time_file_name
                  conf_file_name=base_file_name//".conf";!write(*,"(a)") conf_file_name
                  meas_file_name=base_file_name//".meas";!write(*,"(a)") meas_file_name
                  save_file_name=base_file_name//".save";!write(*,"(a)") meas_file_name


        end subroutine read_options_and_arguments!


            subroutine eject_main()


                  implicit none

                  call eject_complex_langevin()

                  call end_ikkt_simulation()

                  print *
                  print *,"Used memory deallocated."

                  stop


        end subroutine eject_main!


  end program main
