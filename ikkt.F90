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

# 1 "main/../system/text_format.F90" 1 




      module text_formatting


            implicit none


            character(2),parameter::escape0x1B=achar(27)//'['
            character(1),parameter::terminator=           'm'

            character(4),parameter::t_normal   =escape0x1B// "0"//terminator,&
                                    t_bold     =escape0x1B// "1"//terminator,&
                                    t_italics  =escape0x1B// "3"//terminator,&
                                    t_underline=escape0x1B// "4"//terminator,&
                                    t_blink    =escape0x1B// "5"//terminator,&
                                    t_highlight=escape0x1B// "7"//terminator,&
                                    t_hide     =escape0x1B// "8"//terminator

            character(5),parameter::s_bold     =escape0x1B//"22"//terminator,&
                                    s_italics  =escape0x1B//"23"//terminator,&
                                    s_underline=escape0x1B//"24"//terminator,&
                                    s_blink    =escape0x1B//"25"//terminator,&
                                    s_highlight=escape0x1B//"27"//terminator,&
                                    s_hide     =escape0x1B//"28"//terminator

            character(5),parameter::t_black    =escape0x1B//"30"//terminator,&
                                    t_red      =escape0x1B//"31"//terminator,&
                                    t_green    =escape0x1B//"32"//terminator,&
                                    t_yellow   =escape0x1B//"33"//terminator,&
                                    t_blue     =escape0x1B//"34"//terminator,&
                                    t_purple   =escape0x1B//"35"//terminator,&
                                    t_cyan     =escape0x1B//"36"//terminator,&
                                    t_white    =escape0x1B//"37"//terminator


      contains


            subroutine write_par(unit,text_width,text)


                  implicit none


                  integer     ,intent(in   )::unit,&
                                              text_width
                  character(*),intent(in   )::text

                  integer::c0,&
                           c1,text_length


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



# 17 "main/main.F90" 2 
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



# 18 "main/main.F90" 2 
# 1 "main/../system/getopts.F90" 1 



# 1 "main/../system/text_format.F90" 1 
# 104

# 5 "main/../system/getopts.F90" 2 


      module get_options


            use::text_formatting


            implicit none


            type option

               character(100)::name ! Long name.
               logical::has_arg ! Does the option require an argument?
               character::chr ! Corresponding short name.
               character(500)::descr ! Description.
               character(20)::argname ! Argument name,if required.

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

!                 If stat is 0 and the parsed option requires an argument,optarg contains the first len(optarg) (but at most 500)
!                 characters of that argument. Otherwise its value is undefined. If the arguments length exceeds 500 characters and
!                 err is .true.,a warning is issued.

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
                  character(500),save::arg

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
                                 -side_width  ,500)

                     else

                        c2=min(c1+term_width  &
                                 -name_width-2,500)

              end    if!line_count>1

!                    if not at the end of the whole string

                     if(c2/=500) then

!                       find the end of a word

                        do

                           if(opt%descr(c2:c2)==" ") then

                              exit

              end          if!opt%descr(c2:c2)==" "

                           c2=c2-1

              end       do

              end    if!c2/=500

                     write(unit,"(a)") opt%descr(c1:c2-1)
                                                 c1=c2+1
                     line_count&
                    =line_count+1

              end do

                  write(unit,*)


        end subroutine


  end module get_options



# 19 "main/main.F90" 2 
# 1 "main/../system/precision.F90" 1 



!                                             +127 i04.03
!                                           +32767 i06.05
!                                      +2147483647 i11.10
!                             +9223372036854775807 i20.19
!         +170141183460469231731687303715884105727 i40.39













!                                     +.____       f06.04
!                                    +._____e+_    e10.05e1
!                                +.340282347e+39   e15.09e2
!                        +.17976931348623157e+309  e24.17e3
!     +.118973149535723176508575932662800702e+4933 e44.36e4














































# 20 "main/main.F90" 2 

!     include "../tensor/tensor.F90"

!     include "../monte_carlo/random_number_generator.F90"
!     include "../monte_carlo/average.F90"
!     include "../monte_carlo/time.F90"
# 1 "main/../monte_carlo/monte_carlo.F90" 1 



# 1 "main/../system/precision.F90" 1 
# 72

# 5 "main/../monte_carlo/monte_carlo.F90" 2 
# 1 "main/../monte_carlo/../system/text_format.F90" 1 
# 104

# 6 "main/../monte_carlo/monte_carlo.F90" 2 

# 1 "main/../monte_carlo/../monte_carlo/random_number_generator.F90" 1 



# 1 "main/../monte_carlo/../system/precision.F90" 1 
# 72

# 5 "main/../monte_carlo/../monte_carlo/random_number_generator.F90" 2 


      module random_number_generator


            implicit none


            character(*),private,parameter::           format_integer="(sp,  x,i11.10   )"
            character(*),private,parameter::text_field_format_integer="(     x,a11      )"

            character(:),allocatable,public::seed_file_name

            integer,                                     private::size_seed
            integer,allocatable,dimension( :           ),private::     seed

            private::prepare_seed

            public::make_seed
            public::load_seed
            public::save_seed


      contains


            subroutine prepare_seed()


                  implicit none


                  call random_seed(size=size_seed)

                  if(allocated(seed)) then

                     if(size(seed)/=size_seed) then

                        deallocate(seed)

              end    if!size(seed)/=size_seed

                  else

                     allocate(seed(0:size_seed))

              end if!allocated(seed)


        end subroutine prepare_seed!


            subroutine make_seed()


                  implicit none


                  call random_seed()


        end subroutine make_seed!


            subroutine load_seed()


                  implicit none


                  integer::unit


                  call prepare_seed()

                   open(newunit=unit,file=seed_file_name)
                   read(        unit,     format_integer) seed
                  close(        unit                    )

                  call random_seed(put=seed)

                  if(allocated(seed)) deallocate(seed)


        end subroutine load_seed!


            subroutine save_seed()


                  implicit none


                  integer::unit


                  call prepare_seed()

                  call random_seed(get=seed)

                   open(newunit=unit,file=seed_file_name)
                  write(        unit,     format_integer) seed
                  close(        unit                    )

                  if(allocated(seed)) deallocate(seed)


        end subroutine save_seed!


  end module random_number_generator



# 8 "main/../monte_carlo/monte_carlo.F90" 2 
# 1 "main/../monte_carlo/../monte_carlo/average.F90" 1 



# 1 "main/../monte_carlo/../system/precision.F90" 1 
# 72

# 5 "main/../monte_carlo/../monte_carlo/average.F90" 2 


      module average_type


            implicit none


            character(*),private,parameter::           format_average_K="(sp,  x,e24.17e3 )"
            character(*),private,parameter::text_field_format_average_K="(     x,a24      )"

            type::average(precision)

               integer,kind::precision

               real(precision),public::weight=+.00000e+0_08
               real(precision),public::value =+.00000e+0_08

            contains

               procedure,private::initialize_average_K;generic::initialize_average=>initialize_average_K

!              procedure,public::weight_K
!              procedure,public::value__K

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

!           interface weight

!              module procedure weight_K

!       end interface weight

!           interface value

!              module procedure value__K

!       end interface value

            interface operator(+)

               module procedure         average_average__plus_K
               module procedure average_average_average__plus_K

        end interface operator(+)

            interface sum

               module procedure average_sum_K

        end interface sum

            interface operator(-)

               module procedure         average_average_minus_K
               module procedure average_average_average_minus_K

        end interface operator(-)

            interface operator(*)

               module procedure real_average_average_times_value_K
               module procedure average_real_average_times_value_K

               module procedure average_average_real_times_value_K

        end interface operator(*)

            interface product

               module procedure average_product_K

        end interface product

            interface operator(/)

               module procedure average_real_average_division_by_K

               module procedure average_average_real_division_by_K

        end interface operator(/)

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


            subroutine initialize_average_K(this,weight,value)


                  implicit none


                  type(average(08)),intent(inout)         ::this
                  real(        08 ),intent(in   ),optional::weight
                  real(        08 ),intent(in   ),optional::value


                  if(present(weight)) this%weight=weight
                  if(present(value )) this%value =value


        end subroutine initialize_average_K!this,weight,value


            function average_constructor_K(weight,value) result(that)


                  implicit none


                  real(        08 ),intent(in   ),optional::weight
                  real(        08 ),intent(in   ),optional::value
                  type(average(08))                       ::that


                  if(present(value)) then

                     if(present(weight)) then

                        call that%initialize_average(weight=weight,value=value)

                     else

                        call that%initialize_average(              value=value)

              end    if!present(weight)

                  else

                     if(present(weight)) then

                        call that%initialize_average(weight=weight            )

                     else

                        call that%initialize_average(                         )

              end    if!present(weight)

              end if!present(value)


        end function average_constructor_K!weight,value


!           function weight_K(this) result(weight)


!                 implicit none


!                 type(average(08)),intent(inout)::this
!                 real(        08 )              ::weight


!                 weight=this%weight


!       end function weight_K!this


!           function value__K(this) result(value )


!                 implicit none


!                 type(average(08)),intent(inout)::this
!                 real(        08 )              ::value


!                 value =this%value


!       end function value__K!this


            function average_average__plus_K(average0) result(average_)


                  implicit none


                  type(average(08)),intent(in   )::average0
                  type(average(08))              ::average_


                  average_%weight&
                = average0%weight
                  average_%value &
                =+average0%value


        end function average_average__plus_K!average0


            function average_average_average__plus_K(average1,average2) result(average_)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2
                  type(average(08))              ::average_


                  average_%weight&
                 =average1%weight&
                 +average2%weight

                  average_%value &
                 =average1%weight&
                 *average1%value &
                 +average2%weight&
                 *average2%value

                  average_%value &
                 =average_%value &
                 /average_%weight


        end function average_average_average__plus_K!average1,average2


            function average_sum_K(average0) result(average_)


                  implicit none


                  type(average(08)),dimension(:),intent(in   )::average0
                  type(average(08))                           ::average_

                      average_%weight&
                 =sum(average0%weight)
                      average_%value &
                 =sum(average0%weight&
                 *    average0%value)

                      average_%value &
                 =    average_%value &
                 /    average_%weight


        end function average_sum_K!average0


            function average_average_minus_K(average0) result(average_)


                  implicit none


                  type(average(08)),intent(in   )::average0
                  type(average(08))              ::average_


                  average_%weight&
                = average0%weight

                  average_%value &
                =-average0%value


        end function average_average_minus_K!average0


            function average_average_average_minus_K(average1,average2) result(average_)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2
                  type(average(08))              ::average_


                  average_%weight&
                 =average1%weight&
                 -average2%weight

                  average_%value &
                 =average1%weight&
                 *average1%value &
                 -average2%weight&
                 *average2%value

                  average_%value &
                 =average_%value &
                 /average_%weight


        end function average_average_average_minus_K!average1,average2


            function real_average_average_times_value_K(real1,average2) result(average_)


                  implicit none


                  real(        08 ),intent(in   )::   real1
                  type(average(08)),intent(in   )::average2
                  type(average(08))              ::average_


                  average_%weight&
                 =average2%weight

                  average_%value =real1&
                 *average2%value


        end function real_average_average_times_value_K!real1,average2


            function average_real_average_times_value_K(average1,real2) result(average_)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2
                  type(average(08))              ::average_


                  average_%weight&
                 =average1%weight

                  average_%value &
                 =average1%value *real2


        end function average_real_average_times_value_K!average1,real2


            function average_average_real_times_value_K(average1,average2) result(real_)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2
                  real(        08 )              ::   real_


                  real_=average1%value&
                       *average2%value


        end function average_average_real_times_value_K!average1,average2


            function average_product_K(average0) result(real_)


                  implicit none


                  type(average(08)),dimension(:),intent(in   )::average0
                  real(        08 )                           ::   real_


                  real_=product(average0%value)


        end function average_product_K!average0


            function average_real_average_division_by_K(average1,real2) result(average_)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2
                  type(average(08))              ::average_


                  average_%weight&
                 =average1%weight

                  average_%value &
                 =average1%value /real2


        end function average_real_average_division_by_K!average1,real2


            function average_average_real_division_by_K(average1,average2) result(real_)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2
                  real(        08 )              ::   real_


                 real_=average1%value&
                      /average2%value


        end function average_average_real_division_by_K!average1,average2


          logical function is_equal_to_K(this,value)


                  implicit none


                  type(average(08)),intent(in   )::this
                  real(        08 ),intent(in   )::value


                  if(this%value==value) then

                     is_equal_to_K=.true.

                  else

                     is_equal_to_K=.false.

              end if!this%value==value


        end         function is_equal_to_K!this,value


            logical function real_average_equal_to_K(real1,average2)


                  implicit none


                  real(        08 ),intent(in   )::   real1
                  type(average(08)),intent(in   )::average2


                  real_average_equal_to_K=average2%is_equal_to(real1)


        end         function real_average_equal_to_K!real1,average2


            logical function average_real_equal_to_K(average1,real2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2


                  average_real_equal_to_K=average1%is_equal_to(real2)


        end         function average_real_equal_to_K!average1,real2


            logical function average_average_equal_to_K(average1,average2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2


                  if(average1%value==average2%value) then

                     average_average_equal_to_K=.true.

                  else

                     average_average_equal_to_K=.false.

              end if!average1%value==average2%value


        end         function average_average_equal_to_K!average1,average2


            logical function is_less_than_or_equal_to_K(this,value)


                  implicit none


                  type(average(08)),intent(in   )::this
                  real(        08 ),intent(in   )::value


                  if(this%value<=value) then

                     is_less_than_or_equal_to_K=.true.

                  else

                     is_less_than_or_equal_to_K=.false.

              end if!this%value<=value


        end         function is_less_than_or_equal_to_K!this,value


            logical function real_average_less_than_or_equal_to_K(real1,average2)


                  implicit none


                  real(        08 ),intent(in   )::   real1
                  type(average(08)),intent(in   )::average2


                  real_average_less_than_or_equal_to_K=average2%is_less_than_or_equal_to(real1)


        end         function real_average_less_than_or_equal_to_K!real1,average2


            logical function average_real_less_than_or_equal_to_K(average1,real2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2


                  average_real_less_than_or_equal_to_K=average1%is_less_than_or_equal_to(real2)


        end         function average_real_less_than_or_equal_to_K!average1,real2


            logical function average_average_less_than_or_equal_to_K(average1,average2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2


                  if(average1%value<=average2%value) then

                     average_average_less_than_or_equal_to_K=.true.

                  else

                     average_average_less_than_or_equal_to_K=.false.

              end if!average1%value<=average2%value


        end         function average_average_less_than_or_equal_to_K!average1,average2


            logical function is_less_than_K(this,value)


                  implicit none


                  type(average(08)),intent(in   )::this
                  real(        08 ),intent(in   )::value


                  if(this%value< value) then

                     is_less_than_K=.true.

                  else

                     is_less_than_K=.false.

              end if!this%value< value


        end         function is_less_than_K!this,value


            logical function real_average_less_than_K(real1,average2)


                  implicit none


                  real(        08 ),intent(in   )::   real1
                  type(average(08)),intent(in   )::average2


                  real_average_less_than_K=average2%is_less_than(real1)


        end         function real_average_less_than_K!real1,average2


            logical function average_real_less_than_K(average1,real2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2


                  average_real_less_than_K=average1%is_less_than(real2)


        end         function average_real_less_than_K!average1,real2


            logical function average_average_less_than_K(average1,average2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2


                  if(average1%value< average2%value) then

                     average_average_less_than_K=.true.

                  else

                     average_average_less_than_K=.false.

              end if!average1%value< average2%value


        end         function average_average_less_than_K!average1,average2


            logical function is_not_equal_to_K(this,value)


                  implicit none


                  type(average(08)),intent(in   )::this
                  real(        08 ),intent(in   )::value


                  if(this%value/=value) then

                     is_not_equal_to_K=.true.

                  else

                     is_not_equal_to_K=.false.

              end if!this%value/=value


        end         function is_not_equal_to_K!this,value


            logical function real_average_not_equal_to_K(real1,average2)


                  implicit none


                  real(        08 ),intent(in   )::   real1
                  type(average(08)),intent(in   )::average2


                  real_average_not_equal_to_K=average2%is_not_equal_to(real1)


        end         function real_average_not_equal_to_K!real1,average2


            logical function average_real_not_equal_to_K(average1,real2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2


                  average_real_not_equal_to_K=average1%is_not_equal_to(real2)


        end         function average_real_not_equal_to_K!average1,real2


            logical function average_average_not_equal_to_K(average1,average2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2


                  if(average1%value/=average2%value) then

                     average_average_not_equal_to_K=.true.

                  else

                     average_average_not_equal_to_K=.false.

              end if!average1%value/=average2%value


        end         function average_average_not_equal_to_K!average1,average2


            logical function is_greater_than_or_equal_to_K(this,value)


                  implicit none


                  type(average(08)),intent(in   )::this
                  real(        08 ),intent(in   )::value


                  if(this%value>=value) then

                     is_greater_than_or_equal_to_K=.true.

                  else

                     is_greater_than_or_equal_to_K=.false.

              end if!this%value>=value


        end         function is_greater_than_or_equal_to_K!this,value


            logical function real_average_greater_than_or_equal_to_K(real1,average2)


                  implicit none


                  real(        08 ),intent(in   )::   real1
                  type(average(08)),intent(in   )::average2


                  real_average_greater_than_or_equal_to_K=average2%is_greater_than_or_equal_to(real1)


        end         function real_average_greater_than_or_equal_to_K!real1,average2


            logical function average_real_greater_than_or_equal_to_K(average1,real2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2


                  average_real_greater_than_or_equal_to_K=average1%is_greater_than_or_equal_to(real2)


        end         function average_real_greater_than_or_equal_to_K!average1,real2


            logical function average_average_greater_than_or_equal_to_K(average1,average2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2


                  if(average1%value>=average2%value) then

                     average_average_greater_than_or_equal_to_K=.true.

                  else

                     average_average_greater_than_or_equal_to_K=.false.

              end if!average1%value>=average2%value


        end         function average_average_greater_than_or_equal_to_K!average1,average2


            logical function is_greater_than_K(this,value)


                  implicit none


                  type(average(08)),intent(in   )::this
                  real(        08 ),intent(in   )::value


                  if(this%value> value) then

                     is_greater_than_K=.true.

                  else

                     is_greater_than_K=.false.

              end if!this%value> value


        end         function is_greater_than_K!this,value


            logical function real_average_greater_than_K(real1,average2)


                  implicit none


                  real(        08 ),intent(in   )::   real1
                  type(average(08)),intent(in   )::average2


                  real_average_greater_than_K=average2%is_greater_than(real1)


        end         function real_average_greater_than_K!real1,average2


            logical function average_real_greater_than_K(average1,real2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  real(        08 ),intent(in   )::   real2


                  average_real_greater_than_K=average1%is_greater_than(real2)


        end         function average_real_greater_than_K!average1,real2


            logical function average_average_greater_than_K(average1,average2)


                  implicit none


                  type(average(08)),intent(in   )::average1
                  type(average(08)),intent(in   )::average2


                  if(average1%value> average2%value) then

                     average_average_greater_than_K=.true.

                  else

                     average_average_greater_than_K=.false.

              end if!average1%value> average2%value


        end         function average_average_greater_than_K!average1,average2


            subroutine  read_average_K(unit,this)


                  implicit none


                  integer          ,intent(in   )::unit
                  type(average(08)),intent(inout)::this


                   read(unit,format_average_K,advance="no") this%weight
                   read(unit,format_average_K,advance="no") this%value


        end subroutine  read_average_K!unit,this


            subroutine write_average_K(unit,this)


                  implicit none


                  integer          ,intent(in   )::unit
                  type(average(08)),intent(in   )::this


                  write(unit,format_average_K,advance="no") this%weight
                  write(unit,format_average_K,advance="no") this%value


        end subroutine write_average_K!unit,this


  end module average_type



# 9 "main/../monte_carlo/monte_carlo.F90" 2 
# 1 "main/../monte_carlo/../monte_carlo/time.F90" 1 



# 1 "main/../monte_carlo/../system/precision.F90" 1 
# 72

# 5 "main/../monte_carlo/../monte_carlo/time.F90" 2 

# 1 "main/../monte_carlo/../monte_carlo/../monte_carlo/average.F90" 1 
# 1014

# 7 "main/../monte_carlo/../monte_carlo/time.F90" 2 


      module time_type


            use::average_type


            implicit none


            character(*),private,parameter::           format_time_K="(sp,  x,e24.17e3 )"
            character(*),private,parameter::text_field_format_time_K="(     x,a24      )"

            real(08),parameter::time_tolerance_K=.00000000001

            character(:),allocatable,public::time_file_name
            logical                 ,public::timestep_is_variable=.false.

            type::time(precision)

               integer,kind::precision

               real(precision),private::current_time=+.00000e+0_08
               real(precision),private::time_setting
               real(precision),private::closing_time
               real(precision),private::average_step
               real(precision),private::current_step

               type(average(precision)),private::log_average_control
               type(average(precision)),private::log_current_control

            contains

               procedure,public::make_time_K;generic::make_time=>make_time_K
               procedure,public::load_time_K;generic::load_time=>load_time_K
               procedure,public::save_time_K;generic::save_time=>save_time_K
               procedure,public::zero_time_K;generic::zero_time=>zero_time_K
               procedure,public::time_left_K;generic::time_left=>time_left_K
               procedure,public::time_step_K;generic::time_step=>time_step_K

               procedure,public::        push_time_K;generic::push_time=>        push_time_K
               procedure,public::dynamic_push_time_K;generic::push_time=>dynamic_push_time_K

        end type  time


            interface  read

               module procedure  read_time_K

        end interface  read

            interface write

               module procedure write_time_K

        end interface write


      contains


            subroutine make_time_K(this,time_setting,average_step)


                  implicit none


                  type(time(08)),intent(inout)::this

                  real(08),intent(in   )::time_setting
                  real(08),intent(in   )::average_step


                  this%current_time=+.00000e+0_08

                  this%time_setting&
                      =time_setting
                  this%closing_time&
                 =this%current_time&
                 +this%time_setting

                  this%average_step&
                      =average_step
                  this%current_step&
                 =this%average_step

                  this%log_average_control=average(this%current_time)
                  this%log_current_control=average(this%current_step)


        end subroutine make_time_K!this,closing_time,average_step


            subroutine load_time_K(this,time_setting,average_step)


                  implicit none


                  type(time(08)),intent(inout)::this

                  real(08),intent(in   )::time_setting
                  real(08),intent(in   )::average_step

                  integer::unit


                  call this%make_time(time_setting,average_step)

                        open(newunit=unit,file=time_file_name)
                  call  read(        unit,               this)
                        read(        unit,                  *)
                       close(        unit                    )



        end subroutine load_time_K!this,time_setting,average_step


            subroutine save_time_K(this)


                  implicit none


                  type(time(08)),intent(inout)::this

                  integer::unit


                        open(newunit=unit,file=time_file_name)
                  call write(        unit,               this)
                       write(        unit,                  *)
                       close(        unit                    )


        end subroutine save_time_K!this


            subroutine zero_time_K(this)


                  implicit none


                  type(time(08)),intent(inout)::this


                  this%closing_time&
                 =this%closing_time&
                 +this%time_setting


        end subroutine zero_time_K!this


            logical function time_left_K(this)


                  implicit none


                  type(time(08)),intent(inout)::this


                  if(this%current_time&
                    <this%closing_time) then

                     if(this%current_time&
                       <this%closing_time-time_tolerance_K) then

                        time_left_K=.true.

                     else

                        time_left_K=.false.

              end    if!this%current_time&
!                      <this%closing_time-time_tolerance_K

                  else

                     time_left_K=.false.

              end if!this%current_time&
!                   <this%closing_time

                  if(.not.time_left_K) then

                     call this%zero_time()

              end if!.not.time_left_K


        end         function time_left_K!this


            function time_step_K(this) result(this_current_step)


                  implicit none


                  type(time(08)),intent(in   )::this

                  real(08)::this_current_step


                  this_current_step&
                 =this%current_step


        end function time_step_K!this


            subroutine push_time_K(this)


                  implicit none


                  type(time(08)),intent(inout)::this


!                 this%current_time&
!                =this%current_time+this%time_step()

                  this%log_average_control&
                 =this%log_average_control+average(this%time_step())

                  this%current_time=this%log_average_control%weight


        end subroutine push_time_K!this,current_control


            subroutine dynamic_push_time_K(this,current_control)


                  implicit none


                  type(time(08)),intent(inout)::this

                  real(08),intent(in   )::current_control


                  this%current_step&
                 =this%average_step*(exp(this%log_average_control%value)&
                                   /              current_control      )

                  this%log_current_control=average(this%current_step,&
                       log(current_control))

                  this%log_average_control&
                 =this%log_average_control&
                 +this%log_current_control

                  this%current_time=this%log_average_control%weight


        end subroutine dynamic_push_time_K!this,current_control


            subroutine  read_time_K(unit,this)


                  implicit none


                  integer               ,intent(in   )::unit
                  type(time(08)),intent(inout)::this


                  call  read(unit,                            this%log_average_control)
                        read(unit,format_time_K,advance="no") this%current_step


        end subroutine  read_time_K!unit,this


            subroutine write_time_K(unit,this)


                  implicit none


                  integer               ,intent(in   )::unit
                  type(time(08)),intent(in   )::this


                  call write(unit,                            this%log_average_control)
                       write(unit,format_time_K,advance="no") this%current_step


        end subroutine write_time_K!unit,this


  end module time_type



# 10 "main/../monte_carlo/monte_carlo.F90" 2 


      module monte_carlo


            use::text_formatting

            use::random_number_generator
            use::average_type
            use::time_type


            implicit none


            logical::measure_time_skipped=.false.

            real(08),public::time_setting=+.00000e+0_08
            real(08),public::measure_skip=+.00000e+0_08
            real(08),public::average_step=+.00000e+0_08

            type(time(08)),public::s
            type(time(08)),public::t

            private::read_time_parameters

            public::make_monte_carlo_K
            public::load_monte_carlo_K
            public::save_monte_carlo_K


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

                  if(measure_time_skipped) then

                     call s%make_time(time_setting,measure_skip)
                     call t%make_time(measure_skip,average_step)

                  else

                     call t%make_time(time_setting,average_step)

              end if!measure_time_skipped


        end subroutine make_monte_carlo_K!


            subroutine load_monte_carlo_K()


                  implicit none


                  call read_time_parameters()

                  call load_seed()

                  if(measure_time_skipped) then

                     call s%load_time(time_setting,measure_skip)
                     call t%load_time(measure_skip,average_step)

                  else

                     call t%load_time(time_setting,average_step)

              end if!measure_time_skipped


        end subroutine load_monte_carlo_K!


            subroutine save_monte_carlo_K()


                  implicit none


                  call save_seed()

                  if(measure_time_skipped) then

                     call s%save_time()

              end if!measure_time_skipped

                  call t%save_time()


        end subroutine save_monte_carlo_K!


            subroutine read_time_parameters()


                  implicit none


                  write(*,"(2a)",advance="no") "time_setting: ",t_yellow
                   read(*,   *               )  time_setting
                  write(*,"(2a)",advance="no")                  t_normal

                  if(measure_time_skipped) then

                     write(*,"(2a)",advance="no") "measure_skip: ",t_yellow
                      read(*,   *               )  measure_skip
                     write(*,"(2a)",advance="no")                  t_normal

              end if!measure_time_skipped

                  write(*,"(2a)",advance="no") "average_step: ",t_yellow
                   read(*,   *               )  average_step
                  write(*,"(2a)",advance="no")                  t_normal

                  write(*,   *               )


        end subroutine read_time_parameters!


  end module monte_carlo



# 27 "main/main.F90" 2 

!     include "../tools/brent_minimization.F90"
!     include "../tools/conjugate_gradient.F90"
!     include "../tools/insert_sort.F90"

!     include "../ikkt/constants.F90"
!     include "../ikkt/fields.F90"
!     include "../ikkt/optimal_toolset.F90"
!     include "../ikkt/conjugate_gradient.F90"
!     include "../ikkt/gauge_cooling.F90"
# 1 "main/../ikkt/complex_langevin.F90" 1 



# 1 "main/../system/precision.F90" 1 
# 72

# 5 "main/../ikkt/complex_langevin.F90" 2 

# 1 "main/../ikkt/../tensor/tensor.F90" 1 



# 1 "main/../ikkt/../system/precision.F90" 1 
# 72

# 5 "main/../ikkt/../tensor/tensor.F90" 2 

# 1 "main/../ikkt/../tensor/../tools/insert_sort.F90" 1 



# 1 "main/../ikkt/../tensor/../system/precision.F90" 1 
# 72

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
                                        0:                  ),intent(in   )::this
                  complex(08),dimension(0:size(this,dim=1)-1)              ::that

                  integer::index


                  do index=0,size(that,dim=1)-1,+1

                     that(index)=this(index,index)

              end do!index=0,size(that,dim=1)-1,+1


        end function vector_matrix_constructor_K!this


            function matrix_factor_constructor_K(size,factor) result(that)


                  implicit none


                  integer    ,                    intent(in   )::size
                  complex(08),                    intent(in   )::factor
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


                  complex(08),dimension(0:                  ),intent(in   )::this
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


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  complex(08),dimension(0:size(vector0,dim=1)-1),intent(in   )::vector1
                  complex(08)                                                 ::scalar_




                  scalar_=sum(vector0*vector1)

# 278



        end function vector_vector_scalar_contraction_real_K!vector0,vector1


            function vector_matrix_vector_contraction_real_K(vector0,matrix1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(08),dimension(0:size(matrix1,dim=2)-1)              ::vector_




                  vector_=matmul(      vector0 ,matrix1)

# 304



        end function vector_matrix_vector_contraction_real_K!vector0,matrix1


            function matrix_vector_vector_contraction_real_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1),intent(in   )::vector1
                  complex(08),dimension(0:size(matrix0,dim=1)-1)              ::vector_




                  vector_=matmul(matrix0 ,vector1)

# 330



        end function matrix_vector_vector_contraction_real_K!matrix0,vector1


            function matrix_matrix_matrix_contraction_real_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_




                  matrix_=matmul(matrix0,matrix1)

# 358



        end function matrix_matrix_matrix_contraction_real_K!matrix0,matrix1


            function vector_vector_vector_ext_product_real_K(vector0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  complex(08),dimension(0:                     ),intent(in   )::vector1
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
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
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


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  complex(08),dimension(0:size(vector0,dim=1)-1),intent(in   )::vector1
                  complex(08)                                                 ::scalar_




                  scalar_=dot_product(vector0,vector1)

# 456



        end function vector_vector_scalar_contraction_complex_K!vector0,vector1


            function vector_matrix_vector_contraction_complex_K(vector0,matrix1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(08),dimension(0:size(matrix1,dim=2)-1)              ::vector_




                  vector_=matmul(conjg(vector0),matrix1)

# 482



        end function vector_matrix_vector_contraction_complex_K!vector0,matrix1


            function matrix_vector_vector_contraction_complex_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1),intent(in   )::vector1
                  complex(08),dimension(0:size(matrix0,dim=1)-1)              ::vector_




                  vector_=matmul(conjg(matrix0),vector1)

# 508



        end function matrix_vector_vector_contraction_complex_K!matrix0,vector1


            function matrix_matrix_matrix_contraction_complex_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_




                  matrix_=matmul(conjg(matrix0),matrix1)

# 536



        end function matrix_matrix_matrix_contraction_complex_K!matrix0,matrix1


            function vector_vector_vector_ext_product_complex_K(vector0,vector1) result(vector_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  complex(08),dimension(0:                     ),intent(in   )::vector1
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
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
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
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1,&
                                        0:size(matrix0,dim=1)-1),intent(in   )::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


                  matrix_=(matrix0.o.matrix1)&
                         -(matrix1.o.matrix0)


        end function matrix_matrix_matrix_commutation_K!matrix0,matrix1


            function matrix_matrix_matrix_anticommutation_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(08),dimension(0:size(matrix0,dim=2)-1,&
                                        0:size(matrix0,dim=1)-1),intent(in   )::matrix1
                  complex(08),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


                  matrix_=(matrix0.o.matrix1)&
                         +(matrix1.o.matrix0)


        end function matrix_matrix_matrix_anticommutation_K!matrix0,matrix1


            function matrix_trace_K(matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
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
                                        0:                     ),intent(in   )::matrix0
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
                                        0:                     ),intent(in   )::matrix0
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
                                              0:                     ),intent(in   )::matrix0
                        complex(08),dimension(0:size(matrix0,dim=1)-2,&
                                              0:size(matrix0,dim=2)-2)              ::matrix_

                        integer,intent(in   )::i0
                        integer,intent(in   )::j0
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


                        integer,intent(in   )::exponent
                        integer              ::sign


                        sign=(-1)**exponent


              end function sign!exponent


        end           function matrix_determinant_K!matrix0


            function matrix_conjugate_K(matrix0) result(matrix_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
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
                                        0:                     ),intent(in   )::matrix0
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
                                        0:                     ),intent(in   )::matrix0
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


                  complex(08)                                   ,intent(in   )::scalar0
                  real(   08)                                                 ::scalar_


                  scalar_=           conjg(scalar0)* scalar0


        end function scalar_norm_squared_K!scalar0


            function vector_norm_squared_K(vector0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  real(   08)                                                 ::scalar_


                  scalar_=real(  sum(conjg(vector0)* vector0))
!                 scalar_=real(            vector0.c.vector0)


        end function vector_norm_squared_K!vector0


            function matrix_norm_squared_K(matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  real(   08)                                                 ::scalar_


                  scalar_=real(  sum(conjg(matrix0)* matrix0))
!                 scalar_=real(trace(      matrix0.c.matrix0))

        end function matrix_norm_squared_K!matrix0


            function vector_matrix_norm_squared_K(vector0,matrix0) result(scalar_)


                  implicit none


                  complex(08),dimension(0:                     ),intent(in   )::vector0
                  complex(08),dimension(0:size(vector0,dim=1)-1,&
                                        0:size(vector0,dim=1)-1),intent(in   )::matrix0
                  real(   08)                                                 ::scalar_


                  scalar_=real(vector0.c.(matrix0.o.vector0))


        end function vector_matrix_norm_squared_K!vector0,matrix0


            subroutine make_matrix_eigenmatrix_K(matrix0,eigenvalue)


                  implicit none


                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(08)                                   ,intent(in   )::eigenvalue

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
                  real(   08),                                   intent(in   ),optional::factor

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
                  real(   08),                                   intent(in   ),optional::factor


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


                  integer    ,                                   intent(in   )::unit
                  complex(08),dimension(0:                     ),intent(inout)::vector0

                  integer::i


                  do i=0,size(vector0,dim=1)-1,+1

                      read(unit,format_tensor_K) vector0(i)

              end do!i=0,size(vector0,dim=1)-1,+1

                   read(unit,*)


        end subroutine  read_vector_K!unit,vector0


            subroutine  read_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,                                   intent(in   )::unit
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


                  integer    ,                                   intent(in   )::unit
                  complex(08),dimension(0:                     ),intent(in   )::vector0

                  integer::i


                  do i=0,size(vector0,dim=1)-1,+1

                     write(unit,format_tensor_K) vector0(i)

              end do!i=0,size(vector0,dim=1)-1,+1

                  write(unit,*)


        end subroutine write_vector_K!unit,vector0


            subroutine write_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,                                   intent(in   )::unit
                  complex(08),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0

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
# 164

# 9 "main/../ikkt/complex_langevin.F90" 2 

# 1 "main/../ikkt/../ikkt/constants.F90" 1 



# 1 "main/../ikkt/../system/precision.F90" 1 
# 72

# 5 "main/../ikkt/../ikkt/constants.F90" 2 
# 1 "main/../ikkt/../ikkt/../main/mathematical_constants.F90" 1 



# 1 "main/../ikkt/../ikkt/../system/precision.F90" 1 
# 72

# 5 "main/../ikkt/../ikkt/../main/mathematical_constants.F90" 2 


      module mathematical_constants


            implicit none


            complex(08),parameter,public::   zero=(+.00000e+0_08,&
                                                   +.00000e+0_08)
            complex(08),parameter,public::re_unit=(+.10000e+1_08,&
                                                   +.00000e+0_08)
            complex(08),parameter,public::im_unit=(+.00000e+0_08,&
                                                   +.10000e+1_08)

            real(08),parameter,public::pi    =acos(-.10000e+1_08)


  end module mathematical_constants


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

            public::make_constants,eject_constants


            contains


            function gamma_size(size)


                  implicit none


                  integer,intent(in   )::size

                  integer::gamma_size


                  gamma_size=2**(size/2-1)


        end function gamma_size!size


            subroutine make_delta(size)


                  implicit none


                  integer,intent(in   )::size


                  if(allocated(delta)) return

                     allocate (delta(0:size-1,&
                                     0:size-1))

                  delta=matrix(size,+re_unit)


        end subroutine make_delta!size


            subroutine make_gamma(size)


                  implicit none


                  integer,intent(in   )::size


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


                  integer,intent(in   )::size

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


                  integer,intent(in   )::size

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


                  integer,intent(in   )::inner_size
                  integer,intent(in   )::boson_size


                  call make_delta     (inner_size)
                  call make_gamma     (boson_size)
                  call make_gamma_core(boson_size)

                  call print_gamma()


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
# 72

# 5 "main/../ikkt/../ikkt/fields.F90" 2 
# 1 "main/../ikkt/../ikkt/../system/text_format.F90" 1 
# 104

# 6 "main/../ikkt/../ikkt/fields.F90" 2 
# 1 "main/../ikkt/../ikkt/../main/mathematical_constants.F90" 1 
# 25

# 7 "main/../ikkt/../ikkt/fields.F90" 2 

# 1 "main/../ikkt/../ikkt/../tensor/tensor.F90" 1 
# 1274

# 9 "main/../ikkt/../ikkt/fields.F90" 2 

# 1 "main/../ikkt/../ikkt/../ikkt/constants.F90" 1 
# 292

# 11 "main/../ikkt/../ikkt/fields.F90" 2 


      module fields


            use::lapack95,only:geev,&
                               ggev

            use::text_formatting
            use::mathematical_constants

            use::tensor_type

            use::constants


            implicit none


            character(*),private,parameter::           format_stat_K="(sp,  x,i20.19   )",&
                                            text_field_format_stat_K="(     x,a20      )"

            character(*),private,parameter::           format_mass_K="(sp,  x,e24.17e3 )",&
                                            text_field_format_mass_K="(     x,a24      )"

            logical,public::configuration_loaded=.false.,&
                            start_field_is_noisy=.false.,&
                            fermions_included_in=.false.,&
                            massive_deformations=.false.

            character(:),allocatable,public::stat_file_name,&
                                             conf_file_name

            integer,public::inner_degrees_of_freedom,n,n_size,&
                            boson_degrees_of_freedom,d,a_size,&
                            fermi_degrees_of_freedom,p,f_size

            real(08),                                                    public::boson_epsilon,&
                                                                                 fermi_mass
            real(08),dimension( :                          ),allocatable,public::boson_mass

            complex(08),dimension( :                          ,&
                                   :                          ,&
                                   :                          ),allocatable,public::a

# 62


            complex(08),dimension( :        ),allocatable,public::f,m_eigenvalues_
            complex(08),dimension( :        ,&
                                   :        ),allocatable,public::m,cm,cmm
            complex(08),dimension( :        ,&
                                   :        , :                          ,&
                                              :                          ,&
                                              :                          ),allocatable,public::ma

            private::make_m            ,&
                     make_cm           ,&
                     make_cmm          ,&
                     make_ma           ,&
                     make_m_eigenvalues

            public::update_fermion_matrix



            public::boson_noise,&
                    fermi_noise

            public::make_fields,&
                    load_fields,&
                    save_fields


      contains


            subroutine  read_field_parameters(unit)


                  implicit none


                  integer,intent(in   )::unit

                  integer::mu


                  write(   *,"(2a)",advance="no") "inner_degrees_of_freedom: ",t_yellow
                   read(unit,   *               )  inner_degrees_of_freedom
                  write(   *,"( a)",advance="no")                              t_normal
                  write(   *,"(2a)",advance="no") "boson_degrees_of_freedom: ",t_yellow
                   read(unit,   *               )  boson_degrees_of_freedom
                  write(   *,"( a)",advance="no")                              t_normal

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

                  write(   *,   *               )

                  if(massive_deformations) then

                     write(   *,"(2a)",advance="no") "boson_epsilon: ",t_yellow
                      read(unit,   *               )  boson_epsilon
                     write(   *,"( a)",advance="no")                   t_normal

                     write(   *,   *               )

                     if(allocated(boson_mass)) deallocate(boson_mass)
                                                 allocate(boson_mass(0:boson_degrees_of_freedom-1))

                     do mu=0,boson_degrees_of_freedom-1,+1

                        write(   *,"(a,i2,2a)",advance="no") "boson_mass(",mu+1,"): ",t_yellow
                         read(unit,        *               )  boson_mass(  mu    )
                        write(   *,"(      a)",advance="no")                          t_normal

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     write(   *,        *               )

                     if(fermions_included_in) then

                        write(   *,"(2a)",advance="no") "fermi_mass: ",t_yellow
                         read(unit,   *               )  fermi_mass
                        write(   *,"( a)",advance="no")                t_normal

                        write(   *,   *               )

              end    if!fermions_included_in

              end if!massive_deformations

                  if(allocated(a)) deallocate(a)
                                     allocate(a(0:inner_degrees_of_freedom-1,&
                                                0:inner_degrees_of_freedom-1,&
                                                0:boson_degrees_of_freedom-1))

                  if(fermions_included_in) then

# 171


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


        end subroutine  read_field_parameters!unit


            subroutine write_field_parameters(unit)


                  implicit none


                  integer,intent(in   )::unit

                  integer::mu


                  if(allocated(a)) deallocate(a)

                  if(fermions_included_in) then

                     if(allocated(f)) deallocate(f)



                     if(allocated(m_eigenvalues_)) deallocate(m_eigenvalues_)

                     if(allocated( m )) deallocate( m )
                     if(allocated(cm )) deallocate(cm )
                     if(allocated(cmm)) deallocate(cmm)

                     if(allocated(ma)) deallocate(ma)



              end if!fermions_included_in

                  write(unit,format_stat_K)  inner_degrees_of_freedom
                  write(unit,format_stat_K)  boson_degrees_of_freedom

                  if(massive_deformations) then

                     write(unit,format_mass_K)  boson_epsilon

                     do mu=0,boson_degrees_of_freedom-1,+1

                        write(unit,format_mass_K)  boson_mass(  mu    )

              end    do!mu=0,boson_degrees_of_freedom-1,+1

                     if(allocated(boson_mass)) deallocate(boson_mass)

                     if(fermions_included_in) write(unit,format_mass_K)  fermi_mass

              end if!massive_deformations

                  write(unit,*)


        end subroutine write_field_parameters!unit


            function boson_noise(standard_deviation)


                  implicit none


                  real(08),intent(in   )::standard_deviation

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


                  real(08),intent(in   )::standard_deviation



                  complex(08),dimension(0:f_size-1)::fermi_noise


                  call random_number(fermi_noise)

                                                                        fermi_noise   &
                 =standard_deviation*sqrt(                    -log(real(fermi_noise)))&
                                    * exp(.20000e+1_08*pi*im_unit*aimag(fermi_noise))

# 331



        end function fermi_noise!standard_deviation


            subroutine make_fields()


                  implicit none


                  integer::mu


                  call  read_field_parameters(5)

                     a=boson_noise(.20000e+1_08)


        end subroutine make_fields!


            subroutine load_fields()


                  implicit none


                  integer::unit
                  integer::mu


                   open(newunit=unit,file=stat_file_name)

                  call  read_field_parameters(unit)

                  close(unit)

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

                   open(newunit=unit,file=stat_file_name)

                  call write_field_parameters(unit)

                  close(unit)


        end subroutine save_fields!





            subroutine make_m()


                  implicit none


                  integer::mu,j2,i2,j1,i1

                  complex(08),dimension(0:n_size-2,&
                                        0:n_size-2)::a_delta

                  m=zero

                   open(unit=11,file="a.delta")

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

                  call print_fermion_matrix()


        end subroutine update_fermion_matrix!


            subroutine print_fermion_matrix()


                  implicit none


                  integer::unit


                   open(newunit=unit,file="fermion.matrix")

!                 call write(   unit,m)
!                 call write(   unit,cm)
!                 call write(   unit,cmm)

                  call write(unit,m_eigenvalues_)

                  close(        unit                      )


        end subroutine print_fermion_matrix!





  end module fields



# 12 "main/../ikkt/complex_langevin.F90" 2 



# 1 "main/../ikkt/../tools/conjugate_gradient.F90" 1 



# 1 "main/../ikkt/../system/precision.F90" 1 
# 72

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


                  complex(08),dimension( :               ),intent(in   )::b
                  complex(08),dimension(0:size(b,dim=1)-1,&
                                        0:size(b,dim=1)-1),intent(in   )::a
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
# 72

# 5 "main/../ikkt/../ikkt/gauge_cooling.F90" 2 

# 1 "main/../ikkt/../ikkt/../tensor/tensor.F90" 1 
# 1274

# 7 "main/../ikkt/../ikkt/gauge_cooling.F90" 2 

# 1 "main/../ikkt/../ikkt/../ikkt/fields.F90" 1 
# 656

# 9 "main/../ikkt/../ikkt/gauge_cooling.F90" 2 

# 1 "main/../ikkt/../ikkt/../tools/brent_minimization.F90" 1 



# 1 "main/../ikkt/../ikkt/../system/precision.F90" 1 
# 72

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


            character(*),parameter,private::           format_cooling="(sp,2(x,e24.17e3))",&
                                            text_field_format_cooling="(   2(x,a24     ))"

            real(08),parameter,private::cooling_tolerance=.00000000001

            real(08),private::min_alpha=.00000000001*1  ,&
                              mid_alpha=.00000000001*10 ,&
                              max_alpha=.00000000001*100

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


                  real(08),intent(in   )::alpha

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


                  real(08),intent(in   )::alpha

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


                  integer,intent(in   )::step_100

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
                                   :                          ),allocatable,public::drift,&
                                                                                    noise

            real(08),public::drift_norm

            public::langevin_step

            public::make_drift     ,&
                    make_noise     ,&
                    make_drift_norm


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
                 =a+drift*     t%time_step()&
                   +noise*sqrt(t%time_step())

                  if(gauge_cooling_active) then

                     call apply_cooling()

              end if!gauge_cooling_active

                  if(timestep_is_variable) then

                     call make_drift_norm()
                     call t%push_time(drift_norm)

                  else

                     call t%push_time()

              end if!timestep_is_variable


        end subroutine langevin_step!


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
                             +determinant_degree(boson_degrees_of_freedom)*(fermi_noise(.10000e+1_08).c.(ma(:,:,i,j,mu).o.f(:)))

              end          do!i=0,inner_degrees_of_freedom-1,+1

              end       do!j=0,inner_degrees_of_freedom-1,+1

              end    do!mu=0,boson_degrees_of_freedom-1,+1

# 198


              end if!fermions_included_in

                  do mu=0,boson_degrees_of_freedom-1,+1

!                    call make_hermitian(drift(:,:,mu))
                     call make_traceless(drift(:,:,mu))

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end subroutine make_drift!


            subroutine make_drift_norm()


                  implicit none


                  integer::mu


                  drift_norm=+.00000e+0_08

                  do mu=1,boson_degrees_of_freedom-1,+1

                     drift_norm&
                    =drift_norm+norm(drift(:,:,mu))

              end do!mu= ,boson_degrees_of_freedom-1,+1

                  drift_norm&
                 =drift_norm/a_size


        end subroutine make_drift_norm!


            subroutine make_noise()


                  implicit none


                  integer::mu


                  noise=boson_noise(.20000e+1_08)


        end subroutine make_noise!


            subroutine eject_complex_langevin()


                  implicit none


                  call eject_gauge_cooler()
                  call eject_constants()

                  if(allocated(drift)) deallocate(drift)
                  if(allocated(drift)) deallocate(noise)


        end subroutine eject_complex_langevin


  end module complex_langevin



# 38 "main/main.F90" 2 
# 1 "main/../ikkt/observables.F90" 1 



# 1 "main/../system/precision.F90" 1 
# 72

# 5 "main/../ikkt/observables.F90" 2 

# 1 "main/../ikkt/../tensor/tensor.F90" 1 
# 1274

# 7 "main/../ikkt/observables.F90" 2 

# 1 "main/../ikkt/../monte_carlo/time.F90" 1 
# 310

# 9 "main/../ikkt/observables.F90" 2 

# 1 "main/../ikkt/../ikkt/fields.F90" 1 
# 656

# 11 "main/../ikkt/observables.F90" 2 

# 16


# 1 "main/../ikkt/../ikkt/complex_langevin.F90" 1 
# 272

# 19 "main/../ikkt/observables.F90" 2 


      module observables


            use::tensor_type

            use::time_type

            use::fields

# 34


            use::complex_langevin


            implicit none


            character(*),private,parameter::           format_observables_K="(sp,2(x,e24.17e3))",&
                                            text_field_format_observables_K="(   2(x,a24     ))"

            character(:),allocatable,public::meas_file_name

            public::print_observables

            private::faraday_squared
            private::lambda

            public::boson_action
!           public::fermi_action


      contains


            subroutine print_observables(unit)


                  implicit none


                  integer        ,intent(in   )::unit

                  integer::mu


!                 call make_drift_norm()

                  call write(unit,                   t             )
                       write(unit,format_observables_K,advance="no") faraday_squared()

                  if(massive_deformations) then

                       write(unit,format_observables_K,advance="no") boson_epsilon

                     if(fermions_included_in) then

                       write(unit,format_observables_K,advance="no") fermi_mass

              end    if!fermions_included_in

                     do mu=0,boson_degrees_of_freedom-1,+1

                       write(unit,format_observables_K,advance="no") lambda(mu)

              end    do!mu=0,boson_degrees_of_freedom-1,+1

              end if!massive_deformations

                  write(unit,*)

                  if(measure_time_skipped) then

                     call s%push_time()

              end if!measure_time_skipped


        end subroutine print_observables!unit,measurement_time


            function faraday_squared()


                  implicit none


                  real(08)::faraday_squared

                  integer::mu,nu


                  faraday_squared=+.00000e+0_08

                  do mu=0,boson_degrees_of_freedom-1,+1

                     do nu=0,boson_degrees_of_freedom-1,+1

!                       faraday_squared&
!                      =faraday_squared+norm(a(:,:,mu).commutation.a(:,:,nu))

                        faraday_squared&
                       =faraday_squared-trace((a(:,:,mu).commutation.a(:,:,nu)) &
                                           .o.(a(:,:,mu).commutation.a(:,:,nu)))/inner_degrees_of_freedom

              end    do!nu=0,boson_degrees_of_freedom-1,+1

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end function faraday_squared


            function lambda(k)


                  implicit none


                  integer,intent(in   )::k

                  complex(08)::lambda


                  lambda=trace(a(:,:,k).o.a(:,:,k))


        end function lambda!k


            function boson_action()


                  implicit none


                  real(08)::boson_action

                  integer::mu


                  boson_action=faraday_squared()*inner_degrees_of_freedom&
                                                *inner_degrees_of_freedom/4

                  if(massive_deformations) then

                     do mu=0,boson_degrees_of_freedom-1,+1

                        boson_action&
                       =boson_action+boson_mass(mu)*lambda(mu)*boson_epsilon*inner_degrees_of_freedom&
                                                                            *inner_degrees_of_freedom/2

              end    do!mu=0,boson_degrees_of_freedom-1,+1

              end if!massive_deformations


        end function boson_action


!           function fermi_action()


!                 implicit none


!                 complex(08)::fermi_action


!                 fermi_action&
!                =fermi_action+determinant_degree(boson_degrees_of_freedom)*sum(log(m_eigenvalues_))


!       end function fermi_action


  end module observables



# 39 "main/main.F90" 2 

!     define BLAS
!     define OPTIMAL


      program main


            use::get_options

!           use::tensor_type

!           use::random_number_generator
!           use::average_type
!           use::time_type
            use::monte_carlo

!           use::brent_minimization
!           use::conjugate_gradient_method

!           use::constants
!           use::fields
!           use::complex_langevin
            use::gauge_cooling
            use::observables


            implicit none


            character(4),parameter::simulation_path="data"

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


                  integer::unit!s_counter,&
!                               t_counter


                   open(newunit=unit,file=meas_file_name)

                  if(measure_time_skipped) then

                     do while(s%time_left())

                        do while(t%time_left())

                           call langevin_step()

              end       do!while(t%time_left())

                        call print_observables(unit)

              end    do!while(s%time_left())

                  else

                     do while(t%time_left())

                        call langevin_step()
                        call print_observables(unit)

              end    do!while(t%time_left())

              end if!measure_time_skipped

                  close(unit)


        end subroutine perform_ikkt_simulation!


            subroutine end_ikkt_simulation


                  implicit none


                  call save_monte_carlo()
                  call save_fields()

                  call print_gamma()



                  call print_fermion_matrix()




        end subroutine end_ikkt_simulation!


            subroutine read_options_and_arguments()


                  use,intrinsic::iso_fortran_env,only:stdin=>input_unit,&
                                                      stdout=>output_unit,&
                                                      stderr=>error_unit


                  implicit none


                  character     ::option_character ! currently parsed option
                  character(500)::option_argument  ! currently parsed argument value if not an option
                  integer       ::argument_length  ! currently parsed argument true length
                  integer       ::status     !       stat of parsing option
                  integer       ::argument_index   ! current argument running index starting where the options end
                  integer       ::argument_number  ! number of remaining indices options aside

                  type(option)::options(0:7) ! list of long options

                  integer::index


                  options(1)=option(    "skip-time"    ,.false.,'s',"Use a fixed time to skip before measuring observables. &
                                                                    &This is supposed tobe at least an order of magnitude &
                                                                    &larger than the average timestep, and usually is &
                                                                    &comparable to the autocorrelation time of the time &
                                                                    &history.","")

                  options(2)=option("variable-timestep",.false.,'t',"Use variable timestep with fixed timestep as average. This &
                                                                    &alelviates drift divergence due to instabilities in either a &
                                                                    &poorly chosen starting configuration or a large timestep.","")

                  options(3)=option("noisy-start-field",.false.,'a',"Start with a (gausssian) noisy (hot) initial configuration &
                                                                    &instead of the default zero (cold) initial configuration. &
                                                                    &Overriden by "//t_bold//"-c"//s_bold//" when loading a &
                                                                    &previously saved configuration.","")

                  options(4)=option(    "load-save"    ,.false.,'c',"Load simulation stats and field configuration from a previous &
                                                                    &simulation. This helps in storing thermalized configurations, &
                                                                    &and reusing instead of repeating thermalizations. Overrides "&
                                                                    &//t_bold//"-a"//s_bold//" when not starting a simulation from &
                                                                    &scratch.","")

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

                     call getopt(options="stacfgmh"       ,&
                                longopts=options         ,&
                                 optchar=option_character,&
                                  optarg=option_argument ,&
                                  arglen=argument_length ,&
                                    stat=status          ,&
                                  offset=argument_index  ,&
                                  remain=argument_number)

                     if(status==1) exit

                     select case(option_character)

                     case('s'); measure_time_skipped=.true.
                     case('t'); timestep_is_variable=.true.
                     case('a'); start_field_is_noisy=.true.
                     case('c'); configuration_loaded=.true.
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

                        stop

              end    select!case(option_character)

              end do

                  call get_command_argument(argument_index+1,option_argument,status)

                  select case(status)

                  case(:-1 )

                     stop "Error: bad argment index"

                  case( +1:)

                     stop "Error: a base file name must be provided to store simulation status"

                  case default

                     continue

              end select!case(status)

                  allocate(character(len(trim(adjustl(option_argument))))::base_file_name)
                                                                           base_file_name=trim(adjustl(option_argument))

                  call execute_command_line("mkdir --parents -- "//'"'//"${PWD#'ifort/bin'}"//simulation_path//'"')

                  allocate(character(len(base_file_name)+5)::seed_file_name)
                                                             seed_file_name=simulation_path//trim(adjustl(base_file_name))//".seed"
                  allocate(character(len(base_file_name)+5)::time_file_name)
                                                             time_file_name=simulation_path//trim(adjustl(base_file_name))//".time"
                  allocate(character(len(base_file_name)+5)::stat_file_name)
                                                             stat_file_name=simulation_path//trim(adjustl(base_file_name))//".stat"
                  allocate(character(len(base_file_name)+5)::conf_file_name)
                                                             conf_file_name=simulation_path//trim(adjustl(base_file_name))//".conf"
                  allocate(character(len(base_file_name)+5)::meas_file_name)
                                                             meas_file_name=simulation_path//trim(adjustl(base_file_name))//".meas"

                  write(*,*)


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
