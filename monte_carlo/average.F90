#     ifndef MONTE_CARLO_AVERAGE_F90
#     define MONTE_CARLO_AVERAGE_F90

#     include "main/precision.F90"


      module average_type


            implicit none


            character(*),private,parameter::           format_average_K=REALGK
            character(*),private,parameter::text_field_format_average_K=REALAK

            type::average(precision)

               integer,kind::precision

               real(precision),private::weight=+.10000e+1_KK
               real(precision),private::value =+.00000e+0_KK

            contains

               procedure,private::initialize_average_K;generic::initialize_average=>initialize_average_K

               procedure,public::weight_K
               procedure,public::value__K

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

            interface weight

               module procedure weight_K

        end interface weight

            interface value

               module procedure value__K

        end interface value

            interface operator(+)

               module procedure         average_average__plus_K
               module procedure average_average_average__plus_K

        end interface operator(+)

            interface operator(-)

               module procedure         average_average_minus_K
               module procedure average_average_average_minus_K

        end interface operator(-)

            interface operator(*)

               module procedure real_average_average_times_value_K
               module procedure average_real_average_times_value_K

               module procedure average_average_real_times_value_K

        end interface operator(*)

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


                  type(average(KK)),intent(inout)         ::this
                  real(        KK ),intent(in   ),optional::weight
                  real(        KK ),intent(in   ),optional::value


                  if(present(weight)) then

                     this%weight=weight

              end if!present(weight)

                  if(present(value)) then

                     this%value=value

              end if!present(value)


        end subroutine initialize_average_K!this,weight,value


            function average_constructor_K(weight,value) result(that)


                  implicit none


                  real(        KK ),intent(in   ),optional::weight
                  real(        KK ),intent(in   ),optional::value
                  type(average(KK))                       ::that


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


            function weight_K(this) result(weight)


                  implicit none


                  type(average(KK)),intent(inout)::this
                  real(        KK )              ::weight


                  weight=this%weight


        end function weight_K!this


            function value__K(this) result(value )


                  implicit none


                  type(average(KK)),intent(inout)::this
                  real(        KK )              ::value


                  value =this%value


        end function value__K!this


            function average_average__plus_K(average0) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average0
                  type(average(KK))              ::average_


                  average_%weight&
                = average0%weight
                  average_%value &
                =+average0%value


        end function average_average__plus_K!average0


            function average_average_average__plus_K(average1,average2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


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


            function average_average_minus_K(average0) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average0
                  type(average(KK))              ::average_


                  average_%weight&
                = average0%weight

                  average_%value &
                =-average0%value


        end function average_average_minus_K!average0


            function average_average_average_minus_K(average1,average2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


                  average_%weight&
                 =average1%weight&
                 +average2%weight

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


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


                  average_%weight&
                 =average2%weight

                  average_%value =real1&
                 *average2%value


        end function real_average_average_times_value_K!real1,average2


            function average_real_average_times_value_K(average1,real2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2
                  type(average(KK))              ::average_


                  average_%weight&
                 =average1%weight

                  average_%value &
                 =average1%value *real2


        end function average_real_average_times_value_K!average1,real2


            function average_average_real_times_value_K(average1,average2) result(real_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  real(        KK )              ::   real_


                  real_=average1%value&
                       *average2%value


        end function average_average_real_times_value_K!average1,average2


            function average_real_average_division_by_K(average1,real2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2
                  type(average(KK))              ::average_


                  average_%weight&
                 =average1%weight

                  average_%value &
                 =average1%value /real2


        end function average_real_average_division_by_K!average1,real2


            function average_average_real_division_by_K(average1,average2) result(real_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  real(        KK )              ::   real_


                 real_=average1%value&
                      /average2%value


        end function average_average_real_division_by_K!average1,average2


          logical function is_equal_to_K(this,value)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::value


                  if(this%value==value) then

                     is_equal_to_K=.true.

                  else

                     is_equal_to_K=.false.

              end if!this%value==value


        end         function is_equal_to_K!this,value


            logical function real_average_equal_to_K(real1,average2)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2


                  real_average_equal_to_K=average2%is_equal_to(real1)


        end         function real_average_equal_to_K!real1,average2


            logical function average_real_equal_to_K(average1,real2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2


                  average_real_equal_to_K=average1%is_equal_to(real2)


        end         function average_real_equal_to_K!average1,real2


            logical function average_average_equal_to_K(average1,average2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2


                  if(average1%value==average2%value) then

                     average_average_equal_to_K=.true.

                  else

                     average_average_equal_to_K=.false.

              end if!average1%value==average2%value


        end         function average_average_equal_to_K!average1,average2


            logical function is_less_than_or_equal_to_K(this,value)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::value


                  if(this%value<=value) then

                     is_less_than_or_equal_to_K=.true.

                  else

                     is_less_than_or_equal_to_K=.false.

              end if!this%value<=value


        end         function is_less_than_or_equal_to_K!this,value


            logical function real_average_less_than_or_equal_to_K(real1,average2)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2


                  real_average_less_than_or_equal_to_K=average2%is_less_than_or_equal_to(real1)


        end         function real_average_less_than_or_equal_to_K!real1,average2


            logical function average_real_less_than_or_equal_to_K(average1,real2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2


                  average_real_less_than_or_equal_to_K=average1%is_less_than_or_equal_to(real2)


        end         function average_real_less_than_or_equal_to_K!average1,real2


            logical function average_average_less_than_or_equal_to_K(average1,average2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2


                  if(average1%value<=average2%value) then

                     average_average_less_than_or_equal_to_K=.true.

                  else

                     average_average_less_than_or_equal_to_K=.false.

              end if!average1%value<=average2%value


        end         function average_average_less_than_or_equal_to_K!average1,average2


            logical function is_less_than_K(this,value)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::value


                  if(this%value< value) then

                     is_less_than_K=.true.

                  else

                     is_less_than_K=.false.

              end if!this%value< value


        end         function is_less_than_K!this,value


            logical function real_average_less_than_K(real1,average2)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2


                  real_average_less_than_K=average2%is_less_than(real1)


        end         function real_average_less_than_K!real1,average2


            logical function average_real_less_than_K(average1,real2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2


                  average_real_less_than_K=average1%is_less_than(real2)


        end         function average_real_less_than_K!average1,real2


            logical function average_average_less_than_K(average1,average2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2


                  if(average1%value< average2%value) then

                     average_average_less_than_K=.true.

                  else

                     average_average_less_than_K=.false.

              end if!average1%value< average2%value


        end         function average_average_less_than_K!average1,average2


            logical function is_not_equal_to_K(this,value)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::value


                  if(this%value/=value) then

                     is_not_equal_to_K=.true.

                  else

                     is_not_equal_to_K=.false.

              end if!this%value/=value


        end         function is_not_equal_to_K!this,value


            logical function real_average_not_equal_to_K(real1,average2)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2


                  real_average_not_equal_to_K=average2%is_not_equal_to(real1)


        end         function real_average_not_equal_to_K!real1,average2


            logical function average_real_not_equal_to_K(average1,real2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2


                  average_real_not_equal_to_K=average1%is_not_equal_to(real2)


        end         function average_real_not_equal_to_K!average1,real2


            logical function average_average_not_equal_to_K(average1,average2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2


                  if(average1%value/=average2%value) then

                     average_average_not_equal_to_K=.true.

                  else

                     average_average_not_equal_to_K=.false.

              end if!average1%value/=average2%value


        end         function average_average_not_equal_to_K!average1,average2


            logical function is_greater_than_or_equal_to_K(this,value)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::value


                  if(this%value>=value) then

                     is_greater_than_or_equal_to_K=.true.

                  else

                     is_greater_than_or_equal_to_K=.false.

              end if!this%value>=value


        end         function is_greater_than_or_equal_to_K!this,value


            logical function real_average_greater_than_or_equal_to_K(real1,average2)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2


                  real_average_greater_than_or_equal_to_K=average2%is_greater_than_or_equal_to(real1)


        end         function real_average_greater_than_or_equal_to_K!real1,average2


            logical function average_real_greater_than_or_equal_to_K(average1,real2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2


                  average_real_greater_than_or_equal_to_K=average1%is_greater_than_or_equal_to(real2)


        end         function average_real_greater_than_or_equal_to_K!average1,real2


            logical function average_average_greater_than_or_equal_to_K(average1,average2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2


                  if(average1%value>=average2%value) then

                     average_average_greater_than_or_equal_to_K=.true.

                  else

                     average_average_greater_than_or_equal_to_K=.false.

              end if!average1%value>=average2%value


        end         function average_average_greater_than_or_equal_to_K!average1,average2


            logical function is_greater_than_K(this,value)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::value


                  if(this%value> value) then

                     is_greater_than_K=.true.

                  else

                     is_greater_than_K=.false.

              end if!this%value> value


        end         function is_greater_than_K!this,value


            logical function real_average_greater_than_K(real1,average2)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2


                  real_average_greater_than_K=average2%is_greater_than(real1)


        end         function real_average_greater_than_K!real1,average2


            logical function average_real_greater_than_K(average1,real2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2


                  average_real_greater_than_K=average1%is_greater_than(real2)


        end         function average_real_greater_than_K!average1,real2


            logical function average_average_greater_than_K(average1,average2)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2


                  if(average1%value> average2%value) then

                     average_average_greater_than_K=.true.

                  else

                     average_average_greater_than_K=.false.

              end if!average1%value> average2%value


        end         function average_average_greater_than_K!average1,average2


            subroutine  read_average_K(unit,this)


                  implicit none


                  integer          ,intent(in   )::unit
                  type(average(KK)),intent(inout)::this


                   read(unit,format_average_K,advance="no") this%weight
                   read(unit,format_average_K,advance="no") this%value


        end subroutine  read_average_K!unit,this


            subroutine write_average_K(unit,this)


                  implicit none


                  integer          ,intent(in   )::unit
                  type(average(KK)),intent(in   )::this


                  write(unit,format_average_K,advance="no") this%weight
                  write(unit,format_average_K,advance="no") this%value


        end subroutine write_average_K!unit,this


  end module average_type


#  endif
