#     ifndef AVERAGE_F90
#     define AVERAGE_F90

#     include "../system/precision.F90"


      module average_type


            implicit none


            character(*),private,parameter::           format_average_K=REALGK
            character(*),private,parameter::text_field_format_average_K=REALAK

            real(KK),private,parameter::average_tolerance=TOLERANCEK

            type::average(precision)

               integer,kind::precision

               real(precision),private::m_weight=+.00000e+0_KK
               real(precision),private::m_value =+.00000e+0_KK

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

               module procedure average_construct_K

               module procedure average_value_K
               module procedure average_value_weight_K

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


                  type(average(KK)),intent(inout)         ::this
                  real(        KK ),intent(in   ),optional::weight
                  real(        KK ),intent(in   ),optional::value


                  if(present(weight)) then

                     this%m_weight=weight

                     if(present(value)) then

                        this%m_value=value

              end    if!present(value)

              end if!present(weight)


        end subroutine average_set_K!this,weight,value


            subroutine weight_set_K(this,weight)


                  implicit none


                  type(average(KK)),intent(inout)::this
                  real(        KK )              ::weight


                  this%m_weight=weight


        end subroutine weight_set_K!this,weight


            subroutine value_set_K(this,value)


                  implicit none


                  type(average(KK)),intent(inout)::this
                  real(        KK )              ::value


                  this%m_value=value


        end subroutine value_set_K!this,value


            function average_construct_K(weight,value) result(that)


                  implicit none


                  real(        KK ),intent(in   ),optional::weight
                  real(        KK ),intent(in   ),optional::value
                  type(average(KK))                       ::that


                  if(present(weight)) then

                     if(present(value)) then

                        call that%average(weight,value)

                     else

                        call that%average(weight)

              end    if!present(value)

                  else

                     call that%average()

              end if!present(weight)


        end function average_construct_K!weight,value


            function average_value_K(values) result(standard)


                  implicit none


                  real(KK),dimension(               :               ),intent(inout)::values

                  real(KK)::standard


                  standard=sum(values)/size(values)


        end function average_value_K!values


            function average_value_weight_K(values,weights) result(weighted)


                  implicit none


                  real(KK),dimension(                     :                     ),intent(inout)::weights
                  real(KK),dimension(lbound(weights,dim=1):ubound(weights,dim=1)),intent(inout)::values

                  type(average(KK))::weighted


                  call weighted%weight(sum(weights       )                 )
                  call weighted%value (sum(weights*values)/weight(weighted))


        end function average_value_weight_K!values,weights


            function weight_get_K(this) result(weight)


                  implicit none


                  type(average(KK)),intent(inout)::this
                  real(        KK )              ::weight


                  weight=this%m_weight


        end function weight_get_K!this


            function value_get_K(this) result(value)


                  implicit none


                  type(average(KK)),intent(inout)::this
                  real(        KK )              ::value


                  value=this%m_value


        end function value_get_K!this


            subroutine average_real_assignment_K(average0,real1)


               implicit none


               type(average(KK)),intent(inout)::average0
               real(        KK ),intent(in   )::   real1


               average0%m_weight=real1


        end subroutine average_real_assignment_K!average0,real1


            function real_average_average_plus_K(real1,average2) result(average_)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


                  average_%m_weight=real1&
                 +average2%m_weight


        end function real_average_average_plus_K!real1,average2


            function average_real_average_plus_K(average1,real2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2
                  type(average(KK))              ::average_


                  average_%m_weight&
                 =average1%m_weight+real2


        end function average_real_average_plus_K!average1,real2


            function average_average_average_plus_K(average1,average2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


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


                  type(average(KK)),intent(in   )::average0
                  type(average(KK))              ::average_


                  average_%m_weight&
                =+average0%m_weight

                  average_%m_value &
                =+average0%m_value


        end function average_average_plus_K!average0


            function real_average_average_minus_K(real1,average2) result(average_)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


                  average_%m_weight=real1&
                 -average2%m_weight


        end function real_average_average_minus_K!real1,average2


            function average_real_average_minus_K(average1,real2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2
                  type(average(KK))              ::average_


                  average_%m_weight&
                 =average1%m_weight-real2


        end function average_real_average_minus_K!average1,real2


            function average_average_average_minus_K(average1,average2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


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


                  type(average(KK)),intent(in   )::average0
                  type(average(KK))              ::average_


                  average_%m_weight&
                =-average0%m_weight

                  average_%m_value &
                =+average0%m_value


        end function average_average_minus_K!average0


            function average_sum_K(average0) result(average_)


                  implicit none


                  type(average(KK)),dimension(:),intent(inout)::average0
                  type(average(KK))                           ::average_

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


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2
                  type(average(KK))              ::average_


                  average_%m_weight=real1&
                 *average2%m_weight

                  average_%m_value &
                 =average2%m_value


        end function real_average_average_times_value_K!real1,average2


            function average_real_average_times_value_K(average1,real2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2
                  type(average(KK))              ::average_


                  average_%m_weight&
                 =average1%m_weight*real2

                  average_%m_value &
                 =average1%m_value


        end function average_real_average_times_value_K!average1,real2


            function average_average_real_times_value_K(average1,average2) result(real_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  real(        KK )              ::   real_


                  real_=average1%m_value&
                       *average2%m_value


        end function average_average_real_times_value_K!average1,average2


            function average_real_average_division_by_K(average1,real2) result(average_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2
                  type(average(KK))              ::average_


                  average_%m_weight&
                 =average1%m_weight/real2

                  average_%m_value &
                 =average1%m_value


        end function average_real_average_division_by_K!average1,real2


            function average_average_real_division_by_K(average1,average2) result(real_)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2
                  real(        KK )              ::   real_


                 real_=average1%m_value&
                      /average2%m_value


        end function average_average_real_division_by_K!average1,average2


            function average_product_K(average0) result(real_)


                  implicit none


                  type(average(KK)),dimension(:),intent(inout)::average0
                  real(        KK )                           ::   real_


                  real_=product(average0%m_value)


        end function average_product_K!average0


          function is_equal_to_K(this,weight) result(is_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::weight

                  logical::is_equal_to


                  if(abs(this%m_weight-weight)<average_tolerance) then

                     is_equal_to=.true.

                  else

                     is_equal_to=.false.

              end if!abs(this%m_weight-weight)<average_tolerance


        end function is_equal_to_K!this,weight


            function real_average_equal_to_K(real1,average2) result(real_average_equal_to)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2

                  logical::real_average_equal_to


                  real_average_equal_to=average2%is_equal_to(real1)


        end function real_average_equal_to_K!real1,average2


            function average_real_equal_to_K(average1,real2) result(average_real_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2

                  logical::average_real_equal_to


                  average_real_equal_to=average1%is_equal_to(real2)


        end function average_real_equal_to_K!average1,real2


            function average_average_equal_to_K(average1,average2) result(average_average_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2

                  logical::average_average_equal_to


                  if(abs(average1%m_weight-average2%m_weight)<average_tolerance) then

                     average_average_equal_to=.true.

                  else

                     average_average_equal_to=.false.

              end if!abs(average1%m_weight-average2%m_weight)<average_tolerance


        end function average_average_equal_to_K!average1,average2


            function is_less_than_or_equal_to_K(this,weight) result(is_less_than_or_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::weight

                  logical::is_less_than_or_equal_to


                  if(this%m_weight<=weight+average_tolerance) then

                     is_less_than_or_equal_to=.true.

                  else

                     is_less_than_or_equal_to=.false.

              end if!this%m_weight<weight+average_tolerance


        end function is_less_than_or_equal_to_K!this,weight


            function real_average_less_than_or_equal_to_K(real1,average2) result(real_average_less_than_or_equal_to)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2

                  logical::real_average_less_than_or_equal_to


                  real_average_less_than_or_equal_to=average2%is_less_than_or_equal_to(real1)


        end function real_average_less_than_or_equal_to_K!real1,average2


            function average_real_less_than_or_equal_to_K(average1,real2) result(average_real_less_than_or_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2

                  logical::average_real_less_than_or_equal_to


                  average_real_less_than_or_equal_to=average1%is_less_than_or_equal_to(real2)


        end function average_real_less_than_or_equal_to_K!average1,real2


            function average_average_less_than_or_equal_to_K(average1,average2) result(average_average_less_than_or_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2

                  logical::average_average_less_than_or_equal_to


                  if(average1%m_weight<=average2%m_weight+average_tolerance) then

                     average_average_less_than_or_equal_to=.true.

                  else

                     average_average_less_than_or_equal_to=.false.

              end if!average1%m_weight<average2%m_weight+average_tolerance


        end function average_average_less_than_or_equal_to_K!average1,average2


            function is_less_than_K(this,weight) result(is_less_than)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::weight

                  logical::is_less_than


                  if(this%m_weight<=weight-average_tolerance) then

                     is_less_than=.true.

                  else

                     is_less_than=.false.

              end if!this%m_weight<=weight-average_tolerance


        end function is_less_than_K!this,weight


            function real_average_less_than_K(real1,average2) result(real_average_less_than)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2

                  logical::real_average_less_than


                  real_average_less_than=average2%is_less_than(real1)


        end function real_average_less_than_K!real1,average2


            function average_real_less_than_K(average1,real2) result(average_real_less_than)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2

                  logical::average_real_less_than


                  average_real_less_than=average1%is_less_than(real2)


        end function average_real_less_than_K!average1,real2


            function average_average_less_than_K(average1,average2) result(average_average_less_than)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2

                  logical::average_average_less_than


                  if(average1%m_weight<=average2%m_weight-average_tolerance) then

                     average_average_less_than=.true.

                  else

                     average_average_less_than=.false.

              end if!average1%m_weight<=average2%m_weight-average_tolerance


        end function average_average_less_than_K!average1,average2


            function is_not_equal_to_K(this,weight) result(is_not_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::weight

                  logical::is_not_equal_to


                  if(abs(this%m_weight-weight)>=average_tolerance) then

                     is_not_equal_to=.true.

                  else

                     is_not_equal_to=.false.

              end if!abs(this%m_weight-weight)>=average_tolerance


        end function is_not_equal_to_K!this,weight


            function real_average_not_equal_to_K(real1,average2) result(real_average_not_equal_to)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2

                  logical::real_average_not_equal_to


                  real_average_not_equal_to=average2%is_not_equal_to(real1)


        end function real_average_not_equal_to_K!real1,average2


            function average_real_not_equal_to_K(average1,real2) result(average_real_not_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2

                  logical::average_real_not_equal_to


                  average_real_not_equal_to=average1%is_not_equal_to(real2)


        end function average_real_not_equal_to_K!average1,real2


            function average_average_not_equal_to_K(average1,average2) result(average_average_not_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2

                  logical::average_average_not_equal_to


                  if(abs(average1%m_weight-average2%m_weight)>=average_tolerance) then

                     average_average_not_equal_to=.true.

                  else

                     average_average_not_equal_to=.false.

              end if!abs(average1%m_weight-average2%m_weight)>=average_tolerance


        end function average_average_not_equal_to_K!average1,average2


            function is_greater_than_or_equal_to_K(this,weight) result(is_greater_than_or_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::weight

                  logical::is_greater_than_or_equal_to


                  if(this%m_weight>=weight-average_tolerance) then

                     is_greater_than_or_equal_to=.true.

                  else

                     is_greater_than_or_equal_to=.false.

              end if!this%m_weight>=weight-average_tolerance


        end function is_greater_than_or_equal_to_K!this,weight


            function real_average_greater_than_or_equal_to_K(real1,average2) result(real_average_greater_than_or_equal_to)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2

                  logical::real_average_greater_than_or_equal_to


                  real_average_greater_than_or_equal_to=average2%is_greater_than_or_equal_to(real1)


        end function real_average_greater_than_or_equal_to_K!real1,average2


            function average_real_greater_than_or_equal_to_K(average1,real2) result(average_real_greater_than_or_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2

                  logical::average_real_greater_than_or_equal_to


                  average_real_greater_than_or_equal_to=average1%is_greater_than_or_equal_to(real2)


        end function average_real_greater_than_or_equal_to_K!average1,real2


            function average_average_greater_than_or_equal_to_K(average1,average2) result(average_average_greater_than_or_equal_to)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2

                  logical::average_average_greater_than_or_equal_to


                  if(average1%m_weight>=average2%m_weight-average_tolerance) then

                     average_average_greater_than_or_equal_to=.true.

                  else

                     average_average_greater_than_or_equal_to=.false.

              end if!average1%m_weight>=average2%m_weight-average_tolerance


        end function average_average_greater_than_or_equal_to_K!average1,average2


            function is_greater_than_K(this,weight) result(is_greater_than)


                  implicit none


                  type(average(KK)),intent(in   )::this
                  real(        KK ),intent(in   )::weight

                  logical::is_greater_than


                  if(this%m_weight>=weight+average_tolerance) then

                     is_greater_than=.true.

                  else

                     is_greater_than=.false.

              end if!this%m_weight>=weight+average_tolerance


        end function is_greater_than_K!this,weight


            function real_average_greater_than_K(real1,average2) result(real_average_greater_than)


                  implicit none


                  real(        KK ),intent(in   )::   real1
                  type(average(KK)),intent(in   )::average2

                  logical::real_average_greater_than


                  real_average_greater_than=average2%is_greater_than(real1)


        end function real_average_greater_than_K!real1,average2


            function average_real_greater_than_K(average1,real2) result(average_real_greater_than)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  real(        KK ),intent(in   )::   real2

                  logical::average_real_greater_than


                  average_real_greater_than=average1%is_greater_than(real2)


        end function average_real_greater_than_K!average1,real2


            function average_average_greater_than_K(average1,average2) result(average_average_greater_than)


                  implicit none


                  type(average(KK)),intent(in   )::average1
                  type(average(KK)),intent(in   )::average2

                  logical::average_average_greater_than


                  if(average1%m_weight>=average2%m_weight+average_tolerance) then

                     average_average_greater_than=.true.

                  else

                     average_average_greater_than=.false.

              end if!average1%m_weight>=average2%m_weight+average_tolerance


        end function average_average_greater_than_K!average1,average2


            subroutine  read_average_K(unit,this,iostat)


                  implicit none


                  integer          ,intent(inout)         ::unit
                  type(average(KK)),intent(inout)         ::this
                  integer          ,intent(  out),optional::iostat


                  if(present(iostat)) then

                      read(unit,format_average_K,advance="no",iostat=iostat,end=116) this%m_weight
                      read(unit,format_average_K,advance="no",iostat=iostat,end=116) this%m_value

              116    return

                  else

                      read(unit,format_average_K,advance="no") this%m_weight
                      read(unit,format_average_K,advance="no") this%m_value

              end if!present(iostat)


        end subroutine  read_average_K!unit,this,iostat


            subroutine write_average_K(unit,this,iostat)


                  implicit none


                  integer          ,intent(inout)         ::unit
                  type(average(KK)),intent(in   )         ::this
                  integer          ,intent(  out),optional::iostat


                  if(present(iostat)) then

                     write(unit,format_average_K,advance="no",iostat=iostat,end=117) this%m_weight
                     write(unit,format_average_K,advance="no",iostat=iostat,end=117) this%m_value

              117    return

                  else

                     write(unit,format_average_K,advance="no") this%m_weight
                     write(unit,format_average_K,advance="no") this%m_value

              end if!present(iostat)


        end subroutine write_average_K!unit,this,iostat


  end module average_type


#  endif
