#     ifndef MONTE_CARLO_AVERAGE_F90
#     define MONTE_CARLO_AVERAGE_F90

#     include "../../system/precision.F90"

#     include "../../monte_carlo/average/average.F90"


      module average_type


            use::average_type


            implicit none


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


      contains


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


  end module average_type


#  endif
