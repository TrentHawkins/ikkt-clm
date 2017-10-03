#     ifndef GAUGE_COOLING_F90
#     define GAUGE_COOLING_F90

#     include "../system/precision.F90"

#     include "../tensor/tensor.F90"

#     include "../ikkt/fields.F90"

#     include "../tools/brent_minimization.F90"


      module gauge_cooling


            use::lapack95,only:heev

            use::tensor_type

            use::fields

            use::brent_minimization


            implicit none


            character(*),parameter,private::           format_cooling=COMPLEXGK
            character(*),parameter,private::text_field_format_cooling=COMPLEXAK

            real(KK),parameter,private::cooling_tolerance=TOLERANCEK

            real(KK),private::min_alpha=TOLERANCEK*1
            real(KK),private::mid_alpha=TOLERANCEK*10
            real(KK),private::max_alpha=TOLERANCEK*100

            logical,public::gauge_cooling_active=.false.

            complex(KK),dimension( :                          ,&
                                   :                          ),allocatable,private::h
            real(   KK),dimension( :                          ),allocatable,private::h_eigenvalues

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

                  h=+.00000e+0_KK

                  do mu=0,boson_degrees_of_freedom-1,+1

                     h(:,:)&
                    =h(:,:)+(conjugate(a(:,:,mu)).commutation.a(:,:,mu))/inner_degrees_of_freedom

              end do!mu=0,boson_degrees_of_freedom-1,+1

                  call heev(h,h_eigenvalues,jobz='v')


        end subroutine make_cooler!


            function h_diagonalized(alpha)


                  implicit none


                  real(KK),intent(in   )::alpha

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1)::h_diagonalized


                  h_diagonalized=h.o.matrix(cmplx(exp(alpha*h_eigenvalues),kind=KK)).o.conjugate(h)


        end function h_diagonalized!alpha


            function hermiticity_norm()


                  implicit none


                  real(KK)::hermiticity_norm

                  integer::mu


                  hermiticity_norm=+.00000e+0_KK

                  do mu=0,boson_degrees_of_freedom-1,+1

                     hermiticity_norm&
                    =hermiticity_norm+norm(a(:,:,mu)-conjugate(a(:,:,mu)))*inner_degrees_of_freedom/4

              end do!mu=0,boson_degrees_of_freedom-1,+1


        end function hermiticity_norm!


            function guided_hermiticity_norm(alpha)


                  implicit none


                  real(KK),intent(in   )::alpha

                  real(KK)::guided_hermiticity_norm

                  integer::mu


                  guided_hermiticity_norm=+.00000e+0_KK

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


                  real(KK)::alpha_min
                  real(KK)::min_hermiticity_norm

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

                  real(KK)::step

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


#  endif
