#     ifndef CONJUGATE_GRADIENT_F90
#     define CONJUGATE_GRADIENT_F90

#     include "../system/precision.F90"
#     include "../system/text_format.F90"

#     include "../tensor/tensor.F90"


      module conjugate_gradient_method


            use::text_formatting

            use::tensor_type


            implicit none


            character(*),private,parameter::           format_iterations=INTEGERG2
            character(*),private,parameter::text_field_format_iterations=INTEGERA2

            character(*),private,parameter::           format_error_K=REALGK
            character(*),private,parameter::text_field_format_error_K=REALAK

            real(KK),parameter::tolerance=TOLERANCEK

            integer ,private::iterations
            real(KK),private::error

            public::      conjugate_gradient_K
            public::print_conjugate_gradient


            interface conjugate_gradient

                  module procedure conjugate_gradient_K

        end interface conjugate_gradient


      contains


            subroutine conjugate_gradient_K(a,b,x)


                  implicit none


                  complex(KK),dimension( :               ),intent(inout)::b
                  complex(KK),dimension(0:size(b,dim=1)-1,&
                                        0:size(b,dim=1)-1),intent(inout)::a
                  complex(KK),dimension(0:size(b,dim=1)-1),intent(inout)::x

                  integer,intent(inout)::iterations

                  complex(KK),dimension(0:size(b,dim=1)-1)::r
                  complex(KK),dimension(0:size(b,dim=1)-1)::p

                  real(KK)::cb
                  real(KK)::ca

                  real(KK)::dx

                  real(KK)::norm_r_old
                  real(KK)::norm_r_new


                  r=b-(a.o.x)
                  p=r

                  norm_r_old=norm(r)

                  iterations=0

                  do while(sqrt(norm_r_old)>=tolerance)

                     cb=-norm_r_old/norm(r,a)

                         dx=cb*     p
                     x=x-dx
                     r=r   +cb*(a.o.p)

                     norm_r_new=norm(r)

                     ca=norm_r_new&
                       /norm_r_old

                        norm_r_old&
                       =norm_r_new

                     p=r   +ca*     p

                     iterations&
                    =iterations+1

              end do!while(sqrt(norm_r_old)>=tolerance)

                  error=sqrt(norm(dx)/norm(x))


        end subroutine conjugate_gradient_K!a,b,x,iterations


            subroutine print_conjugate_gradient(unit,tag)


                  implicit none


                  integer     ,intent(inout)::unit
                  character(*),intent(inout)::tag


                  call write(unit,size(tag),tag)

                  write(unit,format_iterations,advance="no") iterations
                  write(unit,format_error_K   ,advance="no") error
                  write(unit,format_error_K   ,advance="no") tolerance


        end subroutine print_conjugate_gradient!unit,tag


  end module conjugate_gradient_method


#  endif
