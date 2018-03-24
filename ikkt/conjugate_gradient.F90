#     ifndef CONJUGATE_GRADIENT_F90
#     define CONJUGATE_GRADIENT_F90

#     include "../system/precision.F90"
#     include "../system/text_format.F90"

#     include "../tensor/tensor.F90"

#     include "../ikkt/fields.F90"
#     include "../ikkt/optimal_toolset.F90"


      module conjugate_gradient_method


            use::text_formatting

            use::tensor_type

            use::fields
            use::optimal_toolset


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


                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::b
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1),intent(inout)::x

                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::r
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::p
                  complex(KK),dimension(0:inner_degrees_of_freedom-1,&
                                        0:inner_degrees_of_freedom-1,&
                                        0:fermi_degrees_of_freedom-1)::dx


                  real(KK)::cb
                  real(KK)::ca

                  real(KK)::norm_r_old
                  real(KK)::norm_r_new


                  r=b-cmmv(a,x)
                  p=r

                  norm_r_old=ucv(r,r)

                  iterations=0

                  do while(sqrt(norm_r_old)>=tolerance)

                     cb=-norm_r_old/ucmmv(r,a,r)

                         dx=cb*       p
                     x=x-dx
                     r=r   +cb*cmmv(a,p)

                     norm_r_new=ucv(r,r)

                     ca=norm_r_new&
                       /norm_r_old

                        norm_r_old&
                       =norm_r_new

                     p=r   +ca*       p

                     iterations&
                    =iterations+1

              end do!while(sqrt(norm_r_old)>=tolerance)

                  error=sqrt(ucv(dx,dx)/ucv(x,x))


        end subroutine conjugate_gradient_K!a,b,x,iterations


            subroutine print_conjugate_gradient(unit)


                  implicit none


                  integer,intent(inout)::unit


                  write(unit,format_iterations,advance="no") iterations
                  write(unit,format_error_K   ,advance="no") error
                  write(unit,format_error_K                ) tolerance


        end subroutine print_conjugate_gradient!unit


  end module conjugate_gradient_method


#  endif
