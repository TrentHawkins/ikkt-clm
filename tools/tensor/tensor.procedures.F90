#     ifndef TENSOR_TENSOR_PROCEDURES_F90
#     define TENSOR_TENSOR_PROCEDURES_F90

#     include "../../system/precision.F90"

#     include "../../tools/insert_sort.F90"
#     include "../../tools/constants.F90"
#     include "../../tools/tensor/tensor.F90"
#     include "../../tools/tensor/tensor.operations.F90"


      module tensor_procedures


            use::blas95,only:dotu,&
                             dotc,&
                             gemv,&
                             gemm

            use::insertion_sort
            use::mathematical_constants
            use::tensor_type
            use::tensor_operations


            implicit none


            interface trace

               module procedure matrix_trace_K

        end interface trace

            interface determinant

               module procedure matrix_determinant_K

        end interface determinant

            interface conjugate

               module procedure matrix_conjugate_K

        end interface conjugate

            interface     symmetric

               module procedure matrix_symmetric_K

        end interface     symmetric

            interface antisymmetric

               module procedure matrix_antisymmetric_K

        end interface antisymmetric

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

            interface make_symmetric

               module procedure make_matrix_symmetric_K

        end interface make_symmetric

            interface make_antisymmetric

               module procedure make_matrix_antisymmetric_K

        end interface make_antisymmetric

            interface make_hermitian

               module procedure make_matrix_hermitian_K

        end interface make_hermitian

            interface make_antihermitian

               module procedure make_matrix_antihermitian_K

        end interface make_antihermitian


      contains


            function matrix_trace_K(matrix0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK)                                                 ::scalar_

                  integer::index


                  scalar_=(+.00000e+0_KK,&
                           +.00000e+0_KK)

                  do index=0,size(matrix0,dim=1)-1,+1

                     scalar_&
                    =scalar_+matrix0(index,index)

              end do!index=0,size(matrix0,dim=1)-1,+1

!                 scalar_=sum(vector(matrix0))


        end function matrix_trace_K!matrix0


            recursive function matrix_determinant_K(matrix0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK)                                                 ::scalar_

                  integer::k


                  if(size(matrix0)==4) then

                     scalar_=matrix0(0,0)*matrix0(1,1)&
                            -matrix0(0,1)*matrix0(1,0)

                  else

                     scalar_=(+.00000e+0_KK,&
                              +.00000e+0_KK)

                     do k=0,size(matrix0,dim=1)-1,+1

                        scalar_&
                       =scalar_+matrix0(k,0)*matrix_determinant_K(cofactor(matrix0,k,0))

              end    do!k=0,size(matrix0,dim=1)-1,+1

              end if!size(matrix0,dim=1)-1==0


            contains


                  function cofactor(matrix0,i0,j0) result(matrix_)


                        implicit none


                        complex(KK),dimension(0:                     ,&
                                              0:                     ),intent(in   )::matrix0
                        complex(KK),dimension(0:size(matrix0,dim=1)-2,&
                                              0:size(matrix0,dim=2)-2)              ::matrix_

                        integer,intent(in   )::i0
                        integer,intent(in   )::j0
                        integer              ::i
                        integer              ::j


                        do j=0,size(matrix_,dim=2)-1,+1

                           do i=0,size(matrix_,dim=1)-1,+1

                              matrix_(i,j)=matrix0(mod(i0+1+i,size(matrix0,dim=1)),&
                                                   mod(j0+j+1,size(matrix0,dim=2)))*sign(i0+j0)

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


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=conjg(matrix0(j,i))

              end    do!i=0,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=conjg(transpose(matrix0))


        end function matrix_conjugate_K!matrix0


            function matrix_symmetric_K(matrix0) result(matrix_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j


                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix_(i,j)=(matrix0(j,i)+matrix0(i,j))*(+.50000e+0_KK,&
                                                                  +.00000e+0_KK)

              end    do!i=0,j-1,+1

                     matrix_(j,j)=matrix0(j,j)

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=matrix_(j,i)

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=(transpose(matrix0)+matrix0)*(+.50000e+0_KK,&
!                                                       +.00000e+0_KK)


        end function matrix_symmetric_K!matrix0


            function matrix_antisymmetric_K(matrix0) result(matrix_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix_(i,j)=(matrix0(j,i)-matrix0(i,j))*(+.00000e+0_KK,&
                                                                  +.50000e+0_KK)

              end    do!i=0,j-1,+1

                     matrix_(j,j)=matrix0(j,j)

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=-matrix_(j,i)

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=(transpose(matrix0)-matrix0)*(+.00000e+0_KK,&
!                                                       +.50000e+0_KK)


        end function matrix_antisymmetric_K!matrix0


            function matrix_hermitian_K(matrix0) result(matrix_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j


                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix_(i,j)=(conjg(matrix0(j,i))+matrix0(i,j))*(+.50000e+0_KK,&
                                                                         +.00000e+0_KK)

              end    do!i=0,j-1,+1

                     matrix_(j,j)=real(matrix0(j,j))

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=(conjugate(matrix0)+matrix0)*(+.50000e+0_KK,&
!                                                       +.00000e+0_KK)


        end function matrix_hermitian_K!matrix0


            function matrix_antihermitian_K(matrix0) result(matrix_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix0,dim=2)-1)              ::matrix_

                  integer::i,j

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix_(i,j)=(conjg(matrix0(j,i))-matrix0(i,j))*(+.00000e+0_KK,&
                                                                         +.50000e+0_KK)

              end    do!i=0,j-1,+1

                     matrix_(j,j)=aimag(matrix0(j,j))

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=-conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

!                 matrix_=(conjugate(matrix0)-matrix0)*(+.00000e+0_KK,&
!                                                       +.50000e+0_KK)


        end function matrix_antihermitian_K!matrix0


            function scalar_norm_squared_K(scalar0) result(scalar_)


                  implicit none


                  complex(KK)                                   ,intent(in   )::scalar0
                  real(   KK)                                                 ::scalar_


                  scalar_=         conjg(scalar0)*scalar0


        end function scalar_norm_squared_K!scalar0


            function vector_norm_squared_K(vector0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  real(   KK)                                                 ::scalar_


                  scalar_=real(sum(conjg(vector0)*vector0))


        end function vector_norm_squared_K!vector0


            function matrix_norm_squared_K(matrix0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  real(   KK)                                                 ::scalar_


                  scalar_=real(sum(conjg(matrix0)*matrix0))

        end function matrix_norm_squared_K!matrix0


            function vector_matrix_norm_squared_K(vector0,matrix0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:size(vector0,dim=1)-1,&
                                        0:size(vector0,dim=1)-1),intent(in   )::matrix0
                  real(   KK)                                                 ::scalar_


                  scalar_=real(conjg(vector0).o.matrix0.o.vector0)


        end function vector_matrix_norm_squared_K!vector0,matrix0


            subroutine make_matrix_eigenmatrix_K(matrix0,eigenvalue)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(KK)                                   ,intent(inout)::eigenvalue

                  integer::index


                  do index=0,size(matrix0,dim=1)-1,+1

                     matrix0(index,index)=matrix0(index,index)-eigenvalue

              end do!index=0,size(matrix0,dim=1)-1,+1

!                 matrix0=matrix0-matrix(size(matrix0,dim=1),eigenvalue)


        end subroutine make_matrix_eigenmatrix_K!matrix0,eigenvalue


            subroutine make_matrix_traceless_K(matrix0)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0

!                 call make_eigenmatrix(matrix0,trace_piece(matrix0))

!                 call make_eigenmatrix(matrix0,trace(matrix0)/real(size(matrix0,dim=1)))

                  matrix0(size(matrix0,dim=1)-1,size(matrix0,dim=2)-1)&
                 =matrix0(size(matrix0,dim=1)-1,size(matrix0,dim=2)-1)-trace(matrix0)


            contains


                  function trace_piece(matrix0) result(scalar_)


                        implicit none


                        complex(KK),dimension(0:                     ,&
                                              0:                     ),intent(in   )::matrix0
                        complex(KK)                                                 ::scalar_

                        real(KK):: real_vector_matrix0(0:size(matrix0,dim=1)-1)
                        real(KK)::aimag_vector_matrix0(0:size(matrix0,dim=1)-1)


                         real_vector_matrix0= real(vector(matrix0))
                        aimag_vector_matrix0=aimag(vector(matrix0))

                        scalar_=cmplx(sort_sum( real_vector_matrix0),&
                                      sort_sum(aimag_vector_matrix0),kind=KK)


              end function trace_piece!matrix0


        end subroutine make_matrix_traceless_K!matrix0


            subroutine make_matrix_symmetric_K(matrix0,factor)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)         ::matrix0
                  real(   KK),                                   intent(inout),optional::factor

                  integer::i,j


                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(matrix0(j,i)+matrix0(i,j))*(+.50000e+0_KK,&
                                                                  +.00000e+0_KK)

              end    do!i=0,j-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=j+1,size(matrix0,dim=1)-1,+1

                        matrix0(i,j)=matrix0(j,i)

              end    do!i=j+1,size(matrix0,dim=1)-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1

                  if(present(factor)) matrix0&
                                     =matrix0*factor


        end subroutine make_matrix_symmetric_K!matrix0,factor


            subroutine make_matrix_antisymmetric_K(matrix0,factor)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)         ::matrix0
                  real(   KK),                                   intent(inout),optional::factor


                  integer::i,j

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(matrix0(j,i)-matrix0(i,j))*(+.00000e+0_KK,&
                                                                  +.50000e+0_KK)

              end    do!i=0,j-1,+1

                     matrix0(j,j)=zero

              end do!j=0,size(matrix0,dim=2)-1,+1

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=j+1,size(matrix0,dim=1)-1,+1

                        matrix0(i,j)=-matrix0(j,i)

              end    do!i=j+1,size(matrix0,dim=1)-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1

                  if(present(factor)) matrix0&
                                     =matrix0*factor


        end subroutine make_matrix_antisymmetric_K!matrix0,factor


            subroutine make_matrix_hermitian_K(matrix0,factor)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)         ::matrix0
                  real(   KK),                                   intent(inout),optional::factor

                  integer::i,j


                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(conjg(matrix0(j,i))+matrix0(i,j))*(+.50000e+0_KK,&
                                                                         +.00000e+0_KK)

              end    do!i=0,j-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1

                     matrix0(j,j)=real(matrix0(j,j))

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


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)         ::matrix0
                  real(   KK),                                   intent(inout),optional::factor


                  integer::i,j

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(conjg(matrix0(j,i))-matrix0(i,j))*(+.00000e+0_KK,&
                                                                         +.50000e+0_KK)

              end    do!i=0,j-1,+1

                     matrix0(j,j)=aimag(matrix0(j,j))

              end do!j=0,size(matrix0,dim=2)-1,+1

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=j+1,size(matrix0,dim=1)-1,+1

                        matrix0(i,j)=-conjg(matrix0(j,i))

              end    do!i=j+1,size(matrix0,dim=1)-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1

                  if(present(factor)) matrix0&
                                     =matrix0*factor


        end subroutine make_matrix_antihermitian_K!matrix0,factor


  end module tensor_procedures


#     endif
