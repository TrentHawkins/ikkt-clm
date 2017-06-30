#     ifndef TENSOR_TENSOR_F90
#     define TENSOR_TENSOR_F90

#     include "main/precision.F90"

#     include "tools/insert_sort.F90"


      module tensor_type


            use::blas95,only:dotu,&
                             dotc,&
                             gemv,&
                             gemm

            use::insertion_sort


            implicit none


            character(*),private,parameter::           format_tensor_K=COMPLEXGK
            character(*),private,parameter::text_field_format_tensor_K=COMPLEXAK


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


                  complex(KK),dimension(0:                  ,&
                                        0:                  ),intent(in   )::this
                  complex(KK),dimension(0:size(this,dim=1)-1)              ::that

                  integer::index


                  do index=0,size(that,dim=1)-1,+1

                     that(index)=this(index,index)

              end do!index=0,size(that,dim=1)-1,+1


        end function vector_matrix_constructor_K!this


            function matrix_factor_constructor_K(size,factor) result(that)


                  implicit none


                  integer    ,                    intent(in   )::size
                  complex(KK),                    intent(in   )::factor
                  complex(KK),dimension(0:size-1,&
                                        0:size-1)              ::that

                  integer::index


                  that=(+.00000e+0_KK,&
                        +.00000e+0_KK)

                  do index=0,size-1,+1

                     that(index,index)=factor

              end do!index=0,size-1,+1


        end function matrix_factor_constructor_K!size,factor


            function matrix_vector_constructor_K(this) result(that)


                  implicit none


                  complex(KK),dimension(0:                  ),intent(in   )::this
                  complex(KK),dimension(0:size(this,dim=1)-1,&
                                        0:size(this,dim=1)-1)              ::that

                  integer::index


                  that=(+.00000e+0_KK,&
                        +.00000e+0_KK)

                  do index=0,size(that,dim=1)-1,+1

                     that(index,index)=this(index)

              end do!index=0,size(that,dim=1)-1,+1


        end function matrix_vector_constructor_K!this


            function vector_vector_scalar_contraction_real_K(vector0,vector1) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:size(vector0,dim=1)-1),intent(in   )::vector1
                  complex(KK)                                                 ::scalar_


#                 ifndef BLAS

                  scalar_=sum(vector0*vector1)

#                 else

                  scalar_=dotu(vector0,vector1)

#              endif


        end function vector_vector_scalar_contraction_real_K!vector0,vector1


            function vector_matrix_vector_contraction_real_K(vector0,matrix1) result(vector_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix1,dim=2)-1)              ::vector_


#                 ifndef BLAS

                  vector_=matmul(      vector0 ,matrix1)

#                 else

                  call gemv(matrix1,      vector0 ,vector_,trans='T')

#              endif


        end function vector_matrix_vector_contraction_real_K!vector0,matrix1


            function matrix_vector_vector_contraction_real_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=2)-1),intent(in   )::vector1
                  complex(KK),dimension(0:size(matrix0,dim=1)-1)              ::vector_


#                 ifndef BLAS

                  vector_=matmul(matrix0 ,vector1)

#                 else

                  call gemv(matrix0,vector1,vector_,trans='N')

#              endif


        end function matrix_vector_vector_contraction_real_K!matrix0,vector1


            function matrix_matrix_matrix_contraction_real_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


#                 ifndef BLAS

                  matrix_=matmul(matrix0,matrix1)

#                 else

                  call gemm(matrix0,matrix1,matrix_,transa='N',transb='N')

#              endif


        end function matrix_matrix_matrix_contraction_real_K!matrix0,matrix1


            function vector_vector_vector_ext_product_real_K(vector0,vector1) result(vector_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:                     ),intent(in   )::vector1
                  complex(KK),dimension(0:size(vector0,dim=1)   &
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


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix0,dim=1)   &
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


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:size(vector0,dim=1)-1),intent(in   )::vector1
                  complex(KK)                                                 ::scalar_


#                 ifndef BLAS

                  scalar_=dot_product(vector0,vector1)

#                 else

                  scalar_=dotc(vector0,vector1)

#              endif


        end function vector_vector_scalar_contraction_complex_K!vector0,vector1


            function vector_matrix_vector_contraction_complex_K(vector0,matrix1) result(vector_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix1,dim=2)-1)              ::vector_


#                 ifndef BLAS

                  vector_=matmul(conjg(vector0),matrix1)

#                 else

                  call gemv(matrix1,conjg(vector0),vector_,trans='T')

#              endif


        end function vector_matrix_vector_contraction_complex_K!vector0,matrix1


            function matrix_vector_vector_contraction_complex_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=2)-1),intent(in   )::vector1
                  complex(KK),dimension(0:size(matrix0,dim=1)-1)              ::vector_


#                 ifndef BLAS

                  vector_=matmul(conjg(matrix0),vector1)

#                 else

                  call gemv(conjg(matrix0),vector1,vector_,trans='N')

#              endif


        end function matrix_vector_vector_contraction_complex_K!matrix0,vector1


            function matrix_matrix_matrix_contraction_complex_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


#                 ifndef BLAS

                  matrix_=matmul(conjg(matrix0),matrix1)

#                 else

                  call gemm(conjg(matrix0),matrix1,matrix_,transa='N',transb='N')

#              endif


        end function matrix_matrix_matrix_contraction_complex_K!matrix0,matrix1


            function vector_vector_vector_ext_product_complex_K(vector0,vector1) result(vector_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:                     ),intent(in   )::vector1
                  complex(KK),dimension(0:size(vector0,dim=1)   &
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


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix0,dim=1)   &
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


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=2)-1,&
                                        0:size(matrix0,dim=1)-1),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


                  matrix_=(matrix0.o.matrix1)&
                         -(matrix1.o.matrix0)


        end function matrix_matrix_matrix_commutation_K!matrix0,matrix1


            function matrix_matrix_matrix_anticommutation_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=2)-1,&
                                        0:size(matrix0,dim=1)-1),intent(in   )::matrix1
                  complex(KK),dimension(0:size(matrix0,dim=1)-1,&
                                        0:size(matrix1,dim=2)-1)              ::matrix_


                  matrix_=(matrix0.o.matrix1)&
                         +(matrix1.o.matrix0)


        end function matrix_matrix_matrix_anticommutation_K!matrix0,matrix1


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

      !           scalar_=sum(vector(matrix0))


        end function matrix_trace_K!matrix0


            function matrix_trace_piece_K(matrix0) result(scalar_)


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


        end function matrix_trace_piece_K!matrix0


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

            !     matrix_=conjg(transpose(matrix0))


        end function matrix_conjugate_K!matrix0


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

                     matrix_(j,j)=real(matrix0(i,j))

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

            !     matrix_=(conjugate(matrix0)+matrix0)*(+.50000e+0_KK,&
            !                                           +.00000e+0_KK)


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

                     matrix_(j,j)=aimag(matrix0(i,j))

              end do!j=0,size(matrix_,dim=2)-1,+1

                  do j=0,size(matrix_,dim=2)-1,+1

                     do i=j+1,size(matrix_,dim=1)-1,+1

                        matrix_(i,j)=-conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix_,dim=1)-1,+1

              end do!j=0,size(matrix_,dim=2)-1,+1

            !     matrix_=(conjugate(matrix0)-matrix0)*(+.00000e+0_KK,&
            !                                           +.50000e+0_KK)


        end function matrix_antihermitian_K!matrix0


            function vector_norm_squared_K(vector0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  real(   KK)                                                 ::scalar_


                  scalar_=real(  sum(conjg(vector0)* vector0))
            !     scalar_=real(            vector0.c.vector0)


        end function vector_norm_squared_K!vector0


            function matrix_norm_squared_K(matrix0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  real(   KK)                                                 ::scalar_


                  scalar_=real(  sum(conjg(matrix0)* matrix0))
            !     scalar_=real(trace(      matrix0.c.matrix0))

        end function matrix_norm_squared_K!matrix0


            function vector_matrix_norm_squared_K(vector0,matrix0) result(scalar_)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(in   )::vector0
                  complex(KK),dimension(0:size(vector0,dim=1)-1,&
                                        0:size(vector0,dim=1)-1),intent(in   )::matrix0
                  real(   KK)                                                 ::scalar_


                  scalar_=real(vector0.c.(matrix0.o.vector0))


        end function vector_matrix_norm_squared_K!vector0,matrix0


            subroutine make_matrix_eigenmatrix_K(matrix0,eigenvalue)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  complex(KK)                                   ,intent(in   )::eigenvalue

                  integer::index


                  do index=0,size(matrix0,dim=1)-1,+1

                     matrix0(index,index)=matrix0(index,index)-eigenvalue

              end do!index=0,size(matrix0,dim=1)-1,+1

            !     matrix0=matrix0-matrix(size(matrix0,dim=1),eigenvalue)


        end subroutine make_matrix_eigenmatrix_K!matrix0,eigenvalue


            subroutine make_matrix_traceless_K(matrix0)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0

                  call make_eigenmatrix(matrix0,trace_piece(matrix0))
            !     call make_eigenmatrix(matrix0,trace(      matrix0)/real(size(matrix0,dim=1)))


        end subroutine make_matrix_traceless_K!matrix0


            subroutine make_matrix_hermitian_K(matrix0,factor)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  real(   KK),                                   intent(in   )::factor

                  integer::i,j


                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(conjg(matrix0(j,i))+matrix0(i,j))*(+.50000e+0_KK,&
                                                                         +.00000e+0_KK)*factor

              end    do!i=0,j-1,+1

                     matrix0(j,j)=real(matrix0(i,j))*factor

              end do!j=0,size(matrix0,dim=2)-1,+1

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=j+1,size(matrix0,dim=1)-1,+1

                        matrix0(i,j)=conjg(matrix0(j,i))

              end    do!i=j+1,size(matrix0,dim=1)-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1


        end subroutine make_matrix_hermitian_K!matrix0,factor


            subroutine make_matrix_antihermitian_K(matrix0,factor)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0
                  real(   KK),                                   intent(in   )::factor


                  integer::i,j

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=0,j-1,+1

                        matrix0(i,j)=(conjg(matrix0(j,i))-matrix0(i,j))*(+.00000e+0_KK,&
                                                                         +.50000e+0_KK)*factor

              end    do!i=0,j-1,+1

                     matrix0(j,j)=aimag(matrix0(i,j))

              end do!j=0,size(matrix0,dim=2)-1,+1

                  do j=0,size(matrix0,dim=2)-1,+1

                     do i=j+1,size(matrix0,dim=1)-1,+1

                        matrix0(i,j)=-conjg(matrix0(j,i))

              end    do!i=j+1,size(matrix0,dim=1)-1,+1

              end do!j=0,size(matrix0,dim=2)-1,+1


        end subroutine make_matrix_antihermitian_K!matrix0,factor


            subroutine vector_random_number_K(vector0)


                  implicit none


                  complex(KK),dimension(0:                     ),intent(inout)::vector0

                  real(KK),dimension(0:size(vector0,dim=1)-1):: real_vector0
                  real(KK),dimension(0:size(vector0,dim=1)-1)::aimag_vector0


                  call random_number( real_vector0)
                  call random_number(aimag_vector0)


                  vector0=cmplx( real_vector0,&
                                aimag_vector0,kind=KK)


        end subroutine vector_random_number_K!vector0


            subroutine matrix_random_number_K(matrix0)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(inout)::matrix0

                  real(KK),dimension(0:size(matrix0,dim=1)-1,&
                                     0:size(matrix0,dim=2)-1):: real_matrix0
                  real(KK),dimension(0:size(matrix0,dim=1)-1,&
                                     0:size(matrix0,dim=2)-1)::aimag_matrix0


                  call random_number( real_matrix0)
                  call random_number(aimag_matrix0)


                  matrix0=cmplx( real_matrix0,&
                                aimag_matrix0,kind=KK)


        end subroutine matrix_random_number_K!matrix0


            subroutine  read_vector_K(unit,vector0)


                  implicit none


                  integer    ,                                   intent(in   )::unit
                  complex(KK),dimension(0:                     ),intent(inout)::vector0

                  integer::i


                  do i=0,size(vector0,dim=1)-1,+1

                      read(unit,format_tensor_K) vector0(i)

              end do!i=0,size(vector0,dim=1)-1,+1

                   read(unit,*)


        end subroutine  read_vector_K!unit,vector0


            subroutine  read_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,                                   intent(in   )::unit
                  complex(KK),dimension(0:                     ,&
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
                  complex(KK),dimension(0:                     ),intent(in   )::vector0

                  integer::i


                  do i=0,size(vector0,dim=1)-1,+1

                     write(unit,format_tensor_K) vector0(i)

              end do!i=0,size(vector0,dim=1)-1,+1

                  write(unit,*)


        end subroutine write_vector_K!unit,vector0


            subroutine write_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,                                   intent(in   )::unit
                  complex(KK),dimension(0:                     ,&
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


#     endif
