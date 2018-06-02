#     ifndef TENSOR_TENSOR_OPERATIONS_F90
#     define TENSOR_TENSOR_OPERATIONS_F90

#     include "../../system/precision.F90"

#     include "../../tools/constants.F90"
#     include "../../tools/tensor/tensor.F90"


      module tensor_operations


            use::blas95,only:dotu,&
                             dotc,&
                             gemv,&
                             gemm

            use::mathematical_constants
            use::tensor_type


            implicit none


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


      contains


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

                  vector_=matmul(vector0,matrix1)

#                 else

                  call gemv(matrix1,vector0,vector_,trans='T')

#              endif


        end function vector_matrix_vector_contraction_real_K!vector0,matrix1


            function matrix_vector_vector_contraction_real_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(KK),dimension(0:                     ,&
                                        0:                     ),intent(in   )::matrix0
                  complex(KK),dimension(0:size(matrix0,dim=2)-1),intent(in   )::vector1
                  complex(KK),dimension(0:size(matrix0,dim=1)-1)              ::vector_


#                 ifndef BLAS

                  vector_=matmul(matrix0,vector1)

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

                  vector_=matmul(conjg(transpose(matrix0)),vector1)

#                 else

                  call gemv(matrix0,vector1,vector_,trans='C')

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

                  matrix_=matmul(conjg(transpose(matrix0)),matrix1)

#                 else

                  call gemm(matrix0,matrix1,matrix_,transa='C',transb='N')

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


  end module tensor_operations


#     include "../../tools/tensor/tensor.procedures.F90"

#     endif
