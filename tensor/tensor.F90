#     ifndef TENSOR_TENSOR_F90
#     define TENSOR_TENSOR_F90

#     include "precision.F90"


      module tensor_type


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

               module procedure vector_vector_scalar_contraction_K

               module procedure vector_matrix_vector_contraction_K
               module procedure matrix_vector_vector_contraction_K
               module procedure matrix_matrix_matrix_contraction_K

        end interface operator(.o.)

            interface operator(.x.)

               module procedure vector_vector_vector_ext_product_K

               module procedure matrix_matrix_matrix_ext_product_K

        end interface operator(.x.)

            interface operator(    .commutation.)

               module procedure matrix_matrix_matrix_____commutation_K

        end interface operator(    .commutation.)

            interface operator(.anticommutation.)

               module procedure matrix_matrix_matrix_anticommutation_K

        end interface operator(.anticommutation.)

            interface trace

               module procedure matrix_trace_K

        end interface trace

            interface determinant

               module procedure matrix_determinant_K

        end interface determinant


            interface conjugate

               module procedure matrix_conjugate_K

        end interface conjugate

            interface     hermitian

               module procedure matrix_____hermitian_K

        end interface     hermitian

            interface antihermitian

               module procedure matrix_antihermitian_K

        end interface antihermitian

            interface norm

               module procedure vector_norm_squared_K
               module procedure matrix_norm_squared_K

               module procedure vector_matrix_norm_squared_K

        end interface norm

            interface eigenmatrix

               module procedure make_matrix_eigenmatrix_K

        end interface eigenmatrix

            interface traceless

               module procedure make_matrix_traceless_K

        end interface traceless

            interface hermitian

               module procedure make_matrix_hermitian_K

        end interface hermitian

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


                  complex(KK),intent(in   )::this(1:                ,&
                                                  1:                )
                  complex(KK)              ::that(1:size(this,dim=1))

                  integer::index


                  do index=1,size(that,dim=1),+1

                     that(index)=this(index,index)

              end do!index=1,size(that,dim=1),+1


        end function vector_matrix_constructor_K!this


            function matrix_factor_constructor_K(size,factor) result(that)


                  implicit none


                  integer    ,intent(in   )::size
                  complex(KK),intent(in   )::factor
                  complex(KK)              ::that(1:size,1:size)

                  integer::index


                  that=(00.00_KK,&
                        00.00_KK)

                  do index=1,size,+1

                     that(index,index)=factor

              end do!index=1,size,+1


        end function matrix_factor_constructor_K!size,factor


            function matrix_vector_constructor_K(this) result(that)


                  implicit none


                  complex(KK),intent(in   )::this(1:                )
                  complex(KK)              ::that(1:size(this,dim=1),&
                                                  1:size(this,dim=1))

                  integer::index


                  that=(00.00_KK,&
                        00.00_KK)

                  do index=1,size(that,dim=1),+1

                     that(index,index)=this(index)

              end do!index=1,size(that,dim=1),+1


        end function matrix_vector_constructor_K!this


            function vector_vector_scalar_contraction_K(vector0,vector1) result(scalar_)


                  implicit none


                  complex(KK),intent(in   )::vector0(1:                   )
                  complex(KK),intent(in   )::vector1(1:                   )
                  complex(KK)              ::scalar_


                  scalar_=dot_product(vector0,vector1)


        end function vector_vector_scalar_contraction_K!vector0,vector1


            function vector_matrix_vector_contraction_K(vector0,matrix1) result(vector_)


                  implicit none


                  complex(KK),intent(in   )::vector0(1:                   )
                  complex(KK),intent(in   )::matrix1(1:                   ,&
                                                     1:                   )
                  complex(KK)              ::vector_(1:size(matrix1,dim=2))


                  vector_=matmul(vector0,matrix1)


        end function vector_matrix_vector_contraction_K!vector0,matrix1


            function matrix_vector_vector_contraction_K(matrix0,vector1) result(vector_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                   ,&
                                                     1:                   )
                  complex(KK),intent(in   )::vector1(1:                   )
                  complex(KK)              ::vector_(1:size(matrix0,dim=1))


                  vector_=matmul(matrix0,vector1)


        end function matrix_vector_vector_contraction_K!matrix0,vector1


            function matrix_matrix_matrix_contraction_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                   ,&
                                                     1:                   )
                  complex(KK),intent(in   )::matrix1(1:                   ,&
                                                     1:                   )
                  complex(KK)              ::matrix_(1:size(matrix0,dim=1),&
                                                     1:size(matrix1,dim=2))


                  matrix_=matmul(matrix0,matrix1)


        end function matrix_matrix_matrix_contraction_K!matrix0,matrix1


            function vector_vector_vector_ext_product_K(vector0,vector1) result(vector_)


                  implicit none


                  complex(KK),intent(in   )::vector0(1:                                       )
                  complex(KK),intent(in   )::vector1(1:                                       )
                  complex(KK)              ::vector_(1:size(vector0,dim=1)*size(vector1,dim=1))

                  integer::i0
                  integer::i1


                  do i0=1,size(vector0,dim=1),+1

                     do i1=1,size(vector1,dim=1),+1

                        vector_(i0*size(vector1,dim=1)+i1)=vector0(i0)*vector1(i1)

              end    do!i1=1,size(vector1,dim=1),+1

              end do!i0=1,size(vector0,dim=1),+1


        end function vector_vector_vector_ext_product_K!vector0,vector1


            function matrix_matrix_matrix_ext_product_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                                       ,&
                                                     1:                                       )
                  complex(KK),intent(in   )::matrix1(1:                                       ,&
                                                     1:                                       )
                  complex(KK)              ::matrix_(1:size(matrix0,dim=1)*size(matrix1,dim=1),&
                                                     1:size(matrix0,dim=2)*size(matrix1,dim=2))

                  integer::i0
                  integer::j0
                  integer::i1
                  integer::j1


                  do j0=1,size(matrix0,dim=2),+1

                     do j1=1,size(matrix1,dim=2),+1

                        do i0=1,size(matrix0,dim=1),+1

                           do i1=1,size(matrix1,dim=1),+1

                              matrix_(i0*size(matrix1,dim=1)+i1,j0*size(matrix1,dim=2)+j1)=matrix0(i0,j0)*matrix1(i1,j1)

              end          do!i1=1,size(matrix1,dim=1),+1

              end       do!i0=1,size(matrix0,dim=1),+1

              end    do!j1=1,size(matrix1,dim=2),+1

              end do!j0=1,size(matrix0,dim=2),+1


        end function matrix_matrix_matrix_ext_product_K!matrix0,matrix1


            function matrix_matrix_matrix_____commutation_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                                       ,&
                                                     1:                                       )
                  complex(KK),intent(in   )::matrix1(1:                                       ,&
                                                     1:                                       )
                  complex(KK)              ::matrix_(1:size(matrix0,dim=1)*size(matrix1,dim=1),&
                                                     1:size(matrix0,dim=2)*size(matrix1,dim=2))


                  matrix_=matrix0.o.matrix1-matrix1.o.matrix0


        end function matrix_matrix_matrix_____commutation_K!matrix0,matrix1


            function matrix_matrix_matrix_anticommutation_K(matrix0,matrix1) result(matrix_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                                       ,&
                                                     1:                                       )
                  complex(KK),intent(in   )::matrix1(1:                                       ,&
                                                     1:                                       )
                  complex(KK)              ::matrix_(1:size(matrix0,dim=1)*size(matrix1,dim=1),&
                                                     1:size(matrix0,dim=2)*size(matrix1,dim=2))


                  matrix_=matrix0.o.matrix1+matrix1.o.matrix0


        end function matrix_matrix_matrix_anticommutation_K!matrix0,matrix1


            function matrix_trace_K(matrix0) result(scalar_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:,&
                                                     1:)
                  complex(KK)              ::scalar_

                  integer::index


                  scalar_=(00.00_KK,&
                           00.00_KK)

                  do index=1,size(matrix0,dim=1),+1

                     scalar_&
                    =scalar_+matrix0(index,index)

              end do!index=1,size(matrix0,dim=1),+1

      !           scalar_=sum(vector(matrix0))


        end function matrix_trace_K!matrix0


            recursive function matrix_determinant_K(matrix0) result(scalar_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:,&
                                                     1:)
                  complex(KK)              ::scalar_

                  integer::k


                  if(size(matrix0,dim=1)==0) then

                     scalar_=matrix0(0,0)

                  else

                     scalar_=(00.00_KK,&
                              00.00_KK)

                     do k=1,size(matrix0,dim=1),+1

                        scalar_&
                       =scalar_+matrix0(k,0)*matrix_determinant_K(cofactor_K(matrix0,k,0))

              end    do!k=1,size(matrix0,dim=1),+1

              end if!size(matrix0,dim=1)==0


            contains


                  function cofactor_K(matrix0,i0,j0) result(matrix_)


                        implicit none


                        complex(KK),intent(in   )::matrix0(1:                     ,&
                                                           1:                     )
                        complex(KK)              ::matrix_(1:size(matrix0,dim=1)-1,&
                                                           1:size(matrix0,dim=2)-1)

                        integer,intent(in   )::i0
                        integer,intent(in   )::j0
                        integer              ::i
                        integer              ::j


                        do j=1,size(matrix_,dim=2),+1

                           do i=1,size(matrix_,dim=1),+1

                              matrix_(i,j)=matrix0(mod(i0+i+1,size(matrix0,dim=1)),mod(j0+j+1,size(matrix0,dim=1)))

                    end    do!i=1,size(matrix_,dim=1),+1

                    end do!j=1,size(matrix_,dim=2),+1


              end function cofactor_K!matrix0,i0,j0


        end           function matrix_determinant_K!matrix0


            function matrix_conjugate_K(matrix0) result(matrix_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                   ,&
                                                     1:                   )
                  complex(KK)              ::matrix_(1:size(matrix0,dim=1),&
                                                     1:size(matrix0,dim=2))

                  integer::i
                  integer::j

                  do j=1,size(matrix_,dim=2),+1

                     do i=1,size(matrix_,dim=1),+1

                        matrix_(i,j)=conjg(matrix0(j,i))

              end    do!i=1,size(matrix_,dim=1),+1

              end do!j=1,size(matrix_,dim=2),+1

            !     matrix_=conjg(transpose(matrix0))


        end function matrix_conjugate_K!matrix0


            function matrix_____hermitian_K(matrix0) result(matrix_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                   ,&
                                                     1:                   )
                  complex(KK)              ::matrix_(1:size(matrix0,dim=1),&
                                                     1:size(matrix0,dim=2))

                  integer::i
                  integer::j


                  do j=1,size(matrix0,dim=2),+1

                     do i=1,j-1,+1

                        matrix_(i,j)=(conjg(matrix0(j,i))+matrix0(i,j))*(00.50_KK,&
                                                                         00.00_KK)

              end    do!i=1,j-1,+1

                     matrix_(j,j)= real(matrix0(i,j),kind=KK)

              end do!j=1,size(matrix0,dim=2),+1

                  do j=1,size(matrix0,dim=2),+1

                     do i=j+1,size(matrix0,dim=1),+1

                        matrix_(i,j)=conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix0,dim=1),+1

              end do!j=1,size(matrix0,dim=2),+1

            !     matrix_=(conjugate(matrix0)+matrix0)*(00.50_KK,&
            !                                           00.00_KK)


        end function matrix_____hermitian_K!matrix0


            function matrix_antihermitian_K(matrix0) result(matrix_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:                   ,&
                                                     1:                   )
                  complex(KK)              ::matrix_(1:size(matrix0,dim=1),&
                                                     1:size(matrix0,dim=2))

                  integer::i
                  integer::j

                  do j=1,size(matrix0,dim=2),+1

                     do i=1,j-1,+1

                        matrix_(i,j)=(conjg(matrix0(j,i))-matrix0(i,j))*(00.00_KK,&
                                                                         00.50_KK)

              end    do!i=1,j-1,+1

                     matrix_(j,j)= real(matrix0(i,j),kind=KK)

              end do!j=1,size(matrix0,dim=2),+1

                  do j=1,size(matrix0,dim=2),+1

                     do i=j+1,size(matrix0,dim=1),+1

                        matrix_(i,j)=conjg(matrix_(j,i))

              end    do!i=j+1,size(matrix0,dim=1),+1

              end do!j=1,size(matrix0,dim=2),+1

            !     matrix_=(conjugate(matrix0)-matrix0)*(00.00_KK,&
            !                                           00.50_KK)


        end function matrix_antihermitian_K!matrix0


            function vector_norm_squared_K(vector0) result(scalar_)


                  implicit none


                  complex(KK),intent(in   )::vector0(1:)
                  real(   KK)              ::scalar_


                  scalar_=real(vector0.o.vector0,kind=KK)


        end function vector_norm_squared_K!vector0


            function matrix_norm_squared_K(matrix0) result(scalar_)


                  implicit none


                  complex(KK),intent(in   )::matrix0(1:,&
                                                     1:)
                  real(   KK)              ::scalar_


                  scalar_=real(trace(conjugate(matrix0).o.matrix0),kind=KK)


        end function matrix_norm_squared_K!matrix0


            function vector_matrix_norm_squared_K(vector0,matrix0) result(scalar_)


                  implicit none


                  complex(KK),intent(in   )::vector0(1:                  )
                  complex(KK),intent(in   )::matrix0(1:size(vector0,dim=1),&
                                                     1:size(vector0,dim=1))
                  real(   KK)              ::scalar_


                  scalar_=real(vector0.o.matrix0.o.vector0,kind=KK)


        end function vector_matrix_norm_squared_K!vector0,matrix0


            subroutine make_matrix_eigenmatrix_K(matrix0,eigenvalue)


                  implicit none


                  complex(KK),intent(inout)::matrix0(1:,&
                                                     1:)
                  complex(KK)              ::eigenvalue

                  integer::index


                  do index=1,size(matrix0,dim=1),+1

                     matrix0(index,index)=matrix0(index,index)-eigenvalue

              end do!index=1,size(matrix0,dim=1),+1

            !     matrix0=matrix0-matrix(size(matrix0,dim=1),eigenvalue)


        end subroutine make_matrix_eigenmatrix_K!matrix0,eigenvalue


            subroutine make_matrix_traceless_K(matrix0)


                  implicit none


                  complex(KK),intent(inout)::matrix0(1:,&
                                                     1:)


                  call eigenmatrix(matrix0,trace(matrix0))


        end subroutine make_matrix_traceless_K!matrix0


            subroutine make_matrix_hermitian_K(matrix0,factor)


                  implicit none


                  complex(KK),intent(inout)::matrix0(1:,&
                                                     1:)
                  real(   KK)              ::factor

                  integer::i
                  integer::j


                  do j=1,size(matrix0,dim=1),+1

                     do i=1,j-1,+1

                        matrix0(i,j)=(conjg(matrix0(j,i))+matrix0(i,j))*factor

              end    do!i=1,j-1,+1

                     matrix0(j,j)= real(matrix0(i,j),kind=KK)*factor

              end do!j=1,size(matrix0,dim=1),+1

                  do j=1,size(matrix0,dim=1),+1

                     do i=j+1,size(matrix0,dim=1),+1

                        matrix0(i,j)=conjg(matrix0(j,i))

              end    do!i=j+1,size(matrix0,dim=1),+1

              end do!j=1,size(matrix0,dim=1),+1


        end subroutine make_matrix_hermitian_K!matrix0


            subroutine vector_random_number_K(vector0)


                  implicit none


                  complex(KK),intent(inout)::vector0(1:)

                  real::vector__real(1:size(vector0,dim=1))
                  real::vector_aimag(1:size(vector0,dim=1))


                  call random_number(vector__real)
                  call random_number(vector_aimag)


                  vector0=cmplx(vector__real,vector_aimag)


        end subroutine vector_random_number_K!vector0


            subroutine matrix_random_number_K(matrix0)


                  implicit none


                  complex(KK),intent(inout)::matrix0(1:,1:)

                  real::matrix__real(1:size(matrix0,dim=1),1:size(matrix0,dim=2))
                  real::matrix_aimag(1:size(matrix0,dim=1),1:size(matrix0,dim=2))


                  call random_number(matrix__real)
                  call random_number(matrix_aimag)


                  matrix0=cmplx(matrix__real,matrix_aimag)


        end subroutine matrix_random_number_K!matrix0


            subroutine  read_vector_K(unit,vector0)


                  implicit none


                  integer    ,intent(in   )::unit
                  complex(KK),intent(inout)::vector0(1:)

                  integer::i


                  do i=1,size(vector0,dim=1),+1

                      read(unit,format_tensor_K) vector0(i)

              end do!i=1,size(vector0,dim=1),+1

                   read(unit,*)


        end subroutine  read_vector_K!unit,vector0


            subroutine  read_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,intent(in   )::unit
                  complex(KK),intent(inout)::matrix0(1:,&
                                                     1:)

                  integer::i
                  integer::j


                  do j=1,size(matrix0,dim=2),+1

                     do i=1,size(matrix0,dim=1),+1

                         read(unit,format_tensor_K) matrix0(i,j)

              end    do!i=1,size(matrix0,dim=1),+1

                      read(unit,*)

              end do!j=1,size(matrix0,dim=2),+1

                   read(unit,*)


        end subroutine  read_matrix_K!unit,matrix0


            subroutine write_vector_K(unit,vector0)


                  implicit none


                  integer    ,intent(in   )::unit
                  complex(KK),intent(in   )::vector0(1:)

                  integer::i


                  do i=1,size(vector0,dim=1),+1

                     write(unit,format_tensor_K) vector0(i)

              end do!i=1,size(vector0,dim=1),+1

                  write(unit,*)


        end subroutine write_vector_K!unit,vector0


            subroutine write_matrix_K(unit,matrix0)


                  implicit none


                  integer    ,intent(in   )::unit
                  complex(KK),intent(in   )::matrix0(1:,&
                                                     1:)

                  integer::i
                  integer::j


                  do j=1,size(matrix0,dim=2),+1

                     do i=1,size(matrix0,dim=1),+1

                        write(unit,format_tensor_K) matrix0(i,j)

              end    do!i=1,size(matrix0,dim=1),+1

                     write(unit,*)

              end do!j=1,size(matrix0,dim=2),+1

                  write(unit,*)


        end subroutine write_matrix_K!unit,matrix0


  end module tensor_type


#     endif
