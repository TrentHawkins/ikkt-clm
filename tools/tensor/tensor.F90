#     ifndef TENSOR_TENSOR_F90
#     define TENSOR_TENSOR_F90

#     include "../../system/precision.F90"

#     include "../../tools/constants.F90"


      module tensor_type


            use::mathematical_constants


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

            interface polar

               module procedure scalar_polar_K
               module procedure vector_polar_K
               module procedure matrix_polar_K

        end interface polar

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


            function scalar_polar_K(this) result(that)


                  implicit none


                  complex(KK),intent(in   )::this
                  complex(KK)              ::that


                  that=cmplx(abs(this),arg(this))


        end function scalar_polar_K!this


            function vector_polar_K(this) result(that)


                  implicit none


                  complex(KK),dimension(0:                  ),intent(in   )::this
                  complex(KK),dimension(0:size(this,dim=1)-1)              ::that

                  integer::index


                  do index=0,size(this,dim=1)-1,+1

                     that(index)=polar(this(index))

              end do!index=0,size(this,dim=1)-1,+1


        end function vector_polar_K!this


            function matrix_polar_K(this) result(that)


                  implicit none


                  complex(KK),dimension(0:                  ,&
                                        0:                  ),intent(in   )::this
                  complex(KK),dimension(0:size(this,dim=1)-1,&
                                        0:size(this,dim=1)-2)              ::that

                  integer::index


                  do index=0,size(this,dim=1)-1,+1

                        that(:,index)=polar(this(:,index))

              end do!index=0,size(this,dim=1)-1,+1


        end function matrix_polar_K!this


            subroutine vector_random_number_K(this)


                  implicit none


                  complex(KK),dimension(0:                  ),intent(inout)::this

                  real(KK),dimension(0:size(this,dim=1)-1):: real_this
                  real(KK),dimension(0:size(this,dim=1)-1)::aimag_this


                  call random_number( real_this)
                  call random_number(aimag_this)


                  this=cmplx( real_this,&
                             aimag_this,kind=KK)


        end subroutine vector_random_number_K!this


            subroutine matrix_random_number_K(this)


                  implicit none


                  complex(KK),dimension(0:                  ,&
                                        0:                  ),intent(inout)::this

                  real(KK),dimension(0:size(this,dim=1)-1,&
                                     0:size(this,dim=2)-1):: real_this
                  real(KK),dimension(0:size(this,dim=1)-1,&
                                     0:size(this,dim=2)-1)::aimag_this


                  call random_number( real_this)
                  call random_number(aimag_this)


                  this=cmplx( real_this,&
                             aimag_this,kind=KK)


        end subroutine matrix_random_number_K!this


            subroutine  read_vector_K(unit,this,iostat)


                  implicit none


                  integer                                    ,intent(inout)         ::unit
                  complex(KK),dimension(0:                  ),intent(inout)         ::this
                  integer                                    ,intent(  out),optional::iostat

                  integer::i


                  if(present(iostat)) then

                     do i=0,size(this,dim=1)-1,+1

                         read(unit,format_tensor_K,iostat=iostat,end=110) this(i)

              end    do!i=0,size(this,dim=1)-1,+1

                      read(unit,*,iostat=iostat,end=110)

              110    return

                  else

                     do i=0,size(this,dim=1)-1,+1

                         read(unit,format_tensor_K) this(i)

              end    do!i=0,size(this,dim=1)-1,+1

                      read(unit,*)

              end if!present(iostat)


        end subroutine  read_vector_K!unit,this,iostat


            subroutine  read_matrix_K(unit,this,iostat)


                  implicit none


                  integer                                    ,intent(inout)         ::unit
                  complex(KK),dimension(0:                  ,&
                                        0:                  ),intent(inout)         ::this
                  integer                                    ,intent(  out),optional::iostat

                  integer::i,j


                  if(present(iostat)) then

                     do j=0,size(this,dim=2)-1,+1

                        do i=0,size(this,dim=1)-1,+1

                            read(unit,format_tensor_K,iostat=iostat,end=112) this(i,j)

              end       do!i=0,size(this,dim=1)-1,+1

                         read(unit,*,iostat=iostat,end=112)

              end    do!j=0,size(this,dim=2)-1,+1

                      read(unit,*,iostat=iostat,end=112)

              112    return

                  else

                     do j=0,size(this,dim=2)-1,+1

                        do i=0,size(this,dim=1)-1,+1

                            read(unit,format_tensor_K) this(i,j)

              end       do!i=0,size(this,dim=1)-1,+1

                         read(unit,*)

              end    do!j=0,size(this,dim=2)-1,+1

                      read(unit,*)

              end if!present(iostat)


        end subroutine  read_matrix_K!unit,this,iostat


            subroutine write_vector_K(unit,this,iostat)


                  implicit none


                  integer                                    ,intent(inout)         ::unit
                  complex(KK),dimension(0:                  ),intent(in   )         ::this
                  integer                                    ,intent(  out),optional::iostat

                  integer::i


                  if(present(iostat)) then

                     do i=0,size(this,dim=1)-1,+1

                        write(unit,format_tensor_K,iostat=iostat,err=111) this(i)

              end    do!i=0,size(this,dim=1)-1,+1

                     write(unit,*,iostat=iostat,err=111)

              111    return

                  else

                     do i=0,size(this,dim=1)-1,+1

                        write(unit,format_tensor_K) this(i)

              end    do!i=0,size(this,dim=1)-1,+1

                     write(unit,*)

              end if!present(iostat)


        end subroutine write_vector_K!unit,this,iostat


            subroutine write_matrix_K(unit,this,iostat)


                  implicit none


                  integer                                    ,intent(inout)         ::unit
                  complex(KK),dimension(0:                  ,&
                                        0:                  ),intent(in   )         ::this
                  integer                                    ,intent(  out),optional::iostat

                  integer::i
                  integer::j


                  if(present(iostat)) then

                     do j=0,size(this,dim=2)-1,+1

                        do i=0,size(this,dim=1)-1,+1

                           write(unit,format_tensor_K,iostat=iostat,err=113) this(i,j)

              end       do!i=0,size(this,dim=1)-1,+1

                        write(unit,*,iostat=iostat,err=113)

              end    do!j=0,size(this,dim=2)-1,+1

                     write(unit,*,iostat=iostat,err=113)

              113    return

                  else

                     do j=0,size(this,dim=2)-1,+1

                        do i=0,size(this,dim=1)-1,+1

                           write(unit,format_tensor_K) this(i,j)

              end       do!i=0,size(this,dim=1)-1,+1

                        write(unit,*)

              end    do!j=0,size(this,dim=2)-1,+1

                     write(unit,*)

              end if!present(iostat)


        end subroutine write_matrix_K!unit,this,iostat


  end module tensor_type


#     include "../../tools/tensor/tensor.operations.F90"
#     include "../../tools/tensor/tensor.procedures.F90"

#     endif
