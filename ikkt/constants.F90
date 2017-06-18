#     ifndef IKKT_CONSTANTS_F90
#     define IKKT_CONSTANTS_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"


      module constants


            use::tensor_type


            implicit none


            complex(KK),public,parameter::   zero=( .00000e+0, .00000e+0)
            complex(KK),public,parameter::re_unit=(+.10000e+1, .00000e+0)
            complex(KK),public,parameter::im_unit=( .00000e+0,+.10000e+1)

            complex(KK),dimension(0:1,&
                                  0:1,&
                                  0:3),public,parameter::sigma=[[[+re_unit,    zero],[    zero,+re_unit]],&
                                                                [[    zero,+re_unit],[+re_unit,    zero]],&
                                                                [[    zero,+im_unit],[-im_unit,    zero]],&
                                                                [[+re_unit,    zero],[    zero,-re_unit]]]

            complex(KK),allocatable,dimension( : ,&
                                               : ),public,protected::delta
            complex(KK),allocatable,dimension( : ,&
                                               : ),public,protected::delta_super_traceless
            complex(KK),allocatable,dimension( : ,&
                                               : ,&
                                               : ),public,protected::gamma

            public::make_delta
            public::make_delta_super_traceless
            public::make_gamma


            contains


            subroutine make_delta(size)


                  implicit none


                  integer,intent(in   )::size


                  if(allocated(delta)) then

                     return

              end if!allocated(delta)

                  allocate(delta(0:size-1,&
                                 0:size-1))

                  delta=matrix(size,+re_unit)


        end subroutine make_delta!size


            subroutine make_delta_super_traceless(size)


                  implicit none

                  integer,intent(in   )::size

                  integer::i0,j0
                  integer::i1,j1


                  call make_delta(size)

                  allocate(delta_super_traceless(0:size*size-1,&
                                                 0:size*size-1))

                  do j0=0,size*size-1,+1

                     do j1=0,size*size-1,+1

                        do i0=0,size*size-1,+1

                           do i1=0,size*size-1,+1

                              delta_super_traceless(i0*size+i1,&
                                                    j0*size+j1)=delta(i0,j0)&
                                                               *delta(i1,j1)-delta(i0,i1)&
                                                                            *delta(j0,j1)/real(size,kind=KK)

              end          do!i1=0,size*size-1,+1

              end       do!i0=0,size*size-1,+1

              end    do!j1=0,size*size-1,+1

              end do!j0=0,size*size-1,+1


        end subroutine make_delta_super_traceless!size


            subroutine make_gamma(size)


                  implicit none


                  integer,intent(in   )::size


                  select case(size)

                  case( 4)

                     allocate(gamma(0:1,&
                                    0:1,&
                                    0:3))

                     gamma(:,:,0)=im_unit*(sigma(:,:,1))
                     gamma(:,:,1)=im_unit*(sigma(:,:,2))
                     gamma(:,:,2)=im_unit*(sigma(:,:,3))
                     gamma(:,:,3)=        (sigma(:,:,0))


                  case( 6)

                     allocate(gamma(0:3,&
                                    0:3,&
                                    0:5))

                     gamma(:,:,0)=        (sigma(:,:,1).x.sigma(:,:,2))
                     gamma(:,:,1)=        (sigma(:,:,2).x.sigma(:,:,2))
                     gamma(:,:,2)=        (sigma(:,:,3).x.sigma(:,:,2))
                     gamma(:,:,3)=        (sigma(:,:,0).x.sigma(:,:,1))
                     gamma(:,:,4)=        (sigma(:,:,0).x.sigma(:,:,3))
                     gamma(:,:,5)=im_unit*(sigma(:,:,0).x.sigma(:,:,0))

                  case(10)

                     allocate(gamma(0:16,&
                                    0:16,&
                                    0: 9))

                     gamma(:,:,0)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,2))
                     gamma(:,:,1)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,1))
                     gamma(:,:,2)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,3))
                     gamma(:,:,3)=im_unit*(sigma(:,:,2).x.sigma(:,:,1).x.sigma(:,:,2).x.sigma(:,:,0))
                     gamma(:,:,4)=im_unit*(sigma(:,:,2).x.sigma(:,:,3).x.sigma(:,:,2).x.sigma(:,:,0))
                     gamma(:,:,5)=im_unit*(sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,1).x.sigma(:,:,2))
                     gamma(:,:,6)=im_unit*(sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,3).x.sigma(:,:,2))
                     gamma(:,:,7)=im_unit*(sigma(:,:,1).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))
                     gamma(:,:,8)=im_unit*(sigma(:,:,3).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))
                     gamma(:,:,9)=        (sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))

                  case default

                     stop "Error: unavailable spacetime dimension (4,6,10)"

              end select!case(size)


        end subroutine make_gamma!size


            subroutine print_gamma()


                  implicit none


                  integer::unit
                  integer::mu


                   open(newunit=unit,file="gamma.matrix")

                  call write(unit,delta(:,:))

                  do mu=0,size(sigma,dim=3)-1,+1

                     call write(unit,sigma(:,:,mu))

              end do!mu=0,size(sigma,dim=3)-1,+1

                  do mu=0,size(gamma,dim=3)-1,+1

                     call write(unit,gamma(:,:,mu))

              end do!mu=0,size(gamma,dim=3)-1,+1

                  close(        unit                    )


        end subroutine print_gamma!size


            function determinant_degree(size)


                  implicit none


                  integer::size

                  real(KK)::determinant_degree


                  select case(size)

                  case(4,6)

                     determinant_degree=+.10000e+1

                  case(10)

                     determinant_degree=+.50000e+0

              end select!case(size)


        end function determinant_degree!d


  end module constants


#  endif
