#     ifndef CONSTANTS_F90
#     define CONSTANTS_F90

#     include "../system/precision.F90"
#     include "../main/mathematical_constants.F90"

#     include "../tensor/tensor.F90"


      module constants


            use::mathematical_constants

            use::tensor_type


            implicit none


            complex(KK),dimension(0:1,&
                                  0:1,&
                                  0:3),parameter,public::sigma=[[[+re_unit,    zero],[    zero,+re_unit]],&
                                                                [[    zero,+re_unit],[+re_unit,    zero]],&
                                                                [[    zero,+im_unit],[-im_unit,    zero]],&
                                                                [[+re_unit,    zero],[    zero,-re_unit]]]

            complex(KK),allocatable,dimension( : ,&
                                               : ),public,protected::delta
            complex(KK),allocatable,dimension( : ,&
                                               : ,&
                                               : ),public,protected::gamma,&
                                                           conjugate_gamma
            complex(KK),allocatable,dimension( : ,&
                                               : ,&
                                               : ,&
                                               : ),public,protected::gamma_core

            private::gamma_size

            private::make_delta
            private::make_gamma
            private::make_gamma_core

            public::determinant_degree

            public:: make_constants
            public::eject_constants


            contains


            function gamma_size(size)


                  implicit none


                  integer,intent(in   )::size

                  integer::gamma_size


                  gamma_size=2**(size/2-1)


        end function gamma_size!size


            subroutine make_delta(size)


                  implicit none


                  integer,intent(in   )::size


                  if(allocated(delta)) return

                     allocate (delta(0:size-1,&
                                     0:size-1))

                  delta=matrix(size,+re_unit)


        end subroutine make_delta!size


            subroutine make_gamma(size)


                  implicit none


                  integer,intent(in   )::size


                  allocate(gamma(0:gamma_size(size)-1,&
                                 0:gamma_size(size)-1,&
                                 0:           size -1))

                  select case(size)

                  case( 4)

                     gamma(:,:,0)=im_unit*(sigma(:,:,1))
                     gamma(:,:,1)=im_unit*(sigma(:,:,2))
                     gamma(:,:,2)=im_unit*(sigma(:,:,3))
                     gamma(:,:,3)=        (sigma(:,:,0))


                  case( 6)

                     gamma(:,:,0)=        (sigma(:,:,1).x.sigma(:,:,2))
                     gamma(:,:,1)=        (sigma(:,:,2).x.sigma(:,:,2))
                     gamma(:,:,2)=        (sigma(:,:,3).x.sigma(:,:,2))
                     gamma(:,:,3)=        (sigma(:,:,0).x.sigma(:,:,1))
                     gamma(:,:,4)=        (sigma(:,:,0).x.sigma(:,:,3))
                     gamma(:,:,5)=im_unit*(sigma(:,:,0).x.sigma(:,:,0))

                  case(10)

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


            subroutine make_gamma_core(size)


                  implicit none


                  integer,intent(in   )::size

                  integer::mu,nu


                  allocate(conjugate_gamma(0:gamma_size(size)-1,&
                                           0:gamma_size(size)-1,&
                                           0:           size -1))

                  allocate(gamma_core(0:gamma_size(size)-1,&
                                      0:gamma_size(size)-1,&
                                      0:           size -1,&
                                      0:           size -1))

                  do mu=0,size-1,+1

                     conjugate_gamma(:,:,mu)&
                    =conjugate(gamma(:,:,mu))

                     do nu=0,size-1,+1

                        gamma_core(:,:,mu,nu)=conjugate_gamma(:,:,mu).o.gamma(:,:,nu)

              end    do!nu=0,size-1,+1

              end do!mu=0,size-1,+1


        end subroutine make_gamma_core!size


            function determinant_degree(size)


                  implicit none


                  integer,intent(in   )::size

                  real(KK)::determinant_degree


                  select case(size)

                  case(4,6)

                     determinant_degree=+.10000e+1_KK

                  case(10)

                     determinant_degree=+.50000e+0_KK

              end select!case(size)


        end function determinant_degree!d


            subroutine make_constants(inner_size,&
                                      boson_size)


                  implicit none


                  integer,intent(in   )::inner_size
                  integer,intent(in   )::boson_size


                  call make_delta     (inner_size)
                  call make_gamma     (boson_size)
                  call make_gamma_core(boson_size)

                  call print_gamma()


        end subroutine make_constants!inner_size,
!                                     boson_size


            subroutine eject_constants


                  implicit none


                  if(allocated(          delta     )) deallocate(          delta     )
                  if(allocated(          gamma     )) deallocate(          gamma     )
                  if(allocated(conjugate_gamma     )) deallocate(conjugate_gamma     )
                  if(allocated(          gamma_core)) deallocate(          gamma_core)


        end subroutine eject_constants


            subroutine print_gamma()


                  implicit none


                  integer::unit

                  integer::mu,nu


                   open(newunit=unit,file="gamma.matrix")

                  call write(unit,delta(:,:))

                  do mu=0,size(sigma,dim=3)-1,+1

                     call write(unit,sigma(:,:,mu))

              end do!mu=0,size(sigma,dim=3)-1,+1

                  do mu=0,size(gamma,dim=3)-1,+1

                     call write(unit,gamma(:,:,mu))

              end do!mu=0,size(gamma,dim=3)-1,+1

                  do mu=0,size(gamma_core,dim=3)-1,+1

                     do nu=0,size(gamma_core,dim=3)-1,+1

                        write(unit,*) norm(conjugate(gamma_core(:,:,mu,nu))&
                                                    -gamma_core(:,:,mu,nu))

              end    do!nu=0,size(gamma_core,dim=3)-1,+1

              end do!mu=0,size(gamma_core,dim=3)-1,+1

                  close(        unit                    )


        end subroutine print_gamma!


  end module constants


#  endif
