#     ifndef IKKT_CONSTANTS_F90
#     define IKKT_CONSTANTS_F90

#     include "precision.F90"

#     include "tensor/tensor.F90"


      module constants


         use tensor_type


            implicit none


            complex(KK),parameter::   zero=( 00.00, 00.00)
            complex(KK),parameter::re_unit=(+01.00, 00.00)
            complex(KK),parameter::im_unit=( 00.00,+01.00)

            complex(KK),parameter::sigma(1:2,&
                                         1:2,&
                                         0:3)=[[[+re_unit,    zero],[    zero,+re_unit]],&
                                               [[    zero,+re_unit],[+re_unit,    zero]],&
                                               [[    zero,+im_unit],[-im_unit,    zero]],&
                                               [[+re_unit,    zero],[    zero,-re_unit]]]

            complex(KK),parameter::delta(1:2,&
                                         1:2)=sigma(:,:,0)

            complex(KK),allocatable,protected::gamma(:,:,:)


            contains


            subroutine make_gamma(d)


                  implicit none


                  integer::d


                  if(allocated(gamma)) then

                     deallocate (gamma)

              end if!allocated(gamma)

                  select case(d)

                  case( 4)

                     allocate(gamma(1: 2,&
                                    1: 2,&
                                    1: 4))

                     gamma(:,:, 1)=im_unit*(sigma(:,:,1))
                     gamma(:,:, 2)=im_unit*(sigma(:,:,2))
                     gamma(:,:, 3)=im_unit*(sigma(:,:,3))
                     gamma(:,:, 4)=        (sigma(:,:,0))


                  case( 6)

                     allocate(gamma(1: 4,&
                                    1: 4,&
                                    1: 6))

                     gamma(:,:, 1)=        (sigma(:,:,1).x.sigma(:,:,2))
                     gamma(:,:, 2)=        (sigma(:,:,2).x.sigma(:,:,2))
                     gamma(:,:, 3)=        (sigma(:,:,3).x.sigma(:,:,2))
                     gamma(:,:, 4)=        (sigma(:,:,0).x.sigma(:,:,1))
                     gamma(:,:, 5)=        (sigma(:,:,0).x.sigma(:,:,3))
                     gamma(:,:, 6)=im_unit*(sigma(:,:,0).x.sigma(:,:,0))

                  case(10)

                     allocate(gamma(1:16,&
                                    1:16,&
                                    1:10))

                     gamma(:,:, 1)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,2))
                     gamma(:,:, 2)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,1))
                     gamma(:,:, 3)=im_unit*(sigma(:,:,2).x.sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,3))
                     gamma(:,:, 4)=im_unit*(sigma(:,:,2).x.sigma(:,:,1).x.sigma(:,:,2).x.sigma(:,:,0))
                     gamma(:,:, 5)=im_unit*(sigma(:,:,2).x.sigma(:,:,3).x.sigma(:,:,2).x.sigma(:,:,0))
                     gamma(:,:, 6)=im_unit*(sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,1).x.sigma(:,:,2))
                     gamma(:,:, 7)=im_unit*(sigma(:,:,2).x.sigma(:,:,0).x.sigma(:,:,3).x.sigma(:,:,2))
                     gamma(:,:, 8)=im_unit*(sigma(:,:,1).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))
                     gamma(:,:, 9)=im_unit*(sigma(:,:,3).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))
                     gamma(:,:,10)=        (sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0).x.sigma(:,:,0))

              end select!case(d)


        end subroutine make_gamma!d


  end module constants


#  endif
