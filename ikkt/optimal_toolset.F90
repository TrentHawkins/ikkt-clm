#     ifndef IKKT_OPTIMAL_TOOLSET_F90
#     define IKKT_OPTIMAL_TOOLSET_F90

#     include "main/precision.F90"

#     include "tensor/tensor.F90"

#     include "ikkt/fields.F90"


      module optimal_toolset


            use::fields

            use::tensor_type


            implicit none


            complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                  0:inner_degrees_of_freedom  ,&
                                  0:fermi_degrees_of_freedom-1,&
                                  0:boson_degrees_of_freedom-1,0:1,&
                                                               0:1)::av_va
            public::vv,mv,cmv,um,ucm,vcmmv,cmmv,ucmm,umav


            interface operator( .m.)

               module procedure  mv
               module procedure um
               module procedure umv

        end interface operator( .m.)

            interface operator( .cm.)

               module procedure  cmv
               module procedure ucm
               module procedure ucmv

        end interface operator( .cm.)

            interface operator( .cmm.)

               module procedure  cmmv
               module procedure ucmm
               module procedure ucmmv

        end interface operator( .cmm.)


      contains


            function vv(v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  real(KK)::vv

                  integer::a_


                  vv=+.00000e+0_KK

                  do a_=0,fermi_degrees_of_freedom-1,+1

                     vv&
                    =vv+norm(v(:,:,a_))

              end do!a_=0,fermi_degrees_of_freedom-1,+1


        end function vv!v


            function mv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1)::mv

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::av_va

                  integer::a1,a2,m_


                  mv=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        av_va(:,:)=transpose(      a(:,:,m_) .commutation.      v(:,:,a2) )

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           mv(:,:,a1)&
                          =mv(:,:,a1)+           gamma(a1,a2,m_)*(av_va(:,:)&
                                                                - av_va(n,n)*delta(:,:))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function mv!a,v


            function cmv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1)::cmv

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::av_va

                  integer::a1,a2,m_


                  cmv=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a2=0,fermi_degrees_of_freedom-1,+1

                        av_va(:,:)=transpose(conjg(a(:,:,m_)).commutation.      v(:,:,a2) )

                        do a1=0,fermi_degrees_of_freedom-1,+1

                           cmv(:,:,a1)&
                          =cmv(:,:,a1)-conjugate_gamma(a1,a2,m_)*(av_va(:,:)&
                                                                - av_va(n,n)*delta(:,:))

              end       do!a1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function cmv!a,v


            function um(u,a)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1)::um

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::au_ua

                  integer::a1,a2,m_


                  um=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a1=0,fermi_degrees_of_freedom-1,+1

                        au_ua(:,:)=transpose(      a(:,:,m_) .commutation.conjg(v(:,:,a1)))

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           um(:,:,a2)&
                          =um(:,:,a2)-           gamma(a1,a2,m_)*(au_ua(:,:)&
                                                                - au_ua(n,n)*delta(:,:))

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!a1=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function um!u,a


            function ucm(u,a)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1)::ucm

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::au_ua

                  integer::a1,a2,m_


                  ucm=zero

                  do m_=0,boson_degrees_of_freedom-1,+1

                     do a1=0,fermi_degrees_of_freedom-1,+1

                        au_ua(:,:)=conjugate(      a(:,:,m_) .commutation.      v(:,:,a1) )

                        do a2=0,fermi_degrees_of_freedom-1,+1

                           ucm(:,:,a2)&
                          =ucm(:,:,a2)+conjugate_gamma(a1,a2,m_)*(au_ua(:,:)&
                                                                - au_ua(n,n)*delta(:,:))

              end       do!a2=0,fermi_degrees_of_freedom-1,+1

              end    do!a1=0,fermi_degrees_of_freedom-1,+1

              end do!m_=0,boson_degrees_of_freedom-1,+1


        end function ucm!u,a


            function vcmmv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  real(KK)::vcmmv

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::av_va

                  integer::a1,a2,m1,m2


                  vcmmv=+.00000e+0_KK

                  do m2=0,boson_degrees_of_freedom-1,+1

                     do a2=0,boson_degrees_of_freedom-1,+1

                        av_va(:,:)=transpose(conjg(a(:,:,m2)).commutation.      v(:,:,a2) )

                        do m1=0,fermi_degrees_of_freedom-1,+1

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              vcmmv&
                             =vcmmv+gamma_core(a1,a2,m1,m2)*(norm(av_va(:,:))&
                                                           + norm(av_va(n,n))*(inner_degrees_of_freedom+1))

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!m1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,boson_degrees_of_freedom-1,+1

              end do!m2=0,boson_degrees_of_freedom-1,+1


        end function vcmmv!a,v


            function cmmv(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1)::cmmv

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::av_va,aav_ava_ava_vaa

                  integer::a1,a2,m1,m2


                  cmmv=zero

                  do m2=0,boson_degrees_of_freedom-1,+1

                     do a2=0,boson_degrees_of_freedom-1,+1

                        av_va(:,:)=transpose(a(:,:,m2).commutation.v(:,:,a2)))

                        do m1=0,fermi_degrees_of_freedom-1,+1

                           aav_ava_ava_vaa(:,:)=transpose(conjg(a(:,:,m1)).commutation.av_va(:,:))

                           do a1=0,fermi_degrees_of_freedom-1,+1

                              cmmv(:,:,a1)&
                             =cmmv(:,:,a1)-gamma_core(a1,a2,m1,m2)*aav_ava_ava_vaa(:,:)

              end          do!a1=0,fermi_degrees_of_freedom-1,+1

              end       do!m1=0,fermi_degrees_of_freedom-1,+1

              end    do!a2=0,boson_degrees_of_freedom-1,+1

              end do!m2=0,boson_degrees_of_freedom-1,+1


        end function cmmv!a,v


            function ucmm(a,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1),intent(in   )::a
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::v
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1)::ucmm

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::av_va,aav_ava_ava_vaa

                  integer::a1,a2,m1,m2


                  ucmm=zero

                  do m1=0,boson_degrees_of_freedom-1,+1

                     do a1=0,boson_degrees_of_freedom-1,+1

                        av_va(:,:)=conjugate(a(:,:,m1).commutation.v(:,:,a1)))

                        do m2=0,fermi_degrees_of_freedom-1,+1

                           aav_ava_ava_vaa(:,:)=transpose(      a(:,:,m2) .commutation.av_va(:,:))

                           do a2=0,fermi_degrees_of_freedom-1,+1

                              ucmm(:,:,a2)&
                             =ucmm(:,:,a2)+gamma_core(a1,a2,m1,m2)*aav_ava_ava_vaa(:,:)

              end          do!a2=0,fermi_degrees_of_freedom-1,+1

              end       do!m2=0,fermi_degrees_of_freedom-1,+1

              end    do!a1=0,boson_degrees_of_freedom-1,+1

              end do!m1=0,boson_degrees_of_freedom-1,+1


        end function ucmm!a,v


            function umav(u,v)


                  implicit none


                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:fermi_degrees_of_freedom-1),intent(in   )::u,v
                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  ,&
                                        0:boson_degrees_of_freedom-1)::umav

                  complex(KK),dimension(0:inner_degrees_of_freedom  ,&
                                        0:inner_degrees_of_freedom  )::uv_vu

                  integer::a1,a2,m_


                  umav=zero

                  do a2=0,boson_degrees_of_freedom-1,+1

                     do a1=0,fermi_degrees_of_freedom-1,+1

                        uv_vu(:,:)=conjg(u(:,:,a1)).commutation.v(:,:,a2)

                        do m_=0,fermi_degrees_of_freedom-1,+1

                           umav(:,:,m_)&
                          =umav(:,:,m_)-gamma(a1,a2,m_)*uv_vu(:,:)

              end       do!m_=0,fermi_degrees_of_freedom-1,+1

              end    do!a1=0,fermi_degrees_of_freedom-1,+1

              end do!a2=0,boson_degrees_of_freedom-1,+1


        end function umav!u,v


  end module optimal_toolset


#  endif
