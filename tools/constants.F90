#     ifndef TENSOR_CONSTANTS_F90
#     define TENSOR_CONSTANTS_F90

#     include "../system/precision.F90"


      module mathematical_constants


            implicit none


            complex(KK),parameter,public::   zero=(+.00000e+0_KK,&
                                                   +.00000e+0_KK)
            complex(KK),parameter,public::re_unit=(+.10000e+1_KK,&
                                                   +.00000e+0_KK)
            complex(KK),parameter,public::im_unit=(+.00000e+0_KK,&
                                                   +.10000e+1_KK)
            complex(KK),parameter,public::re_half=(+.50000e+0_KK,&
                                                   +.00000e+0_KK)
            complex(KK),parameter,public::im_half=(+.00000e+0_KK,&
                                                   +.50000e+0_KK)

            real(KK),parameter,public::pi=acos(-.10000e+1_KK)

            real(KK),parameter,public::nil=+.00000e+0_KK
            real(KK),parameter,public::one=+.10000e+1_KK
            real(KK),parameter,public::two=+.20000e+1_KK


            interface arg

               module procedure intrinsic_arg

        end interface arg


            contains


            function intrinsic_real(z)


                  implicit none


                  complex(KK),intent(in   )::z

                  real(KK)::intrinsic_real


                  intrinsic_real=real(z)


        end function intrinsic_real!z


            function intrinsic_aimag(z)


                  implicit none


                  complex(KK),intent(in   )::z

                  real(KK)::intrinsic_aimag


                  intrinsic_aimag=aimag(z)


        end function intrinsic_aimag!z


            function intrinsic_abs(z)


                  implicit none


                  complex(KK),intent(in   )::z

                  real(KK)::intrinsic_abs


                  intrinsic_abs=abs(z)


        end function intrinsic_abs!z


            function intrinsic_arg(z)


                  implicit none


                  complex(KK),intent(in   )::z

                  real(KK)::intrinsic_arg


                  intrinsic_arg=atan2(intrinsic_real (z),&
                                      intrinsic_aimag(z))


        end function intrinsic_arg!z


  end module mathematical_constants

#  endif
