#     ifndef MATHEMATICAL_CONSTANTS_F90
#     define MATHEMATICAL_CONSTANTS_F90

#     include "../system/precision.F90"


      module mathematical_constants


            implicit none


            complex(KK),parameter,public::   zero=(+.00000e+0_KK,&
                                                   +.00000e+0_KK)
            complex(KK),parameter,public::re_unit=(+.10000e+1_KK,&
                                                   +.00000e+0_KK)
            complex(KK),parameter,public::im_unit=(+.00000e+0_KK,&
                                                   +.10000e+1_KK)

            real(KK),parameter,public::pi    =acos(-.10000e+1_KK)


            contains


            function log2(x) result(y)


               implicit none


               real(KK),intent(in   )::x
               real(KK)              ::y


               y=log10(x)/log10(.20000e+1_KK)


        end function log2!x


  end module mathematical_constants

#  endif
