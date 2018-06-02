#     ifndef   TOOLS_INSERT_SORT_F90
#     define   TOOLS_INSERT_SORT_F90

#     include "../system/precision.F90"


      module insertion_sort


            implicit none


      contains


            recursive subroutine sort(list,n,mask)


                  implicit none


                  complex(KK),dimension(0: ),intent(inout)::list
                  complex(KK)                             ::temp
                  real   (KK)                             ::mask; external mask

                  integer,optional::n
                  integer         ::i


                  if(n>size(list).or..not.present(n)) &
                     n=size(list)

                  temp=list(n);i=n-1

                  if(n>0) then

                     call sort(list,n-1,mask)

                     do while((i.ge.0).and.(mask(list(i)).gt.mask(temp)))

                        list(i+1)=list(i);i=i-1

              end    do!while((i.ge.0).and.(mask(list(i)).gt.mask(temp)))

                  list(i+1)=temp

              end if!n>0

        end           subroutine sort!list,n,mask


            recursive function sort_sum(list) result(list_sum)


                  implicit none


                  real(KK),dimension(:),intent(inout)::list

                  real(KK)::temp_sum
                  real(KK)::list_sum

                  if(size(list)==1)then

                     list_sum=list(ubound(list,dim=1))

                  else

                     temp_sum=sort_sum(list(lbound(list,dim=1):ubound(list,dim=1)-1))
                     list_sum=        (list(ubound(list,dim=1))-temp_sum)/real(size(list),kind=KK)

              end if!size(list)==1


        end           function sort_sum!list


  end module insertion_sort


#     endif
