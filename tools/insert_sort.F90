#     ifndef   INSERT_SORT_F90
#     define   INSERT_SORT_F90

#     include "../system/precision.F90"


      module insertion_sort


            implicit none


      contains


            subroutine sort_one(list,n)


                  implicit none


                  real(KK),dimension(:),intent(inout)::list
                  real(KK)                           ::temp

                  integer,intent(in)::n
                  integer           ::i


                  temp=list(n);i=n-1

                  do while((i.ge.0).and.(list(i).gt.temp))

                     list(i+1)=list(i);i=i-1

              end do!while((i.ge.0).and.(list(i).gt.temp))

                  list(i+1)=temp


        end subroutine sort_one!list,n


            subroutine sort(list)


                  implicit none


                  real(KK),dimension(:),intent(inout)::list
                  real(KK)                           ::temp

                  integer::n


                  do n=lbound(list,dim=1),ubound(list,dim=1),+1

                     call sort_one(list,n)

              end do!n=lbound(list,dim=1),ubound(list,dim=1),+1


        end subroutine sort!list


            recursive function sort_sum(list) result(list_sum)


                  implicit none


                  real(KK),dimension(:),intent(inout)::list

                  real(KK)::temp_sum
                  real(KK)::list_sum

                  if(size(list)==1)then

                     list_sum=list(ubound(list,dim=1))

                  else

                     temp_sum=sort_sum(list(lbound(list,dim=1):ubound(list,dim=1)-1))
                     list_sum=temp_sum+(list(ubound(list,dim=1))-temp_sum)/real(size(list),kind=KK)

              end if!size(list)==1


        end           function sort_sum!list


  end module insertion_sort


#     endif
