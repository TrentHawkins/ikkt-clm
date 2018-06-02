      program test
            implicit none
            integer,dimension(0:9)::x=[0,3,6,9,2,5,8,1,4,7]
            integer,dimension(0:9)::y=[0,7,4,1,8,5,2,9,6,3]
            integer,dimension(0:9)::z
            integer               ::i,add
                                    z(2)=add(x(2),y(2))
            do i=0,9,+1
               print "(3(x,i2))",x(i),y(i),z(i)
        end do!i=0,9,+1
  end program test
      function add(x,y) result(z)
            implicit none
            integer,intent(in)::x
            integer,intent(in)::y
            integer           ::z
            z=x+y
  end function add!x,y
