      PROGRAM NEWTONS_METHOD
      implicit none
      integer:: n,i, auto_exit_n
      double precision:: q, a,h, dp, exit_percentage
      logical:: auto_exit, file_output
      character (LEN=5):: input
      
      auto_exit = .FALSE.
      write(*,*)'==Newtons Method=='
      write(*,*)' '
      do while(input/='Q'.and.input/='q')
        write(*,*)'==Main Menu=='    
        read(*,*)input
        if(input=='R'.or.input=='r') then
          write(*,*)' '
          write(*,*)'=Newtons Method='
          do while(input/='')
            write(*,*)'Input exit condition for loop'
            read(*,*)input
            if(input=='P'.or.input=='p') then
            
            write(*,*)'Value to take square root of: '
            read(*,*)a
           write(*,*)'Input percentage difference to exit the loop at: '    ! FORTRAN Y U SO HARD?
            read(*,*)exit_percentage
            q=a
            do
              h=q*q
              h=h-a
              h=h/2
              h=h/q
              q=q-h
              i = i+1
              if(auto_exit.eqv..TRUE.) then
                if(i>=auto_exit_n) then
                exit
                end if
              end if
              if(i>1) then
               dp = q-dp
               dp = dp/q
               dp = dp*100
               if(dp<=exit_percentage.and.dp>=(-(exit_percentage))) then
                if(dp<0) then
                 dp = -dp
                end if
                write(*,*)'Iteration was stopped at ',dp,'% difference'
                exit
               end if
              end if
              !write(*,*)i,q      this was scrapped since the print calls actually slow the program down a lot. C's print calls seem to be much faster than fortrans in relative terms
              dp =q
            end do
            write(*,*)'root(',a, ') at iteration number ',i, ' = ' ,q   
            exit
            end if
            if(input=='N'.or.input=='n') then
            
            write(*,*)'Value to take square root of: '
            read(*,*)a
            write(*,*)'Input number of loops to execute: '
            read(*,*)n
            q=a
            do i=0,n,1
              h=q*q
              h=h-a
              h=h/2
              h=h/q
              q=q-h
              if(auto_exit.eqv..TRUE.) then
                if(i>=auto_exit_n) then
                exit
                end if
              end if
              !write(*,*)i,q      this was scrapped since the print calls actually slow the program down a lot. C's print calls seem to be much faster than fortrans in relative terms
              ! next step is to implement a mode that automatically stops the looping when % differences reach a certain value, or at least a sanity check on the number of iterations to avoid turbo-infinite loops
              ! infinite loops are bad
              !dp =q
            end do
            write(*,*)'root(',a, ') at iteration number ',i, ' = ' ,q            
            exit
            end if
            if(input=='Q'.or.input=='q') then
            exit
            end if
            if(input=='H'.or.input=='h') then
              write(*,*)'Enter P for loop exit based on percentage'
            write(*,*)'Enter N for loop exit based on iteration number'
              write(*,*)'Enter Q to cancel & return to the Main Menu'
            end if
          end do  
        end if
        if(input=='H'.or.input=='h') then
          write(*,*)'Enter R to find a square root using Newtons Method'
          write(*,*)'Enter S to define a maximum number of iterations'
          write(*,*)'Enter Q to Quit'
        end if
        if(input=='S'.or.input=='s') then
          write(*,*)'Input maximum iteration number'
          read(*,*)auto_exit_n
          auto_exit = .TRUE.
        end if
      end do
      stop
      end program NEWTONS_METHOD
