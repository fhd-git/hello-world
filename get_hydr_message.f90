program get_hydr_message
implicit none
!
! program get_hydr_message.f90
! get water level and flux of some stations from http://www.gdsw.gov.cn
!
! mean of variables :
! 'sta'  : name of station, chinese
! 'date' : date of the data read from the web site (mm-dd)
! 'time' : as above (HH:MM)
! 'wl'   : water level of each station
! 'flux' : flux of each staion
!
! 'num_of_sta' : number of stations,now it is 14
integer           :: iso,i,start_index,end_index
character(len=500):: string,data_all
logical           :: ifoutput = .false.
character(len=500),external        :: del_null
integer,parameter :: num_of_sta = 14
character(len=10) :: sta,date,time,wl,flux
!
!===============get lines which the data in======
open(11,file='/home/Daofh/Pearl_River_gauge/log',status='old')
open(12,file='/home/Daofh/Pearl_River_gauge/tmp_data/tmp_output.dat',status='replace')
do while(.true.)
    read(11,"(A)",iostat=iso)string
    if(iso /=0 ) then
            exit
    else
            if(string(2:6) == 'tbody')then
                    ifoutput = .true.
            elseif(string(3:7) == 'tbody')then
                    exit
            endif
            if(ifoutput)then
                    write(12,*)adjustL(trim(string))
            endif
    endif
enddo
close(11)
close(12)

!================get data from the lines==========
open(13,file='/home/Daofh/Pearl_River_gauge/tmp_data/tmp_output.dat',status='old')
open(14,file='/home/Daofh/Pearl_River_gauge/all_data_hourly.dat',position='append')
open(15,file='/home/Daofh/Pearl_River_gauge/tmp_data/all_data.dat',status='replace')
read(13,*)
do while(.true.)
    read(13,'(A)',iostat=iso)string
    if( iso /= 0 ) then
        exit
    else
        do while(.true.)
          if(string(1:1) == ' ' ) then
              string = string(2:)
          else
              exit
          endif
        enddo

        do i=1,len_trim(string)
            start_index = index(string,'<')
            end_index = index(string,'>')
            string(start_index:end_index) = ''
        enddo
!    print*,string
!    print*,'del_null = ',del_null(string)
        write(14,*)trim(del_null(string))
        write(15,*)trim(del_null(string))
    endif
enddo
write(14,*)'==========================================='
close(13)
close(14)
close(15)

!===============output data of '高要','石角','博罗'=============
open (17,file='/home/Daofh/Pearl_River_gauge/tmp_data/all_data.dat',status='old')
do while(.true.)
    read(17,*,iostat=iso)sta,date,time,wl,flux
    if(iso /= 0 )then
            exit
    else
        if(adjustL(trim(sta)) == '高要' )then
                open(111,file='/home/Daofh/Pearl_River_gauge/gaoyao.dat',position='append')
                write(111,*)date,time,wl,flux
                close(111)
        elseif(adjustL(trim(sta)) == '石角' )then
                open(112,file='/home/Daofh/Pearl_River_gauge/shijiao.dat',position='append')
                write(112,*)date,time,wl,flux
                close(112)
        elseif(adjustL(trim(sta)) == '博罗' )then
                open(113,file='/home/Daofh/Pearl_River_gauge/boluo.dat',position='append')
                write(113,*)date,time,wl,flux
                close(113)
        endif
    endif
enddo
close(17)
!

end





!=================================================
function del_null(string)
        implicit none
        integer::i,j
        character(len=500) ::string,del_null
        del_null = ''
        do while(.true.)
            if(string(1:1) == '' )then
                 string = string(2:)
            else
                 exit
            endif
        enddo
        j = 1
        do i=1,len(string) 
            if(string(i:i) /= ' ')then
                    del_null(j:j) = string(i:i)
                    j = j + 1
           endif

            if(i < len(string))then
                 if(string(i:i) == ' ' .and. string(i+1 : i+1) /= ' ')then
                       del_null(j:j) = ' '
                       j = j + 1
                 endif
            endif
 

        enddo


end function del_null


