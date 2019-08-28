	  program Snowfall_totals 

!**********************************************************************************
! Program developed by Julie Winkler, Logan Soldo, and Kara Komoto 
! Latest version June 1, 2018 
! Program was developed for Project Greeen 2017. 
! This is program #4 in this series of programs
! This program is part of a suite of programs to clean up the hourly observations from ASOS stations
! The first program in the suite changes changes splits the datestring into year, month, day, hour and also removes leap year
! The second program in the suite changes the observation hour for ASOS reports to the nearest whole hour (e.g., 0053 to 0100)
! A third program program was written to check the maximum number of duplicate records for a station
! A fourth program in the suite and is intended to remove duplicate files. 
! PHILOSOPHY Do not write a line to the new file until after have checked the two following lines for duplicates
! This is the fifth program in the suite and it simply writes out the missing times so that we can look at whether there are large blocks of missing data

! These are unsophisticated, brute force programs.  


!VARIABLES

	  implicit none 
 
 	  
	  integer i, j, k, l,m,n,p
	  integer Datestring,year,month,MonthDay,day,SF_mean_k(60)
	  integer max_elevation, min_elevation, SF_mean(60),start_year,next_year,previous_year
	  integer median_depth,mean_depth, NOAA_area,julian,counter,SF_median(60),SF_median_k(60)
	  integer station_num,station_10(60),station_25(60),station_50(60),station_100(60)
	  integer Fall_total(60), Winter_total(60), Spring_total(60),Winter_total_k(60)
	  integer Next_fall(60),Next_winter(60),count_day
!	  integer station_num,station_10,station_25,station_50,station_100
	  real  longitude,latitude,yearReal,day_mean(365),day_median(365),mean_sum(365),median_sum(365)
!	  real 
	  ! real GDH(24), GDD(365,24),Daily_GDD(365),total_GDD(80),Base,total_100(80)
	  ! real total_GDH(80)

	  	  
	 character(len=500) :: inputfile,outputfile, outputfile2,outputfile3,outputfile4,outputfile5,outputfile6

	  namelist /cdh_nml/ inputfile,outputfile, outputfile2,outputfile3,outputfile4,outputfile5,outputfile6

open(33,file='cdh.nml',status='old')
read(33,nml=cdh_nml,end=55)
55 close(33)
write(*,nml=cdh_nml)



 

	  
	  open(10, file=inputfile,status="old")
	  open(20, file=outputfile,status="replace")
	  open(25, file=outputfile2,status="replace")			 ! Count Date	  
	  open(30, file=outputfile3,status="replace")
	  open(35, file=outputfile4,status="replace")
	  open(40, file=outputfile5,status="replace")
	  Open(200,file=outputfile6, status = "replace")
!	  open(50,file = outputfile3,status = "replace")
	  

		
	do k = 1,60
		SF_mean(k) = 0
		SF_median(k) = 0
		station_num = 0
		station_10(k) = 0
		station_25(k) = 0
		station_50(k) = 0
		station_100(k) = 0 
		SF_mean_k(k) = 0
		SF_median_k(k) = 0		
		Fall_total(k) = 0
		Winter_total(k) = 0
		Winter_total_k(k) = 0
		Spring_total(k) = 0
		Next_fall(k) = 0
		Next_winter(k) = 0
		do l = 1,365
			day_mean(l) = 0
			day_median(l) = 0
			mean_sum(l) = 0
			median_sum(l) = 0
			enddo
	enddo
! Extract the datestring to yyyy,mm,dd
		Do m=1,20000
			julian = 0

			do p = 1,365

	!			print(200)
				read(10,*,end=15) Datestring,j,i,longitude,latitude,max_elevation,min_elevation,mean_depth,median_depth,&
				&station_num,station_10(k),station_25(k),station_50(k),station_100(k),NOAA_area 
				
				yearReal = Datestring/10000
				year= int(yearReal)
				
				MonthDay=mod(Datestring,10000)
				Month= MonthDay/100
							
				Day= mod(MonthDay,100)
				julian = julian + 1
				! if ((month .EQ. 2) .and. (day .eq. 29))then
					! write(29,1000) Datestring,year,month,day,hour,temp,qtemp,dewpt,qdewpt,slp,qslp,rh
				! else			
				 write(20,1000) Datestring,year,month,day,julian,i,j,longitude,latitude,max_elevation,min_elevation,mean_depth,median_depth,&
				 &station_num,station_10(k),station_25(k),station_50(k),station_100(k),NOAA_area 
				! endif
			enddo
		enddo
	15 close(10)
	   rewind(20)

! This part creates a new variable called 'count_day'. The only purpose of this new variable is to create a new count on the first of July every year.
! This is because July 1st is when the snow season is defined to start in this study. Be aware that count day initialization is dependant on the start day.
! In this case the start day is July 1st or Julian = 182.
		count_day = 184
		Do m=1,50000
			 read(20,1000,end=16) Datestring,year,month,day,julian,i,j,longitude,latitude,max_elevation,min_elevation,&
			 &mean_depth,median_depth,station_num,station_10(k),station_25(k),station_50(k),station_100(k),NOAA_area 
				if (julian .eq. 182) then
					count_day = 0
					do n = 1,60
						do p = 1,365				
							count_day = count_day + 1
							write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
							&mean_depth,median_depth,station_num,station_10(k),station_25(k),station_50(k),station_100(k),NOAA_area 	
							read(20,1000,end=16) Datestring,year,month,day,julian,i,j,longitude,latitude,&
							&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10(k),&
							&station_25(k),station_50(k),station_100(k),NOAA_area 								
						enddo
						count_day=0
					enddo
				else
					count_day = count_day + 1
					write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
					&mean_depth,median_depth,station_num,station_10(k),station_25(k),station_50(k),station_100(k),NOAA_area 	
				endif
		enddo

16 		close(20)
!		rewind(25)







	   
! ! After splitting the date, the program rewinds the split date file and runs the program with julian day.
! ! This part of the program checks if the date is July 1st and then counts 365 times to June 30th of the next year.


		
		! write(30,*) "year","Snowfall Mean","Snowfall Median"             !"Stations Reporting","Stations 10mm","Stations 25mm","Station 50mm","Station 100mm"
		! start_year = 1965	

		! do k = 1,60				
			! previous_year = start_year

			! do l=1,365
				! read(20,1000,end=25)Datestring,year,month,day,julian,i,j,longitude,latitude,&
				! &max_elevation,min_elevation,mean_depth,median_depth,station_num(k),station_10(k),&
				! &station_25(k),station_50(k),station_100(k),NOAA_area
				

				! if (julian .LE. 181) then
					! if (mean_depth .NE. -99999) then
						! SF_mean(k) = SF_mean(k) + mean_depth
					! endif
					! if (median_depth .NE. -99999) then
						! SF_median(k) = SF_median(k) + median_depth
					! endif
! !					write(200,*) year,start_year, month,day,k,l,SF_mean(k)
					
				! elseif (julian .GE. 182) then
					! ! SF_mean(k) = 0
					! ! SF_median(k) = 0
					! start_year = year
					! if (mean_depth .NE. -99999) then					
! !						SF_mean_k(k+1) = SF_mean_k(k+1) + mean_depth	
						! SF_mean_k(k) = SF_mean_k(k) + mean_depth						
						
					! endif
					! if (median_depth .NE. -99999) then
						! SF_median_k(k+1) = SF_median_k(k+1) + median_depth
						! SF_median(k+1) = SF_median(k+1) + median_depth

					! endif
				! endif
				! write(200,*) year,start_year, month,day,k,l,mean_depth,SF_mean(k),median_depth,SF_median(k)

			! enddo

			 ! SF_mean(k+1) = SF_mean_k(k)
			 ! SF_median(k+1) = SF_median_k(k)
			
	! !	if (previous_year .GT. 1965) then
			! write(30,*) previous_year, SF_mean(k), SF_median(k)													! Write to file if the year is greater than 1965
! !		endif
		
			! ! SF_mean_k(k) = 0
			! ! SF_median_k(k) = 0
		

		! enddo
		
		
! 25	   rewind(20)

! ! Taking sum of average snowfall on a given day for every year.

		! do k = 1,60
			! do l = 1,365
				! read(20,1000,end=26)Datestring,year,month,day,julian,i,j,longitude,latitude,&
					! &max_elevation,min_elevation,mean_depth,median_depth,station_num(k),station_10(k),&
					! &station_25(k),station_50(k),station_100(k),NOAA_area
			
				! if (julian .EQ. l) then
					! if (mean_depth .NE. -99999) then
						! mean_sum(l) = mean_sum(l) + mean_depth
						! median_sum(l) = median_sum(l) + median_depth
			! !			write(35,*) k,l,year,month,day,julian,mean_depth,mean_sum(l)
					! endif
				! endif
			! enddo
		! enddo
		

! 26		write(35,2100) "Day of Year", "Sum of Mean", "Mean of Day","Sum of Median","Median of Day"
		! do l = 1,365
			! day_mean(l) = mean_sum(l)/k
			! day_median(l) = median_sum(l)/k
			! write(35,2000) l, mean_sum(l), day_mean(l), median_sum(l),day_median(l)
		! enddo				
 		! rewind(20)
		
		
! ! Seasonal totals, in same K loop, starts here.
		
		! start_year = 1965	
		! do k = 1,60
			! previous_year = start_year
			
			! Fall_total(k)=0
			! Winter_total(k) = 0

			! Fall_total(k) = Next_fall(k-1)
			! Winter_total(k) = Next_winter(k-1)
			
			! Spring_total(k) = 0
			! Winter_total_k(k) = 0

				! do l=1,365
					! read(20,1000,end=20)Datestring,year,month,day,julian,i,j,longitude,latitude,&
					! &max_elevation,min_elevation,mean_depth,median_depth,station_num(k),station_10(k),&
					! &station_25(k),station_50(k),station_100(k),NOAA_area
				
					
					! if ((julian .GE. 182) .and. (julian .LE. 304)) then    ! FALL
! !						start_year = year
						! if (mean_depth .NE. -99999) then
						
							! Next_fall(k) = Next_fall(k) + mean_depth						
						! endif
! !						write(200,*) year,start_year, month,day,julian,k,l,mean_depth,Fall_total(k),Winter_total(k),Spring_total(k)
					! elseif ((julian .GE. 60) .and. (julian .LE. 181)) then   ! SPRING
						! if (mean_depth .NE. -99999) then
							! Spring_total(k) = Spring_total(k) + mean_depth
						! endif	
! !					elseif ((julian .LE. 59).and.(julian .GE. 305)) then    
					! elseif (julian .GE. 305) then							! Winter
						! if (mean_depth .NE. -99999) then					
							! Next_winter(k) = Next_winter(k) + mean_depth						
						! endif				
! !						write(200,*) year,start_year, month,day,julian,k,l,mean_depth,Fall_total(k),Winter_total(k),Spring_total(k)
					! elseif (julian .LE. 59) then
						! if (mean_depth .NE. -99999) then					
							! Winter_total_k(k) = Winter_total_k(k) + mean_depth								
						! endif				
! !						write(200,*) year,start_year, month,day,julian,k,l,mean_depth,Fall_total(k),Winter_total(k),Spring_total(k)						
					! ! else
						! ! write(200,*) year,start_year, month,day,julian,k,l,mean_depth,Fall_total(k),Winter_total(k),Spring_total(k)

					! endif
! !					write(200,*) year,start_year, month,day,julian,k,l,mean_depth,Next_fall(k),&
! !					&Fall_total(k),Winter_total(k),Winter_total_k(k),Spring_total(k)
	
				! enddo
				
				! Winter_total(k) = Winter_total_k(k) + Winter_total(k)

! !				Fall_total(k+1) = Fall_total(k)				
! !				Winter_total(k+1) = Winter_total(k)
				! write(40,*) start_year, Fall_total(k), Winter_total(k), Spring_total(k)



		
				! start_year = year
			! enddo

			
			
			
!!!!!!!Notes!!!!!!!!!
!Winter_total_k and spring are OK
!Fall and Winter_total need to be written for k+1

 1000 format(I8,I5,I3,I3,I8,2(I6),2(f10.3),10(I8))
 1500 format(I8,I5,I3,I3,I8,I8,2(I6),2(f10.3),10(I8))
 2000 format(I11,2(f15.3),2(f20.3))
 2100 format(a11,2(a15)2(a20))
! 2000 format(I5,2f10.2)
! 2500 format(a5,2a10)
! 3000 format(I5,2I8)

20 close(20)			

end



























			! do l=1,365
				! read(20,1000,end=25)Datestring,year,month,day,julian,i,j,longitude,latitude,&
				! &max_elevation,min_elevation,mean_depth,median_depth,station_num(k),station_10(k),&
				! &station_25(k),station_50(k),station_100(k),NOAA_area
				

				! if (julian .LE. 181) then
					! if (mean_depth .NE. -99999) then
						! SF_mean(k) = SF_mean(k) + mean_depth
					! endif
					! if (median_depth .NE. -99999) then
						! SF_median(k) = SF_median(k) + median_depth
					! endif
! !					write(200,*) year,start_year, month,day,k,l,SF_mean(k)
					
				! elseif (julian .GE. 182) then
					! start_year = year
					! if (mean_depth .NE. -99999) then					
						! SF_mean_k(k) = SF_mean_k(k) + mean_depth						
					! endif
					! if (median_depth .NE. -99999) then
						! SF_median_k(k) = SF_median_k(k) + median_depth
					! endif
				! endif
				! write(200,*) year,start_year, month,day,k,l,mean_depth,SF_mean(k),median_depth,SF_median(k)

			! enddo

			! SF_mean(k+1) = SF_mean_k(k)
			! SF_median(k+1) = SF_median_k(k)
		! if (previous_year .GT. 1965) then
			! write(30,*) previous_year, SF_mean(k), SF_median(k)													! Write to file if the year is greater than 1965
		! endif			
			! SF_mean_k(k) = 0
			! SF_median_k(k) = 0
	  