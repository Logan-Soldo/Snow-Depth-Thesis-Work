	  program SnowDepth_totals 

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
 
 	  
	  integer i, j, k, l,m,p,n
	  integer Datestring,year,month,MonthDay,day,Depth_mean_k(52),Depth_mean_sum(52)
	  integer snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian
	  integer max_elevation, min_elevation, Depth_mean(52),start_year,next_year,previous_year
	  integer median_depth,mean_depth, NOAA_area,julian,counter,Depth_median(52),Depth_median_k(52)
	  integer snwf_NOAA_area,snwf_mean,snwf_median,previous_snwd,snwf_max_elevation
	  integer snwf_min_elevation,zero_depth,First7day(52),ZeroReporting(52)
	  integer station_num,station_10,station_25,station_50,station_100
	  integer snwf_station_num,snwf_station_10,snwf_station_25,snwf_station_50,snwf_station_100
	  integer Fall_total(52), Winter_total(52), Spring_total(52),Winter_total_k(52)
	  integer Next_fall(52),Next_winter(52),study_year,count_day, max_mean(52),min_mean(52)
	  integer max_count(52), min_count(52),max_reporting(52),total_max_reporting
	  integer MaxDaysAbove76(52), DaysAbove76,AvgFst7day,AvgLst7day,LastDay(52),Last7day(52)
	  integer TotalZeroReportYears,Reporting25(52),TotalReporting25,abs_max_elevation(52)
	  integer AbsAverageElevation,abs_min_elevation(52), AvgElevation(52)
!	  integer station_num,station_10,station_25,station_50,station_100
	  real  longitude,latitude,yearReal,day_mean(365),day_median(365),mean_sum(365),median_sum(365)
	  real  zero_percent(52),zero_counter(52),counter_76(52),percent_76(52),miss_counter(52),miss_percent(52)
	  real  miss_total, total_mpercent,total_counter,annCounter_76,annPercent_76
	  real  Jan_total,Feb_total,Mar_total,Apr_total,May_total,Jun_total,Jul_total,Aug_total,Sep_total,Oct_total
	  real 	Nov_total,Dec_total,flag(52),sumflag
	  real  Jan_Mean,Feb_Mean,Mar_Mean,Apr_Mean,May_Mean,Jun_Mean,Jul_Mean,Aug_Mean,Sep_Mean,Oct_Mean
	  real 	Nov_Mean,Dec_Mean,AvgMaxCount,SumMaxCount
	  ! real GDH(24), GDD(365,24),Daily_GDD(365),total_GDD(80),Base,total_100(80)
	  ! real total_GDH(80)

	  	  
	 character(len=500) :: inputfile,inputfile2,outputfile, outputfile2,outputfile3,outputfile4,outputfile5,&
	 & outputfile6,outputfile7,outputfile8,outputfile9,outputfile10

	  namelist /cdh_nml/ inputfile,inputfile2,outputfile,outputfile2,outputfile3,outputfile4,outputfile5,&
	 & outputfile6,outputfile7,outputfile8,outputfile9,outputfile10
open(33,file='cdh.nml',status='old')
read(33,nml=cdh_nml,end=55)
55 close(33)
write(*,nml=cdh_nml)



 

	  
	  open(10, file=inputfile,status="old")					 ! Snow depth
	  open(15, file=inputfile2,status="old")				 ! Snowfall 
	  open(20, file=outputfile,status="replace")             ! Split Date
	  open(25, file=outputfile2,status="replace")			 ! Count Date
	  open(30, file=outputfile3,status="replace")            ! MeanSnowDepth
	  open(34, file=outputfile4,status="replace")            ! 76SnowDepth
	  open(35, file=outputfile5,status="replace")			 ! SnowDepth Quality
	  open(36, file=outputfile6,status="replace")			 ! Percent missing
	  open(40, file=outputfile7,status="replace")			 ! Daily Snow Depth
	  open(50, file=outputfile8,status="replace")			 ! Monthly Average
	  open(60, file=outputfile9,status="replace")	  		 ! Seasonal Snow Depth
	  Open(200,file=outputfile10, status = "replace")		 ! Junk
!	  open(50,file = outputfile3,status = "replace")
	  

		
	do k = 1,52
		Depth_mean(k) = 0
		Depth_mean_sum(k) = 0
		Depth_median(k) = 0
		Depth_mean_k(k) = 0
		Depth_median_k(k) = 0		
		
		
		Fall_total(k) = 0
		Winter_total(k) = 0
		Winter_total_k(k) = 0
		Spring_total(k) = 0
		Next_fall(k) = 0
		Next_winter(k) = 0
	    max_mean(k) = 0
	    min_mean(k) = 9999
		
		zero_counter(k) = 0.0
		zero_percent(k) = 0.0
		counter_76(k) = 0.0
		percent_76(k) = 0.0
		miss_counter(k) = 0.0
		miss_percent(k) = 0.0
		
		MaxDaysAbove76(k) = 0
		First7day(k) = 0
		LastDay(k) = 0
		Last7day(k) = 0
		
		abs_max_elevation(k) = 0
		abs_min_elevation(k) = 0
		AvgElevation(k) = 0
		
	    max_count(k) = 0
	    min_count(k) = 0
		
		ZeroReporting(k) = 0
		Reporting25(k) = 0
		
			do l = 1,365
				day_mean(l) = 0
				day_median(l) = 0
				mean_sum(l) = 0
				median_sum(l) = 0
			enddo
	enddo
! Extract the datestring to yyyy,mm,dd
! Adding Julian day counter.
		Do m=1,20000
			julian = 0

			do p = 1,365

	!			print(200)
				read(10,*,end=15) Datestring,j,i,longitude,latitude,max_elevation,min_elevation,mean_depth,median_depth,&
				&station_num,station_10,station_25,station_50,station_100,NOAA_area 
				
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
				 &station_num,station_10,station_25,station_50,station_100,NOAA_area 
				! endif
			enddo
		enddo
	15 close(10)
	   rewind(20)
	   
	   
! This part creates a new variable called 'count_day'. The only purpose of this new variable is to create a new count on the first of July every year.
! This is because July 1st is when the snow season is defined to start in this study. Be aware that count day initialization is dependant on the start day.
! In this case the start day is July 1st or Julian = 182.
		count_day = 184
		zero_depth = 0
		Do m=1,50000
			 read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,max_elevation,min_elevation,&
			 &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area 
			 read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,count_day,i,j,longitude,&
			 &latitude,snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,&
			 &snwf_station_25,snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data
			 previous_snwd = mean_depth	
				if (julian .eq. 182) then
					count_day = 0
					do n = 1,52
						do p = 1,365				
							count_day = count_day + 1
							if ((previous_snwd .NE. -99999) .AND. (snwf_mean .EQ. 0) .AND. (mean_depth .GT. previous_snwd)) then
								mean_depth = previous_snwd
								write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
								&mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area
							elseif ((julian .GE. 182) .AND. (julian .LE. 243)) then
								write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
								&zero_depth,zero_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area								
							else
								write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
								&mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area 						
							endif
								previous_snwd = mean_depth				! mean_depth is reset as mean depth
								read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
								&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
								&station_25,station_50,station_100,NOAA_area
								 read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,count_day,i,j,longitude,latitude,&
								 &snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
								 &snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data									
						enddo
						count_day=0
					enddo
				else
					count_day = count_day + 1
					write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
					&mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area 	
				endif
		enddo

! After adding count day, this part of the program uses July 1st as the first day of the counter.
! The program then counts 365 days to June 30th.

17 		close(20)
		close(15)
		rewind(25)

		
		write(30,3100) "year","Max Elevation","SnowDepth Mean","Mean Max","Max Day","Mean Min","Min Count","Max Reporting"    !"Stations Reporting","Stations 10mm","Stations 25mm","Station 50mm","Station 100mm"
		write(34,3410) "year","76Count","76Percent","MaxAbove76","First7day","LastDay","Last7Days","flag"
		write(35,3510) "year","Zero Count","Zero Percent","7.6 Count","7.6 Percent","Miss Count","Miss Percent"
		! write(36,3610) "i","j","lat","lon","Miss Percent","Miss Count","Percent_76","Counter_76",&
		! &"AvgMaxJulian","total_counter","max_reporting"     !percent missing is wrtitten out here. 
		start_year = 1965				
!			previous_year = start_year
		total_mpercent = 0   ! These two variables do a running total of all the missing data so that it can be concatenated with all other files and mapped.
		miss_total = 0
		total_counter = 0
		annCounter_76 = 0
		annPercent_76 = 0
		total_max_reporting = 0
		sumflag = 0
! Initialize months
		Jan_total = 0
		Feb_total = 0
		Mar_total = 0
		Apr_total = 0
		May_total = 0
		Jun_total = 0
		Jul_total = 0
		Aug_total = 0
		Sep_total = 0
		Oct_total = 0
		Nov_total = 0
		Dec_total = 0
		
		Jan_Mean = 0
		Feb_Mean = 0
		Mar_Mean = 0
		Apr_Mean = 0
		May_Mean = 0
		Jun_Mean = 0
		Jul_Mean = 0
		Aug_Mean = 0
		Sep_Mean = 0
		Oct_Mean = 0
		Nov_Mean = 0
		Dec_Mean = 0				
		
		do n = 1,5000	
			read(25,1500,end=25)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&
			&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
			&station_25,station_50,station_100,NOAA_area
				
				if (count_day .eq. 1) then
					study_year = start_year + 1
					do k = 1,52	
					   max_mean(k) = 0
					   min_mean(k) = 9999   ! if there is no snow depth reported over 0, then this value will be reported.
					   max_reporting(k) = 0
					   DaysAbove76 = 0
					   flag(k) = 0.0
					   
					   
						do l=1,365
							total_counter = total_counter + 1
							write(200,*) total_counter,year,month,day,julian,count_day
							if (mean_depth .NE. -99999) then
								Depth_mean(k) = Depth_mean(k) + mean_depth
								if (mean_depth .gt. max_mean(k)) then
									max_mean(k) = mean_depth
									max_count(k) = count_day
									
								elseif((mean_depth .gt. 0) .and.(mean_depth .lt. min_mean(k))) then
									min_mean(k) = mean_depth
									min_count(k) = count_day
								endif

! Monthly average will calculated within the loop							
								if ((julian .GE. 1) .AND. (julian .LE. 31)) then			! January
									Jan_total = Jan_total + mean_depth
								elseif ((julian .GE. 32) .AND. (julian .LE. 59)) then       ! February
									Feb_total = Feb_total + mean_depth
								elseif ((julian .GE. 60) .AND. (julian .LE. 90)) then		! March
									Mar_total = Mar_total + mean_depth
								elseif ((julian .GE. 91) .AND. (julian .LE. 120)) then		! April
									Apr_total = Apr_total + mean_depth
								elseif ((julian .GE. 121) .AND. (julian .LE. 151)) then		! May
									May_total = May_total + mean_depth
								elseif ((julian .GE. 152) .AND. (julian .LE. 181)) then		! June
									Jun_total = Jun_total + mean_depth
								elseif ((julian .GE. 182) .AND. (julian .LE. 212)) then		! July
									Jul_total = Jul_total + mean_depth
								elseif ((julian .GE. 213) .AND. (julian .LE. 243)) then		! August
									Aug_total = Aug_total + mean_depth
								elseif ((julian .GE. 244) .AND. (julian .LE. 273)) then		! September
									Sep_total = Sep_total + mean_depth
								elseif ((julian .GE. 274) .AND. (julian .LE. 304)) then		! October
									Oct_total = Oct_total + mean_depth
								elseif ((julian .GE. 305) .AND. (julian .LE. 334)) then		! November
									Nov_total = Nov_total + mean_depth									
								elseif ((julian .GE. 335) .AND. (julian .LE. 365)) then		! December
									Dec_total = Dec_total + mean_depth	
								
								endif
								
						! Checking for mean_depth values greater than 0.
								if (mean_depth .gt. 0) then
									zero_counter(k) = zero_counter(k) + 1
								endif
								if (mean_depth .ge. 76) then        ! Checking if depth is greater than 76mm. (data is in mm)
									annCounter_76 = annCounter_76 + 1
									counter_76(k) = counter_76(k) + 1
									DaysAbove76 = DaysAbove76 + 1				! Tracking the number of days above 7.6 cm.
									if (DaysAbove76 .EQ. 7) then
										flag(k) = 1
										First7day(k) = count_day					! Write day that first 7 days above 7.6 is reached.
									endif
									LastDay(k) = count_day
								else
									if (DaysAbove76 .GE. 7) then
										if (DaysAbove76 .GT. MaxDaysAbove76(k)) then			! if the number of days above 7.6 is greater than current max, then the maximum is replaced.	
											MaxDaysAbove76(k) = DaysAbove76
										endif
										Last7day(k) = count_day - 1     ! This records the last 7 day period where snow is above 7.6 cm.
									endif
									DaysAbove76 = 0								! Days above is reset
								endif
							
							
							elseif (mean_depth .EQ. -99999) then
								miss_counter(k) = miss_counter(k) + 1	
								miss_total = miss_total + 1
							endif
							
							if (station_num .gt. max_reporting(k)) then
								max_reporting(k) = station_num		! trying to find the maximum number of stations reporting.
							endif
							if (station_num .gt. total_max_reporting) then
								total_max_reporting = station_num
							endif
							if (median_depth .NE. -99999) then
								Depth_median(k) = Depth_median(k) + median_depth
							endif
							
							if (max_elevation .ne. -99999) then
								if (max_elevation .gt. abs_max_elevation(k)) then
									abs_max_elevation(k) = max_elevation
								endif
							endif							
							if (min_elevation .ne. -99999) then
								if (min_elevation .lt. abs_min_elevation(k)) then
									abs_min_elevation(k) = min_elevation
								endif
							endif
														
	!						write(200,*) year,study_year, month,day,julian,count_day,k,l,mean_depth,Depth_mean(k),Depth_mean_k(k), Depth_mean_sum(k)    
	
	
	!	Calculating standard deviation here.
							
							read(25,1500,end=25)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&    ! Check if this read is correct...
							&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
							&station_25,station_50,station_100,NOAA_area	
						enddo
						
						if (max_reporting(k) .EQ. 0) then
							ZeroReporting(k) = 1
						endif
						
						if (max_reporting(k) .lt. (0.25*total_max_reporting)) then
							Reporting25(k) = 1
						endif
						
						AvgElevation(k) = (abs_max_elevation(k) - abs_min_elevation(k))/2  ! This gives an average for each year of the absolute max and min elevation.
						
						zero_percent(k) = (zero_counter(k)/365.0)*100    ! percent of year covered by snow.
						percent_76(k) = (counter_76(k)/365.0)*100
						miss_percent(k) = (miss_counter(k)/365.0)*100							
						
						write(30,3000) study_year,AvgElevation(k),Depth_mean(k), max_mean(k),&
						&max_count(k),min_mean(k),min_count(k),max_reporting(k)  ! This file will only write on calculated statistics. i.e. sum, mean, max, min, standard deviation.	
						write(34,3400) study_year,counter_76(k),percent_76(k), MaxDaysAbove76(k),First7day(k),LastDay(k),Last7day(k),flag(k)
!						write(34,*) DaysAbove76
						write(35,3500) study_year,zero_counter(k),zero_percent(k),counter_76(k),percent_76(k),miss_counter(k),miss_percent(k)    ! This file will be to check the quality of the data. The zero_counter could be important for number of days with snow on the ground.

						study_year = study_year + 1

					enddo					
				endif		
!			write(200,*) year,study_year, month,day,julian,count_day,k,l,mean_depth,Depth_mean(k),Depth_mean_k(k), Depth_mean_sum(k)			
		enddo

25	   				rewind(25)					
					total_mpercent = (miss_total/total_counter)*100         ! percent of missing data in the whole dataset is calculated here.			
					annPercent_76 = (annCounter_76/total_counter)*100

! Converting count days into Julian Day.

	TotalZeroReportYears = 0
	AbsAverageElevation = 0

		AvgMaxCount = (sum(max_count,k))/(k-1)
		! if (AvgMaxCount .GE. 186) then
			! AvgMaxCount = AvgMaxCount - 185
		! elseif (AvgMaxCount .LE. 185) then
			! AvgMaxCount = AvgMaxCount + 180
		! endif
		
		TotalZeroReportYears = sum(ZeroReporting,k)
		TotalReporting25 = sum(Reporting25,k)    ! Number of years when less than 25% of maximum stations in total period are reporting. 
		
		sumflag = sum(flag,k)
		if (sumflag .GE. 3) then						! taking an average of less than 3 will not be benficial to the project.
			AvgFst7day = (sum(First7day,k))/(sumflag)
			! if (AvgFst7day .GE. 186) then
				! AvgFst7day = AvgFst7day - 185
			! elseif (AvgFst7day .LE. 185) then
				! AvgFst7day = AvgFst7day + 180
			! endif
			AvgLst7day = (sum(Last7day,k))/(sumflag)
			! if (AvgLst7day .GE. 186) then
				! AvgLst7day = AvgLst7day - 185
			! elseif (AvgLst7day .LE. 185) then
				! AvgLst7day = AvgLst7day + 180
			! endif						
		else 
			AvgFst7day = -99999
			AvgLst7day = -99999
		endif
		
		AbsAverageElevation = (sum(AvgElevation,k))/k
		
		write(36,3600) i,j,latitude,longitude,AbsAverageElevation,total_mpercent,miss_total,annPercent_76,annCounter_76,&
		&AvgMaxCount,AvgFst7day,AvgLst7day,sumflag,total_counter,total_max_reporting,TotalZeroReportYears,&
		& TotalReporting25   !	percent missing is wrtitten out here. 

! Write out the monthly averages here.
		Jan_Mean = Jan_total/k
		Feb_Mean = Feb_total/k
		Mar_Mean = Mar_total/k
		Apr_Mean = Apr_total/k
		May_Mean = May_total/k
		Jun_Mean = Jun_total/k
		Jul_Mean = Jul_total/k
		Aug_Mean = Aug_total/k
		Sep_Mean = Sep_total/k
		Oct_Mean = Oct_total/k
		Nov_Mean = Nov_total/k
		Dec_Mean = Dec_total/k
					
write(50,5100) "Month","k","Mean","Total"
write(50,5000) "January",k,Jan_Mean,Jan_total	
write(50,5000) "February",k,Feb_Mean,Feb_total
write(50,5000) "March",k,Mar_Mean,Mar_total
write(50,5000) "April",k,Apr_Mean,Apr_total
write(50,5000) "May",k,May_Mean,May_total
write(50,5000) "June",k,Jun_Mean,Jun_total
write(50,5000) "July",k,Jul_Mean,Jul_total
write(50,5000) "August",k,Aug_Mean,Aug_total
write(50,5000) "September",k,Sep_Mean,Sep_total
write(50,5000) "October",k,Oct_Mean,Oct_total
write(50,5000) "November",k,Nov_Mean,Nov_total
write(50,5000) "December",k,Dec_Mean,Dec_total	
		


! Taking sum of average snowfall on a given day for every year.

		do k = 1,52
			do l = 1,365
				read(25,1500,end=26)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&
					&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
					&station_25,station_50,station_100,NOAA_area
			
				if (julian .EQ. l) then
					if (mean_depth .NE. -99999) then
						mean_sum(l) = mean_sum(l) + mean_depth
						median_sum(l) = median_sum(l) + median_depth
					endif
				endif
			enddo
		enddo
		

26		write(40,4100) "Day of Year", "Sum of Mean", "Mean of Day","Sum of Median","Median of Day"
		do l = 1,365
			day_mean(l) = mean_sum(l)/k
			day_median(l) = median_sum(l)/k
			write(40,4000) l, mean_sum(l), day_mean(l), median_sum(l),day_median(l)
		enddo				
 		rewind(25)
		



			
			
			
!!!!!!!Notes!!!!!!!!!
!Winter_total_k and spring are OK
!Fall and Winter_total need to be written for k+1

 1000 format(I8,I5,I3,I3,I8,2(I6),2(f10.3),10(I8))
 1500 format(I8,I5,I3,I3,I8,I8,2(I6),2(f10.3),10(I8))
 3000 format(I8,7(I16))
 3100 format(a8,7(a16))
 3410 format(a8,7(a12))
 3400 format(I8,2(f12.2),4(I12),f12.0)
 3510 format(a8,6(a16))
 3500 format(I8,6(f16.2))
 3600 format(2(I5),2(f14.6)I6,5(f14.3),2(I14),2(f14.1),3(I6))
 3610 format(2(a5),2(a14),5(a14),a14,a14)
 4000 format(I11,2(f15.3),2(f20.3))
 4100 format(a11,2(a15)2(a20))
 5000 format(a12,I6,2(f14.3))
 5100 format(a12,a6,2(a14))


20 close(20)			

end



! Monthly Averages will start here.
		! do n = 1,5000	
			! read(25,1500,end=45)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&
			! &max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10(k),&
			! &station_25(k),station_50(k),station_100(k),NOAA_area
				
				! if (count_day .eq. 1) then
					! study_year = start_year + 1
					! do k = 1,60	
						! do l = 1,365

				



							! endif

						! read(25,1500,end=25)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&    ! Check if this read is correct...
						! &max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10(k),&
						! &station_25(k),station_50(k),station_100(k),NOAA_area

						! enddo
					! enddo
				! endif
		! enddo
	





		
! Seasonal totals, in same K loop, starts here.
!!!! This is on hold until seasons have been defined for the study.!!!!
		
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
					! &max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10(k),&
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









