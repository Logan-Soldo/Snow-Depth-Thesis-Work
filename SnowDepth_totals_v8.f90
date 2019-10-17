	  program SnowDepth_totals 

!**********************************************************************************
! Program developed by Logan Soldo 
! Latest version September 25th, 2019
! Program was developed for Snow Depth Thesis Project. 
! This is an unsophisticated, brute force program.  


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
	  integer AbsAverageElevation,abs_min_elevation(52), AvgElevation(52),melt_length(52)
	  integer melt_days,miss_depth,two_previous_snwd,snf_count_day,abs_max,max_year
	  integer max_mean_year(52),last_recorded,FstLstDiff(52),AvgFstLstDiff,sum_range
	  integer flag2
!	  integer station_num,station_10,station_25,station_50,station_100
	  real  longitude,latitude,yearReal,day_mean(365),day_median(365),mean_sum(365),median_sum(365)
	  real  zero_percent(52),gtzero_counter(52),counter_76(52),percent_76(52),miss_counter(52),miss_percent(52)
	  real  miss_total, total_mpercent,total_counter,annCounter_76,annPercent_76
	  real  Jan_total(52),Feb_total(52),Mar_total(52),Apr_total(52),May_total(52),Jun_total(52)
	  real  Jul_total(52),Aug_total(52),Sep_total(52),Oct_total(52)
	  real 	Nov_total(52),Dec_total(52),flag(52),sumflag
	  real  Jan_Mean,Feb_Mean,Mar_Mean,Apr_Mean,May_Mean,Jun_Mean,Jul_Mean,Aug_Mean,Sep_Mean,Oct_Mean
	  real 	Nov_Mean,Dec_Mean,AvgMaxCount,SumMaxCount
	  real  per5,per10,per15,per20,per25,tot_snwcover,countGT0(52)
	  real count25(52),count50(52),count100(52),count125(52),count150(52),range_count(52)
	  ! real GDH(24), GDD(365,24),Daily_GDD(365),total_GDD(80),Base,total_100(80)
	  ! real total_GDH(80)

	  	  
	 character(len=500) :: inputfile,inputfile2,outputfile3,outputfile4,outputfile5,&
	 & outputfile6,outputfile7,outputfile8,outputfile9,outputfile10,outputfile11

	  namelist /cdh_nml/ inputfile,inputfile2,outputfile3,outputfile4,outputfile5,&
	 & outputfile6,outputfile7,outputfile8,outputfile9,outputfile10,outputfile11
open(33,file='cdh.nml',status='old')
read(33,nml=cdh_nml,end=55)
55 close(33)
write(*,nml=cdh_nml)



 

	  
	  ! open(10, file=inputfile,status="old")					 ! Snow depth
	  open(15, file=inputfile2,status="old")				 ! Snowfall 
	  ! open(20, file=outputfile,status="replace")             ! Split Date
	  open(25, file=inputfile,status="old")			 		 ! Count Date
	  open(30, file=outputfile3,status="replace")            ! MeanSnowDepth
	  open(34, file=outputfile4,status="replace")            ! 76SnowDepth
	  open(35, file=outputfile5,status="replace")			 ! SnowDepth Quality
	  open(36, file=outputfile6,status="replace")			 ! Percent missing
	  open(37, file=outputfile7,status="replace")			 ! Percent depth
	  open(40, file=outputfile8,status="replace")			 ! Daily Snow Depth
	  open(50, file=outputfile9,status="replace")			 ! Monthly Average
	  open(60, file=outputfile10,status="replace")	  		 ! Seasonal Snow Depth
	  Open(200,file=outputfile11, status = "replace")		 ! Junk
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
		max_mean_year(k) = 0
	    min_mean(k) = 9999
		
		gtzero_counter(k) = 0.0
		zero_percent(k) = 0.0
		counter_76(k) = 0.0
		percent_76(k) = 0.0
		miss_counter(k) = 0.0
		miss_percent(k) = 0.0
		
		MaxDaysAbove76(k) = 0
		First7day(k) = 0
		LastDay(k) = 0
		Last7day(k) = 0
		FstLstDiff(k) = 0
		
		abs_max_elevation(k) = 0
		abs_min_elevation(k) = 0
		AvgElevation(k) = 0
		
	    max_count(k) = 0
	    min_count(k) = 0
		
		ZeroReporting(k) = 0
		Reporting25(k) = 0
		
		melt_length(k) = 0
		
		Jan_total(k) = 0
		Feb_total(k) = 0
		Mar_total(k) = 0
		Apr_total(k) = 0
		May_total(k) = 0
		Jun_total(k) = 0
		Jul_total(k) = 0
		Aug_total(k) = 0
		Sep_total(k) = 0
		Oct_total(k) = 0
		Nov_total(k) = 0
		Dec_total(k) = 0
		
		countGT0(k) = 0
		count25(k) = 0
		count50(k) = 0
		count100(k) = 0
		count125(k) = 0 
		count150(k) = 0
			do l = 1,365
				day_mean(l) = 0
				day_median(l) = 0
				mean_sum(l) = 0
				median_sum(l) = 0
			enddo
	enddo

		
		write(30,3100) "year","Max Elevation","SnowDepth Mean","Mean Max","Max Day","Melt Length","Max Reporting"    !"Stations Reporting","Stations 10mm","Stations 25mm","Station 50mm","Station 100mm"
		write(34,3410) "year","76Count","76Percent","MaxAbove76","First7day","Last7Days","DaysAbove76","flag"
		write(35,3510) "year","Zero Count","Zero Percent","7.6 Count","7.6 Percent","Miss Count","Miss Percent"
		write(50,5100) "year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"
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
		abs_max = 0
		max_year = 0
		AvgFstLstDiff = 0
		
		per5 = 0
		per10 = 0
		per15 = 0
		per20 = 0
		per25 = 0
! Initialize months

		
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
					   flag2 = 0
					   melt_days = 0
					   
						if ((study_year .eq.1967) .or. (study_year .eq. 1971) .or. (study_year .eq.1975) .or. &
						 & (study_year .eq. 1979) .or. (study_year .eq. 1983) .or. (study_year .eq.1987) .or. &
						 & (study_year .eq. 1991) .or. (study_year .eq. 1995) .or. (study_year .eq.1999) .or. &
						 & (study_year .eq. 2003) .or. (study_year .eq. 2007) .or. (study_year .eq.2011) .or. &
						 & (study_year .eq. 2015))then
							do l=1,366
							!	write(200,*) "in leap"
								total_counter = total_counter + 1
	!!!							write(200,*) total_counter,year,month,day,julian,count_day
								if (mean_depth .NE. -99999) then
									Depth_mean(k) = Depth_mean(k) + mean_depth
									if (mean_depth .gt. max_mean(k)) then
										max_mean(k) = mean_depth
										max_mean_year(k) = study_year
										max_count(k) = count_day
										melt_days = 0					! If a new maximum is found, melt days is reset to 0.
									elseif((mean_depth .gt. 0) .and.(mean_depth .lt. min_mean(k))) then
										min_mean(k) = mean_depth
										min_count(k) = count_day
									endif
									
									if (mean_depth .ne. 0) then				! This finds the amount of time it takes for 0 depth to be reached after the max melt.
										melt_days = melt_days + 1
									else
										melt_length(k) = melt_days
									endif

	! Monthly average will calculated within the loop							
									if ((julian .GE. 1) .AND. (julian .LE. 31)) then			! January
										Jan_total(k) = Jan_total(k) + mean_depth
									elseif ((julian .GE. 32) .AND. (julian .LE. 59)) then       ! February
										Feb_total(k) = Feb_total(k) + mean_depth
									elseif ((julian .GE. 60) .AND. (julian .LE. 90)) then		! March
										Mar_total(k) = Mar_total(k) + mean_depth
									elseif ((julian .GE. 91) .AND. (julian .LE. 120)) then		! April
										Apr_total(k) = Apr_total(k) + mean_depth
									elseif ((julian .GE. 121) .AND. (julian .LE. 151)) then		! May
										May_total(k) = May_total(k) + mean_depth
									elseif ((julian .GE. 152) .AND. (julian .LE. 181)) then		! June
										Jun_total(k) = Jun_total(k) + mean_depth
									elseif ((julian .GE. 182) .AND. (julian .LE. 212)) then		! July
										Jul_total(k) = Jul_total(k) + mean_depth
									elseif ((julian .GE. 213) .AND. (julian .LE. 243)) then		! August
										Aug_total(k) = Aug_total(k) + mean_depth
									elseif ((julian .GE. 244) .AND. (julian .LE. 273)) then		! September
										Sep_total(k) = Sep_total(k) + mean_depth
									elseif ((julian .GE. 274) .AND. (julian .LE. 304)) then		! October
										Oct_total(k) = Oct_total(k) + mean_depth
									elseif ((julian .GE. 305) .AND. (julian .LE. 334)) then		! November
										Nov_total(k) = Nov_total(k) + mean_depth									
									elseif ((julian .GE. 335) .AND. (julian .LE. 365)) then		! December
										Dec_total(k) = Dec_total(k) + mean_depth	
									
									endif
									
							! Checking for mean_depth values greater than 0.
									if (mean_depth .gt. 0) then
										gtzero_counter(k) = gtzero_counter(k) + 1
									endif
									if ((count_day .GE. 93) .and. (count_day .LE. 304)) then			! Beginning of October to end of April.						
										if (mean_depth .gt. 0) then
											countGT0(k) = countGT0(k) + 1
										endif
										if (mean_depth .ge. 25) then
											count25(k) = count25(k) + 1
										endif
										if (mean_depth .ge. 50) then
											count50(k) = count50(k) + 1
										endif
										if (mean_depth .ge. 100) then
											count100(k) = count100(k) + 1
										endif
										if (mean_depth .ge. 125) then
											count125(k) = count125(k) + 1
										endif
										if (mean_depth .ge. 150) then
											count150(k) = count150(k) + 1
										endif
										range_count(k) = range_count(k) + 1   ! Counting number of days between September and end of May.
									endif			
									
									if (mean_depth .ge. 76) then        ! Checking if depth is greater than 76mm. (data is in mm)
										annCounter_76 = annCounter_76 + 1
										counter_76(k) = counter_76(k) + 1
										DaysAbove76 = DaysAbove76 + 1				! Tracking the number of days above 7.6 cm.
										if ((DaysAbove76 .EQ. 7) .and. (flag2 .eq. 0)) then
											flag(k) = 1
											First7day(k) = count_day					! Write day that first 7 days above 7.6 is reached.
										endif
										LastDay(k) = count_day
									else
										if (DaysAbove76 .GE. 7) then
											if (DaysAbove76 .GT. MaxDaysAbove76(k)) then			! if the number of days above 7.6 is greater than current max, then the maximum is replaced.	
												MaxDaysAbove76(k) = DaysAbove76
											endif
											flag2 = 1
											Last7day(k) = count_day - 1     ! This records the last 7 day period where snow is above 7.6 cm.
										endif
										DaysAbove76 = 0								! Days above is reset
									endif
															
								elseif (mean_depth .EQ. -99999) then
									miss_counter(k) = miss_counter(k) + 1	
									miss_total = miss_total + 1
									melt_length(k) = melt_days
									if (DaysAbove76 .GE. 7) then
										if (DaysAbove76 .GT. MaxDaysAbove76(k)) then			! if the number of days above 7.6 is greater than current max, then the maximum is replaced.	
											MaxDaysAbove76(k) = DaysAbove76
										endif
										Last7day(k) = count_day - 1     ! This records the last 7 day period where snow is above 7.6 cm.
									endif
									!DaysAbove76 = 0								! Days above is reset
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
															
!						write(200,*) year,study_year,month,day,count_day,mean_depth,count25(k),count50(k),count100(k),count125(k),count150(k)    
		
		
								
								read(25,1500,end=25)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&    ! Check if this read is correct...
								&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
								&station_25,station_50,station_100,NOAA_area	
							enddo ! end year loop						
						else
							do l=1,365
								total_counter = total_counter + 1
	!!!							write(200,*) total_counter,year,month,day,julian,count_day
								if (mean_depth .NE. -99999) then
									Depth_mean(k) = Depth_mean(k) + mean_depth
									if (mean_depth .gt. max_mean(k)) then
										max_mean(k) = mean_depth
										max_mean_year(k) = study_year
										max_count(k) = count_day
										melt_days = 0					! If a new maximum is found, melt days is reset to 0.
									elseif((mean_depth .gt. 0) .and.(mean_depth .lt. min_mean(k))) then
										min_mean(k) = mean_depth
										min_count(k) = count_day
									endif
									
									if (mean_depth .ne. 0) then				! This finds the amount of time it takes for 0 depth to be reached after the max melt.
										melt_days = melt_days + 1
									else
										melt_length(k) = melt_days
									endif

	! Monthly average will calculated within the loop							
									if ((julian .GE. 1) .AND. (julian .LE. 31)) then			! January
										Jan_total(k) = Jan_total(k) + mean_depth
									elseif ((julian .GE. 32) .AND. (julian .LE. 59)) then       ! February
										Feb_total(k) = Feb_total(k) + mean_depth
									elseif ((julian .GE. 60) .AND. (julian .LE. 90)) then		! March
										Mar_total(k) = Mar_total(k) + mean_depth
									elseif ((julian .GE. 91) .AND. (julian .LE. 120)) then		! April
										Apr_total(k) = Apr_total(k) + mean_depth
									elseif ((julian .GE. 121) .AND. (julian .LE. 151)) then		! May
										May_total(k) = May_total(k) + mean_depth
									elseif ((julian .GE. 152) .AND. (julian .LE. 181)) then		! June
										Jun_total(k) = Jun_total(k) + mean_depth
									elseif ((julian .GE. 182) .AND. (julian .LE. 212)) then		! July
										Jul_total(k) = Jul_total(k) + mean_depth
									elseif ((julian .GE. 213) .AND. (julian .LE. 243)) then		! August
										Aug_total(k) = Aug_total(k) + mean_depth
									elseif ((julian .GE. 244) .AND. (julian .LE. 273)) then		! September
										Sep_total(k) = Sep_total(k) + mean_depth
									elseif ((julian .GE. 274) .AND. (julian .LE. 304)) then		! October
										Oct_total(k) = Oct_total(k) + mean_depth
									elseif ((julian .GE. 305) .AND. (julian .LE. 334)) then		! November
										Nov_total(k) = Nov_total(k) + mean_depth									
									elseif ((julian .GE. 335) .AND. (julian .LE. 365)) then		! December
										Dec_total(k) = Dec_total(k) + mean_depth	
									
									endif
									
							! Checking for mean_depth values greater than 0.
									if (mean_depth .gt. 0) then
										gtzero_counter(k) = gtzero_counter(k) + 1
									endif
									if ((count_day .GE. 93) .and. (count_day .LE. 304)) then
										if (mean_depth .gt. 0) then
											countGT0(k) = countGT0(k) + 1
										endif									
										if (mean_depth .ge. 25) then
											count25(k) = count25(k) + 1
										endif
										if (mean_depth .ge. 50) then
											count50(k) = count50(k) + 1
										endif
										if (mean_depth .ge. 100) then
											count100(k) = count100(k) + 1
										endif
										if (mean_depth .ge. 125) then
											count125(k) = count125(k) + 1
										endif
										if (mean_depth .ge. 150) then
											count150(k) = count150(k) + 1
										endif
										range_count(k) = range_count(k) + 1   ! Counting number of days between September and end of May.
									endif
									if (mean_depth .ge. 76) then        ! Checking if depth is greater than 76mm. (data is in mm)
										annCounter_76 = annCounter_76 + 1
										counter_76(k) = counter_76(k) + 1
										DaysAbove76 = DaysAbove76 + 1				! Tracking the number of days above 7.6 cm.
										if ((DaysAbove76 .EQ. 7) .and. (flag2 .eq. 0)) then
											flag(k) = 1
											First7day(k) = count_day					! Write day that first 7 days above 7.6 is reached.
										endif
										LastDay(k) = count_day
									else
										if (DaysAbove76 .GE. 7) then
											if (DaysAbove76 .GT. MaxDaysAbove76(k)) then			! if the number of days above 7.6 is greater than current max, then the maximum is replaced.	
												MaxDaysAbove76(k) = DaysAbove76
											endif
											flag2 = 1
											Last7day(k) = count_day - 1     ! This records the last 7 day period where snow is above 7.6 cm.
										endif
										DaysAbove76 = 0								! Days above is reset
									endif
															
								elseif (mean_depth .EQ. -99999) then
									miss_counter(k) = miss_counter(k) + 1	
									miss_total = miss_total + 1
									melt_length(k) = melt_days
									if (DaysAbove76 .GE. 7) then
										if (DaysAbove76 .GT. MaxDaysAbove76(k)) then			! if the number of days above 7.6 is greater than current max, then the maximum is replaced.	
											MaxDaysAbove76(k) = DaysAbove76
										endif
										Last7day(k) = count_day - 1     ! This records the last 7 day period where snow is above 7.6 cm.
									endif
								!	DaysAbove76 = 0								! Days above is reset
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
!							write(200,*) year,study_year,month,day,count_day,mean_depth,count25(k),count50(k),count100(k),count125(k),count150(k)    
															
		
		
		!	Calculating standard deviation here.
								
								read(25,1500,end=25)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&    ! Check if this read is correct...
								&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
								&station_25,station_50,station_100,NOAA_area	
							enddo ! end year loop
						
						endif	
							if (max_reporting(k) .EQ. 0) then
								ZeroReporting(k) = 1
							endif						
							if (max_reporting(k) .lt. (0.25*total_max_reporting)) then
								Reporting25(k) = 1
							endif
							
							if (max_mean(k) .gt. abs_max) then
								abs_max = max_mean(k)
								max_year = max_mean_year(k)
							endif
							FstLstDiff(k) = Last7Day(k) - First7day(k)
							
							AvgElevation(k) = (abs_max_elevation(k) - abs_min_elevation(k))/2  ! This gives an average for each year of the absolute max and min elevation.
							
							zero_percent(k) = (gtzero_counter(k)/365.0)*100    ! percent of year covered by snow.
							percent_76(k) = (counter_76(k)/365.0)*100
							miss_percent(k) = (miss_counter(k)/365.0)*100							
							
							write(30,3000) study_year,AvgElevation(k),Depth_mean(k), max_mean(k),&
							&max_count(k),melt_length(k),max_reporting(k)  ! This file will only write on calculated statistics. i.e. sum, mean, max, min, standard deviation.	
							write(34,3400) study_year,counter_76(k),percent_76(k), MaxDaysAbove76(k),&
							&First7day(k),Last7day(k),FstLstDiff(k),DaysAbove76,flag(k)
							write(35,3500) study_year,gtzero_counter(k),zero_percent(k),counter_76(k),percent_76(k),miss_counter(k),miss_percent(k)    ! This file will be to check the quality of the data. The gtzero_counter could be important for number of days with snow on the ground.
							write(50,5000) study_year,Jan_total(k),Feb_total(k),Mar_total(k),Apr_total(k),May_total(k),Jun_total(k),&
										&Jul_total(k),Aug_total(k),Sep_total(k),Oct_total(k),&
										&Nov_total(k),Dec_total(k)
										
							study_year = study_year + 1

						enddo	
				endif		
!			write(200,*) year,study_year, month,day,julian,count_day,k,l,mean_depth,Depth_mean(k),Depth_mean_k(k), Depth_mean_sum(k)			
		enddo

25	   				rewind(25)	
					total_mpercent = (miss_total/total_counter)*100         ! percent of missing data in the whole dataset is calculated here.			
					annPercent_76 = (annCounter_76/total_counter)*100

! Taking annual averages.
! Converting count days into Julian Day (only if necessary).

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
		if (sumflag .GE. 5.0) then						! taking an average of less than 3 will not be benficial to the project.
			AvgFst7day = (sum(First7day,k))/(sumflag)
			AvgLst7day = (sum(Last7day,k))/(sumflag)
			AvgFstLstDiff = (sum(FstLstDiff,k))/(sumflag)
				
		else 
			AvgFst7day = -99999
			AvgLst7day = -99999
		endif
		
		tot_snwcover = ((sum(countGT0,k))/(sum(range_count,k)))*100
		
		per5 = ((sum(count25,k))/(sum(range_count,k)))*100
		per10 = ((sum(count50,k))/(sum(range_count,k)))*100
		per15 = ((sum(count100,k))/(sum(range_count,k)))*100
		per20 = ((sum(count125,k))/(sum(range_count,k)))*100
		per25 = ((sum(count150,k))/(sum(range_count,k)))*100
		sum_range = sum(range_count,k)
		
		AbsAverageElevation = (sum(AvgElevation,k))/k
!		write(200,*) i,j,latitude,longitude,sum(range_count,k),sum(count150,k)
		
		write(36,3600) i,j,latitude,longitude,AbsAverageElevation,total_mpercent,miss_total,annPercent_76,annCounter_76,&
		&AvgMaxCount,abs_max,max_year,AvgFst7day,AvgLst7day,AvgFstLstDiff,sumflag,total_counter,total_max_reporting,&
		TotalZeroReportYears,TotalReporting25   !	percent missing is wrtitten out here. 
		write(37,*) i,j,latitude,longitude,sum_range,tot_snwcover,per5,per10,per15,per20,per25

! Write out the monthly averages here.
		Jan_Mean = (sum(Jan_total,k))/k
		Feb_Mean = (sum(Feb_total,k))/k
		Mar_Mean = (sum(Mar_total,k))/k
		Apr_Mean = (sum(Apr_total,k))/k
		May_Mean = (sum(May_total,k))/k
		Jun_Mean = (sum(Jun_total,k))/k
		Jul_Mean = (sum(Jul_total,k))/k
		Aug_Mean = (sum(Aug_total,k))/k
		Sep_Mean = (sum(Sep_total,k))/k
		Oct_Mean = (sum(Oct_total,k))/k
		Nov_Mean = (sum(Nov_total,k))/k
		Dec_Mean = (sum(Dec_total,k))/k


write(50,5200) "Mean",Jan_Mean,Feb_Mean,Mar_Mean,Apr_Mean,May_Mean,Jun_Mean,Jul_Mean,&
&Aug_Mean,Sep_Mean,Oct_Mean,Nov_Mean,Dec_Mean				
! write(50,5100) "Month","k","Mean","Total"
! write(50,5000) "January",k,Jan_Mean,Jan_total	
! write(50,5000) "February",k,Feb_Mean,Feb_total
! write(50,5000) "March",k,Mar_Mean,Mar_total
! write(50,5000) "April",k,Apr_Mean,Apr_total
! write(50,5000) "May",k,May_Mean,May_total
! write(50,5000) "June",k,Jun_Mean,Jun_total
! write(50,5000) "July",k,Jul_Mean,Jul_total
! write(50,5000) "August",k,Aug_Mean,Aug_total
! write(50,5000) "September",k,Sep_Mean,Sep_total
! write(50,5000) "October",k,Oct_Mean,Oct_total
! write(50,5000) "November",k,Nov_Mean,Nov_total
! write(50,5000) "December",k,Dec_Mean,Dec_total	
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Taking sum of average snowfall on a given day for every year.

		do k = 1,52
			do l = 1,365
				read(25,1500,end=26)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&
					&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
					&station_25,station_50,station_100,NOAA_area
				if ((month .eq. 2) .and. (day .eq. 29)) then
					read(25,1500,end=26)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&
					&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
					&station_25,station_50,station_100,NOAA_area
				endif
				if (julian .EQ. l) then
					if (mean_depth .NE. -99999) then
						mean_sum(l) = mean_sum(l) + mean_depth
						median_sum(l) = median_sum(l) + median_depth
					endif
				endif
			enddo
		enddo
		

26		write(40,4100) "DayOfYear","MeanOfDay","MedianOfDay"				
		do l = 1,365		! This may need to be redone becaue of leap year addition.
			day_mean(l) = mean_sum(l)/k
			day_median(l) = median_sum(l)/k
			write(40,4000) l, mean_sum(l), day_mean(l), median_sum(l),day_median(l)
		enddo				
 		rewind(25)
		



			
			
			
!!!!!!!Notes!!!!!!!!!
! Added in leap year logic, probably going to make everything explode. Stay Tuned.




 1000 format(I8,I5,I3,I3,I8,2(I6),2(f10.3),10(I8))
 1500 format(I8,I5,I3,I3,I8,I8,2(I6),2(f10.3),10(I8))
 3000 format(I8,6(I16))
 3100 format(a8,6(a16))
 3410 format(a8,7(a12))
 3400 format(I8,2(f12.2),5(I12),f12.0)
 3510 format(a8,6(a16))
 3500 format(I8,6(f16.2))
 3600 format(2(I5),2(f14.6),I6,5(f14.3),5(I14),2(f14.1),3(I6))
 3610 format(2(a5),2(a14),5(a14),a14,a14)
 4000 format(I11,2(f15.3),2(f20.3))
 4100 format(a11,2(a15))
 5000 format(I10,12(f10.2))
 5100 format(13(a10))
 5200 format(a10,12(f10.2))
 !5100 format(a12,a6,2(a14))


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









