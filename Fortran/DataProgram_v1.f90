	  program SnowDepth_Data
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
	  integer max_mean_year(52),last_recorded
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

	  	  
	 character(len=500) :: inputfile,inputfile2,outputfile, outputfile2,outputfile3

	  namelist /cdh_nml/ inputfile,inputfile2,outputfile,outputfile2,outputfile3
open(33,file='cdh.nml',status='old')
read(33,nml=cdh_nml,end=55)
55 close(33)
write(*,nml=cdh_nml)



 

	  
	  open(10, file=inputfile,status="old")					 ! Snow depth
	  open(15, file=inputfile2,status="old")				 ! Snowfall 
	  open(20, file=outputfile,status="replace")             ! Split Date
	  open(25, file=outputfile2,status="replace")			 ! Count Date
	  Open(200,file=outputfile3, status = "replace")		 ! Junk
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
		
		melt_length(k) = 0
		
			do l = 1,365
				day_mean(l) = 0
				day_median(l) = 0
				mean_sum(l) = 0
				median_sum(l) = 0
			enddo
	enddo
! Extract the datestring to yyyy,mm,dd
! Adding Julian day counter.
		julian = 0
		Do m=1,100000
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
				
				if ((Month .eq. 12) .and. (day .eq. 31)) then
					julian = 0
				endif
				! endif
		enddo
	15 close(10)
	   rewind(20)
	   
	   
! This part creates a new variable called 'count_day'. The only purpose of this new variable is to create a new count on the first of July every year.
! This is because July 1st is when the snow season is defined to start in this study. Be aware that count day initialization is dependant on the start day.
! In this case the start day is July 1st or Julian = 182.
		count_day = 184
		snf_count_day = 184
		zero_depth = 0
		last_recorded = 0
		miss_depth = -99999
		study_year = 1965
		Do m=1,50000
			 read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,max_elevation,min_elevation,&
			 &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area 
			 read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,&
			 &latitude,snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,&
			 &snwf_station_25,snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data
			 previous_snwd = mean_depth
			
			 
				if (julian .eq. 182) then
					count_day = 0
					snf_count_day = 184
					do n = 1,52
						study_year = study_year + 1

!						if (ANY((1967,1971,1975,1979,1983,1987,1991,1995,1999,2003,2007,2011,2015) == study_year)) then
						if ((study_year .eq.1967) .or. (study_year .eq. 1971) .or. (study_year .eq.1975) .or. &
						 & (study_year .eq. 1979) .or. (study_year .eq. 1983) .or. (study_year .eq.1987) .or. &
						 & (study_year .eq. 1991) .or. (study_year .eq. 1995) .or. (study_year .eq.1999) .or. &
						 & (study_year .eq. 2003) .or. (study_year .eq. 2007) .or. (study_year .eq.2011) .or. &
						 & (study_year .eq. 2015))then
							do p = 1,366
								write(200,*) "in leap", year,study_year,month,day,count_day
								count_day = count_day + 1
								
								! if (julian .eq. 183) then
									! count_day= 2
								! endif
								if ((julian .GE. 182) .AND. (julian .LE. 243)) then
									! two_previous_snwd = zero_depth
									! previous_snwd = zero_depth
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&zero_depth,zero_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area								
								elseif ((mean_depth .ne. -99999) .and. (previous_snwd .ne. -99999) .AND. &
								&(snwf_mean .EQ. 0) .AND. (mean_depth .GT. previous_snwd)) then				! will try using snwf median instead?	
									 mean_depth = miss_depth
									! previous_snwd = two_previous_snwd
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area
								elseif ((mean_depth .ne. -99999) .and. (previous_snwd .ne. -99999) .AND. &
								&(mean_depth .GT. previous_snwd+750)) then  ! could change this to a lower value... (750mm == 29.5in)
									mean_depth = miss_depth								
								!	last_recorded = mean_depth								
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area							
								elseif ((previous_snwd .eq. -99999) .AND. (snwf_mean .EQ. 0) .AND. (mean_depth .GT. last_recorded)) then
									mean_depth = last_recorded
									
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area							
								elseif (mean_depth .eq. -99999) then
								   write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
								   &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area								
									backspace(20)							
									backspace(20)
									backspace(20)
									backspace(15)
									backspace(15)
									backspace(15)
									backspace(25)
									backspace(25)
								!	backspace(25)							
									!	write(200,*) "in backspace"
									!	write(200,*) year, month, day,julian,count_day,snf_count_day, mean_depth
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day
									!		write(200,*) year, month, day,julian, count_day, snf_count_day,mean_depth																		
									if (mean_depth .eq. -99999) then
											! write(200,*) "in if"
											! write(200,*)year, month, day,julian, count_day,snf_count_day, mean_depth										
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data											
											count_day = snf_count_day										
											 write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,miss_depth,miss_depth,&
											 &miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area											
											! write(200,*) year, month, day,julian,count_day,snf_count_day, mean_depth 
											
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day										
											! write(200,*) year, month, day,julian, count_day,snf_count_day, mean_depth 										
											 write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											 &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area											
											! write(200,*) "end if"
									else ! 
											! write(200,*) "in else"
											! write(200,*) year, month, day, julian, count_day,mean_depth
											  !write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											 ! &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area
											
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day
											
											! write(200,*) year,month,day,julian,count_day, mean_depth
											 write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											 &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area	
											
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day
											
											! write(200,*) year,month,day,julian,count_day, mean_depth
											write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											&mean_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area																									
											! write(200,*) year,month,day,julian,count_day, mean_depth																							
											! write(200,*)"endif"				
										endif
										
								else
									last_recorded = mean_depth

									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area 						
								endif
									
									! write(200,*) year,month,day,julian,count_day,mean_depth
									!write(200,*)year,month,day,julian,count_day, mean_depth,previous_snwd,last_recorded,snwf_mean
							!		two_previous_snwd = previous_snwd
									previous_snwd = mean_depth				! mean_depth is reset as mean depth
									read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
									&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
									&station_25,station_50,station_100,NOAA_area
									 read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
									 &snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
									 &snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data															
									

									!write(200,*)year,month,day,julian,count_day, mean_depth,previous_snwd, last_recorded,snwf_mean

							enddo
! NOT LEAP YEAR
						else
							do p = 1,365
								count_day = count_day + 1
								
								! if (julian .eq. 183) then
									! count_day= 2
								! endif
								if ((julian .GE. 182) .AND. (julian .LE. 243)) then
									! two_previous_snwd = zero_depth
									! previous_snwd = zero_depth
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&zero_depth,zero_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area								
								elseif ((mean_depth .ne. -99999) .and. (previous_snwd .ne. -99999) .AND. &
								&(snwf_mean .EQ. 0) .AND. (mean_depth .GT. previous_snwd)) then				! will try using snwf median instead?	
									 mean_depth = miss_depth
									! previous_snwd = two_previous_snwd
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area
								elseif ((mean_depth .ne. -99999) .and. (previous_snwd .ne. -99999) .AND. &
								&(mean_depth .GT. previous_snwd+750)) then  ! could change this to a lower value... (750mm == 29.5in)
									mean_depth = miss_depth								
								!	last_recorded = mean_depth								
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area							
								elseif ((previous_snwd .eq. -99999) .AND. (snwf_mean .EQ. 0) .AND. (mean_depth .GT. last_recorded)) then
									mean_depth = last_recorded
									
									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area							
								elseif (mean_depth .eq. -99999) then
								   write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
								   &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area								
									backspace(20)							
									backspace(20)
									backspace(20)
									backspace(15)
									backspace(15)
									backspace(15)
									backspace(25)
									backspace(25)
								!	backspace(25)							
									!	write(200,*) "in backspace"
									!	write(200,*) year, month, day,julian,count_day,snf_count_day, mean_depth
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day
									!		write(200,*) year, month, day,julian, count_day, snf_count_day,mean_depth																		
									if (mean_depth .eq. -99999) then
											! write(200,*) "in if"
											! write(200,*)year, month, day,julian, count_day,snf_count_day, mean_depth										
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data											
											count_day = snf_count_day										
											 write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,miss_depth,miss_depth,&
											 &miss_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area											
											! write(200,*) year, month, day,julian,count_day,snf_count_day, mean_depth 
											
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day										
											! write(200,*) year, month, day,julian, count_day,snf_count_day, mean_depth 										
											 write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											 &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area											
											! write(200,*) "end if"
									else ! 
											! write(200,*) "in else"
											! write(200,*) year, month, day, julian, count_day,mean_depth
											  !write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											 ! &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area
											
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day
											
											! write(200,*) year,month,day,julian,count_day, mean_depth
											 write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											 &mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area	
											
											read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
											&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
											&station_25,station_50,station_100,NOAA_area		
											read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
											&snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
											&snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data		
											count_day = snf_count_day
											
											! write(200,*) year,month,day,julian,count_day, mean_depth
											write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
											&mean_depth,miss_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area																									
											! write(200,*) year,month,day,julian,count_day, mean_depth																							
											! write(200,*)"endif"				
										endif
										
								else
									last_recorded = mean_depth

									write(25,1500) Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,max_elevation,min_elevation,&
									&mean_depth,median_depth,station_num,station_10,station_25,station_50,station_100,NOAA_area 						
								endif
									
									! write(200,*) year,month,day,julian,count_day,mean_depth
						!			write(200,*)year,month,day,julian,count_day, mean_depth,previous_snwd,last_recorded,snwf_mean
							!		two_previous_snwd = previous_snwd
									previous_snwd = mean_depth				! mean_depth is reset as mean depth
									read(20,1000,end=17) Datestring,year,month,day,julian,i,j,longitude,latitude,&
									&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
									&station_25,station_50,station_100,NOAA_area
									 read(15,1500,end=17)snwf_Datestring,snwf_year,snwf_month,snwf_day,snwf_julian,snf_count_day,i,j,longitude,latitude,&
									 &snwf_max_elevation,snwf_min_elevation,snwf_mean,snwf_median,snwf_station_num,snwf_station_10,snwf_station_25,&
									 &snwf_station_50,snwf_station_100,snwf_NOAA_area 			! Read in snowfall data															
									

						!			write(200,*)year,month,day,julian,count_day, mean_depth,previous_snwd, last_recorded,snwf_mean

							enddo
						endif
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
		
1000 format(I8,I5,I3,I3,I8,2(I6),2(f10.3),10(I8))
1500 format(I8,I5,I3,I3,I8,I8,2(I6),2(f10.3),10(I8))		
		
		
		
		
end
