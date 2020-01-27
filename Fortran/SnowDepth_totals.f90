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
	  integer flag2,avg_max_mean,sum_rangeb,region,leap_count,k_count(366),DOY
	  real  Jan_count(52),Feb_count(52),Mar_count(52),Apr_count(52),May_count(52),Jun_count(52)
	  real  Jul_count(52),Aug_count(52),Sep_count(52),Oct_count(52),Nov_count(52),Dec_count(52)	  
!	  integer station_num,station_10,station_25,station_50,station_100
	  real  longitude,latitude,yearReal,day_mean(366),day_median(366),mean_sum(366),median_sum(366)
	  real  zero_percent(52),gtzero_counter(52),counter_76(52),percent_76(52),miss_counter(52),miss_percent(52)
	  real  miss_total, total_mpercent,total_counter,annCounter_76,annPercent_76
	  real  Jan_total(52),Feb_total(52),Mar_total(52),Apr_total(52),May_total(52),Jun_total(52)
	  real  Jul_total(52),Aug_total(52),Sep_total(52),Oct_total(52)
	  real 	Nov_total(52),Dec_total(52),flag(52),sumflag,Non_miss_days(52)
	  real  Jan_zdepth(52),Feb_zdepth(52),Mar_zdepth(52),Apr_zdepth(52),May_zdepth(52),Jun_zdepth(52)
	  real  Jul_zdepth(52),Aug_zdepth(52),Sep_zdepth(52),Oct_zdepth(52),Nov_zdepth(52),Dec_zdepth(52)
	  
	  real  Jan_zzdepth(52),Feb_zzdepth(52),Mar_zzdepth(52),Apr_zzdepth(52),May_zzdepth(52),Jun_zzdepth(52)
	  real  Jul_zzdepth(52),Aug_zzdepth(52),Sep_zzdepth(52),Oct_zzdepth(52),Nov_zzdepth(52),Dec_zzdepth(52)	  
	  
	  real  Jan_zmean,Feb_zmean,Mar_zmean,Apr_zmean,May_zmean,Jun_zmean
	  real  Jul_zmean,Aug_zmean,Sep_zmean,Oct_zmean,Nov_zmean,Dec_zmean
	  
	  real  Jan_zzmean,Feb_zzmean,Mar_zzmean,Apr_zzmean,May_zzmean,Jun_zzmean
	  real  Jul_zzmean,Aug_zzmean,Sep_zzmean,Oct_zzmean,Nov_zzmean,Dec_zzmean
	  
	  ! real  Jan_ztotal(52),Feb_ztotal(52),Mar_ztotal(52),Apr_ztotal(52),May_ztotal(52),Jun_ztotal(52)
	  ! real  Jul_ztotal(52),Aug_ztotal(52),Sep_ztotal(52),Oct_ztotal(52)
	  ! real 	Nov_ztotal(52),Dec_ztotal(52)	  

	  ! real  Jan_zztotal(52),Feb_zztotal(52),Mar_zztotal(52),Apr_zztotal(52),May_zztotal(52),Jun_zztotal(52)
	  ! real  Jul_zztotal(52),Aug_zztotal(52),Sep_zztotal(52),Oct_zztotal(52)
	  ! real 	Nov_zztotal(52),Dec_zztotal(52)
	  
	  real  Jan_Mean,Feb_Mean,Mar_Mean,Apr_Mean,May_Mean,Jun_Mean,Jul_Mean,Aug_Mean,Sep_Mean,Oct_Mean
	  real 	Nov_Mean,Dec_Mean,AvgMaxCount,SumMaxCount
	  real  per25,per50,per76,per125,per150,tot_snwcover,countGT0(52)
	  real count25(52),count50(52),count76(52),count100(52),count125(52),range_count(52)
	  real range_countb(52)

	  real AnnDecadeSum1,AnnDecadeSum2,AnnDecadeSum3,AnnDecadeSum4,AnnDecadeSum5
	  real AnnDecadeDif_1,AnnDecadeDif_2,AnnDecadeDif_3,AnnDecadeDif_4,AnnDecadeDif_5

	  real DMDecadeSum1,DMDecadeSum2,DMDecadeSum3,DMDecadeSum4,DMDecadeSum5
	  real DMDecadeDif_1,DMDecadeDif_2,DMDecadeDif_3,DMDecadeDif_4,DMDecadeDif_5
	  real SLDecadeSum1,SLDecadeSum2,SLDecadeSum3,SLDecadeSum4,SLDecadeSum5
	  real SLDecadeDif_1,SLDecadeDif_2,SLDecadeDif_3,SLDecadeDif_4,SLDecadeDif_5
	  ! real GDH(24), GDD(365,24),Daily_GDD(365),total_GDD(80),Base,total_100(80)
	  ! real total_GDH(80)

	  	  
	 character(len=500) :: inputfile,inputfile2,outputfile3,outputfile4,outputfile5,&
	 & outputfile6,outputfile7,outputfile8,outputfile9,outputfile10,outputfile11, &
	 & outputfile12,outputfile13,outputfile14,outputfile15,outputfile16,outputfile17

	  namelist /cdh_nml/ inputfile,inputfile2,outputfile3,outputfile4,outputfile5,&
	 & outputfile6,outputfile7,outputfile8,outputfile9,outputfile10,outputfile11,&
	 & outputfile12,outputfile13,outputfile14,outputfile15,outputfile16,outputfile17
open(33,file='cdh.nml',status='old')
read(33,nml=cdh_nml,end=55)
55 close(33)
write(*,nml=cdh_nml)



 

	  
	  ! open(10, file=inputfile,status="old")					 ! Snow depth
	  open(15, file=inputfile2,status="old")				 ! Snowfall 
	  ! open(20, file=outputfile,status="replace")             ! Split Date
	  open(25, file=inputfile,status="old")			 		 ! Count Date
	  open(30, file=outputfile3,status="replace")            ! MeanSnowDepth
	  open(31, file=outputfile4,status="replace")			 ! MaxReporting		
	  open(34, file=outputfile5,status="replace")            ! 76SnowDepth
	  open(35, file=outputfile6,status="replace")			 ! SnowDepth Quality
	  open(38, file=outputfile7,status="replace")			 ! Monthly Percent above x mm
	  open(40, file=outputfile8,status="replace")			 ! Daily Snow Depth
	  open(50, file=outputfile9,status="replace")			 ! Monthly Average
	  open(60, file=outputfile10,status="replace")	  		 ! Seasonal Snow Depth
	  ! Writing out "TOTAL" (i.e. all cells in one file) below.
	  open(80, file=outputfile11,status="replace")			 ! TOTAL Percent missing
	  open(81, file=outputfile12,status="replace")			 ! TOTAL Percent depth	  
	  open(82, file=outputfile13,status="replace")			 ! TOTAL Monthly Snow Depth
	  open(83, file=outputfile14,status="replace")			 ! TOTAL Monthly Snwd GE 76mm
	  open(84, file=outputfile15,status="replace")			 ! TOTAL Decade Percent GE76mm
	  open(85, file=outputfile16,status="replace")			 ! TOTAL Decade season length
	  Open(200,file=outputfile17, status = "replace")		 ! Junk
!	  open(50,file = outputfile3,status = "replace")
	  

	
	DMDecadeSum1 = 0.0
	DMDecadeSum2 = 0.0
	DMDecadeSum3 = 0.0
	DMDecadeSum4 = 0.0
	DMDecadeSum5 = 0.0

	DMDecadeDif_1 = 0.0
	DMDecadeDif_2 = 0.0
	DMDecadeDif_3 = 0.0
	DMDecadeDif_4 = 0.0
	DMDecadeDif_5 = 0.0
	
	SLDecadeSum1 = 0.0
	SLDecadeSum2 = 0.0
	SLDecadeSum3 = 0.0
	SLDecadeSum4 = 0.0
	SLDecadeSum5 = 0.0

	SLDecadeDif_1 = 0.0
	SLDecadeDif_2 = 0.0
	SLDecadeDif_3 = 0.0
	SLDecadeDif_4 = 0.0
	SLDecadeDif_5 = 0.0
	
	AnnDecadeSum1 = 0.0
	AnnDecadeSum2 = 0.0
	AnnDecadeSum3 = 0.0
	AnnDecadeSum4 = 0.0
	AnnDecadeSum5 = 0.0

	AnnDecadeDif_1 = 0.0
	AnnDecadeDif_2 = 0.0
	AnnDecadeDif_3 = 0.0
	AnnDecadeDif_4 = 0.0
	AnnDecadeDif_5 = 0.0	

	avg_max_mean = 0
	
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
		Non_miss_days(k) = 0.0
		
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
		
		Jan_total(k) = 0.0
		Feb_total(k) = 0.0
		Mar_total(k) = 0.0
		Apr_total(k) = 0.0
		May_total(k) = 0.0
		Jun_total(k) = 0.0
		Jul_total(k) = 0.0
		Aug_total(k) = 0.0
		Sep_total(k) = 0.0
		Oct_total(k) = 0.0
		Nov_total(k) = 0.0
		Dec_total(k) = 0.0
		
		
		
		! Jan_ztotal(k) = 0.0
		! Feb_ztotal(k) = 0.0
		! Mar_ztotal(k) = 0.0
		! Apr_ztotal(k) = 0.0
		! May_ztotal(k) = 0.0
		! Jun_ztotal(k) = 0.0
		! Jul_ztotal(k) = 0.0
		! Aug_ztotal(k) = 0.0
		! Sep_ztotal(k) = 0.0
		! Oct_ztotal(k) = 0.0
		! Nov_ztotal(k) = 0.0
		! Dec_ztotal(k) = 0.0

		! Jan_zztotal(k) = 0.0
		! Feb_zztotal(k) = 0.0
		! Mar_zztotal(k) = 0.0
		! Apr_zztotal(k) = 0.0
		! May_zztotal(k) = 0.0
		! Jun_zztotal(k) = 0.0
		! Jul_zztotal(k) = 0.0
		! Aug_zztotal(k) = 0.0
		! Sep_zztotal(k) = 0.0
		! Oct_zztotal(k) = 0.0
		! Nov_zztotal(k) = 0.0
		! Dec_zztotal(k) = 0.0

		Jan_zdepth(k) = 0.0
		Feb_zdepth(k) = 0.0
		Mar_zdepth(k) = 0.0
		Apr_zdepth(k) = 0.0
		May_zdepth(k) = 0.0
		Jun_zdepth(k) = 0.0
		Jul_zdepth(k) = 0.0
		Aug_zdepth(k) = 0.0
		Sep_zdepth(k) = 0.0
		Oct_zdepth(k) = 0.0
		Nov_zdepth(k) = 0.0
		Dec_zdepth(k) = 0.0
		
		Jan_zzdepth(k) = 0.0
		Feb_zzdepth(k) = 0.0
		Mar_zzdepth(k) = 0.0
		Apr_zzdepth(k) = 0.0
		May_zzdepth(k) = 0.0
		Jun_zzdepth(k) = 0.0
		Jul_zzdepth(k) = 0.0
		Aug_zzdepth(k) = 0.0
		Sep_zzdepth(k) = 0.0
		Oct_zzdepth(k) = 0.0
		Nov_zzdepth(k) = 0.0
		Dec_zzdepth(k) = 0.0		

		Jan_count(k) = 0.0
		Feb_count(k) = 0.0
		Mar_count(k) = 0.0
		Apr_count(k) = 0.0
		May_count(k) = 0.0
		Jun_count(k) = 0.0
		Jul_count(k) = 0.0
		Aug_count(k) = 0.0
		Sep_count(k) = 0.0
		Oct_count(k) = 0.0
		Nov_count(k) = 0.0
		Dec_count(k) = 0.0	
		
		countGT0(k) = 0
		count25(k) = 0
		count50(k) = 0
		count76(k) = 0
		count100(k) = 0 
		count125(k) = 0
		
		range_count(k) = 0.0
		
			do l = 1,366
				day_mean(l) = 0
				day_median(l) = 0
				mean_sum(l) = 0
				median_sum(l) = 0
				k_count(l) = 0

			enddo
	enddo

		
		write(30,3100) "year","k","SnowDepthMean","MeanMax","MaxDay","MeltLength","MaxReporting"    !"Stations Reporting","Stations 10mm","Stations 25mm","Station 50mm","Station 100mm"
		write(34,3410) "year","76Count","76Percent","MaxAbove76","First7day","Last7Days","DaysAbove76","flag"
		write(35,3510) "year","Zero Count","Zero Percent","7.6 Count","7.6 Percent","Miss Count","Miss Percent"
		write(38,3820) "year","Jan","Feb","Mar","Apr","Nov","Dec"
		write(50,5100) "year","Jan","Feb","Mar","Apr","Nov","Dec"
		! write(80,3610) "i","j","lat","lon","Miss Percent","Miss Count","Percent_76","Counter_76",&
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
		
		per25 = 0
		per50 = 0
		per76 = 0
		per125 = 0
		per150 = 0
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

		Jan_zmean = 0
		Feb_zmean = 0
		Mar_zmean = 0
		Apr_zmean = 0
		May_zmean = 0
		Jun_zmean = 0
		Jul_zmean = 0
		Aug_zmean = 0
		Sep_zmean = 0
		Oct_zmean = 0
		Nov_zmean = 0
		Dec_zmean = 0

		Jan_zzmean = 0
		Feb_zzmean = 0
		Mar_zzmean = 0
		Apr_zzmean = 0
		May_zzmean = 0
		Jun_zzmean = 0
		Jul_zzmean = 0
		Aug_zzmean = 0
		Sep_zzmean = 0
		Oct_zzmean = 0
		Nov_zzmean = 0
		Dec_zzmean = 0	

		leap_count = 0
		
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
					   

						do l=1,366
						!	write(200,*) "in leap"
							total_counter = total_counter + 1

!!!							write(200,*) total_counter,year,month,day,julian,count_day
							if (mean_depth .NE. -99999) then
								! if (julian .eq. 60) then
									! leap_count = leap_count + 1
								! endif
								Non_miss_days(k) = Non_miss_days(k) + 1
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
									Jan_count(k) = Jan_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Jan_zdepth(k) = Jan_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Jan_zzdepth(k) = Jan_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 32) .AND. (julian .LE. 60)) then       ! February
									Feb_total(k) = Feb_total(k) + mean_depth
									Feb_count(k) = Feb_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Feb_zdepth(k) = Feb_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Feb_zzdepth(k) = Feb_zzdepth(k) + 1
										endif
									endif										
								elseif ((julian .GE. 61) .AND. (julian .LE. 91)) then		! March
									Mar_total(k) = Mar_total(k) + mean_depth
									Mar_count(k) = Mar_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Mar_zdepth(k) = Mar_zdepth(k) + 1	
										if (mean_depth .ge. 76) then
											Mar_zzdepth(k) = Mar_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 92) .AND. (julian .LE. 121)) then		! April
									Apr_total(k) = Apr_total(k) + mean_depth
									Apr_count(k) = Apr_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Apr_zdepth(k) = Apr_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Apr_zzdepth(k) = Apr_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 122) .AND. (julian .LE. 152)) then		! May
									May_total(k) = May_total(k) + mean_depth
									May_count(k) = May_count(k) + 1
									if (mean_depth .gt. 25.4) then
										May_zdepth(k) = May_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											May_zzdepth(k) = May_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 153) .AND. (julian .LE. 182)) then		! June
									Jun_total(k) = Jun_total(k) + mean_depth
									Jun_count(k) = Jun_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Jun_zdepth(k) = Jun_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Jun_zzdepth(k) = Jun_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 183) .AND. (julian .LE. 213)) then		! July
									Jul_total(k) = Jul_total(k) + mean_depth
									Jul_count(k) = Jul_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Jul_zdepth(k) = Jul_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Jul_zzdepth(k) = Jul_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 214) .AND. (julian .LE. 244)) then		! August
									Aug_total(k) = Aug_total(k) + mean_depth
									Aug_count(k) = Aug_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Aug_zdepth(k) = Aug_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Aug_zzdepth(k) = Aug_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 245) .AND. (julian .LE. 274)) then		! September
									Sep_total(k) = Sep_total(k) + mean_depth
									Sep_count(k) = Sep_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Sep_zdepth(k) = Sep_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Sep_zzdepth(k) = Sep_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 275) .AND. (julian .LE. 305)) then		! October
									Oct_total(k) = Oct_total(k) + mean_depth
									Oct_count(k) = Oct_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Oct_zdepth(k) = Oct_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Oct_zzdepth(k) = Oct_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 306) .AND. (julian .LE. 335)) then		! November
									Nov_total(k) = Nov_total(k) + mean_depth									
									Nov_count(k) = Nov_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Nov_zdepth(k) = Nov_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Nov_zzdepth(k) = Nov_zzdepth(k) + 1
										endif
									endif
								elseif ((julian .GE. 336) .AND. (julian .LE. 366)) then		! December
									Dec_total(k) = Dec_total(k) + mean_depth	
									Dec_count(k) = Dec_count(k) + 1
									if (mean_depth .gt. 25.4) then
										Dec_zdepth(k) = Dec_zdepth(k) + 1
										if (mean_depth .ge. 76) then
											Dec_zzdepth(k) = Dec_zzdepth(k) + 1
										endif
									endif
								endif
								
						! Checking for mean_depth values greater than 0.
								if (mean_depth .gt. 0) then
									gtzero_counter(k) = gtzero_counter(k) + 1
								endif
								if ((count_day .GE. 93) .and. (count_day .LE. 275)) then			! Beginning of October to end of March.						
									if (mean_depth .gt. 0) then
										countGT0(k) = countGT0(k) + 1
									endif
									if (mean_depth .ge. 25) then
										count25(k) = count25(k) + 1
									endif
									if (mean_depth .ge. 50) then
										count50(k) = count50(k) + 1
									endif
									if ((count_day .GE. 154) .and. (count_day .LE. 275)) then  ! 7.6cm starts in November ends March 31st based on monthly analysis.
										if (mean_depth .ge. 76) then
											count76(k) = count76(k) + 1
										endif
										range_count(k) = range_count(k) + 1
									endif
									if (mean_depth .ge. 100) then
										count100(k) = count100(k) + 1
									endif
									if (mean_depth .ge. 125) then
										count125(k) = count125(k) + 1
									endif
									range_countb(k) = range_countb(k) + 1   ! Counting number of days between November and end of March.
								endif			
								
								if (mean_depth .ge. 76) then        ! Checking if depth is greater than 76mm. (data is in mm). Are these duplicates?
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
						
						if (max_mean(k) .gt. abs_max) then
							abs_max = max_mean(k)
							max_year = max_mean_year(k)
						endif
						
						if (First7day(k) .ne. 0) then
							First7day(k) = First7day(k) - 7
							FstLstDiff(k) = Last7Day(k) - First7day(k)
						endif
						
						AvgElevation(k) = (abs_max_elevation(k) - abs_min_elevation(k))/2  ! This gives an average for each year of the absolute max and min elevation.
						
						zero_percent(k) = (gtzero_counter(k)/365.0)*100    ! percent of year covered by snow.
						percent_76(k) = (counter_76(k)/365.0)*100
						miss_percent(k) = (miss_counter(k)/365.0)*100							
						
						Jan_total = Jan_total*0.1      ! Converting to cm.
						Feb_total = Feb_total*0.1
						Mar_total = Mar_total*0.1
						Apr_total = Apr_total*0.1
						May_total = May_total*0.1
						Jun_total = Jun_total*0.1
						Jul_total = Jul_total*0.1
						Aug_total = Aug_total*0.1
						Sep_total = Sep_total*0.1
						Oct_total = Oct_total*0.1
						Nov_total = Nov_total*0.1
						Dec_total = Dec_total*0.1
						
						
						write(30,3000) study_year,k,Depth_mean(k), max_mean(k),&
						&max_count(k),melt_length(k),max_reporting(k)  ! This file will only write on calculated statistics. i.e. sum, mean, max, min, standard deviation.	
						write(31,*) max_reporting(k)
						write(34,3400) study_year,counter_76(k),percent_76(k), MaxDaysAbove76(k),&
						&First7day(k),Last7day(k),FstLstDiff(k),DaysAbove76,flag(k)
						write(35,3500) study_year,gtzero_counter(k),zero_percent(k),counter_76(k),percent_76(k),miss_counter(k),miss_percent(k)    ! This file will be to check the quality of the data. The gtzero_counter could be important for number of days with snow on the ground.
						! write(50,5000) study_year,Jan_total(k),Feb_total(k),Mar_total(k),Apr_total(k),May_total(k),Jun_total(k),&
									! &Jul_total(k),Aug_total(k),Sep_total(k),Oct_total(k),&
									! &Nov_total(k),Dec_total(k)
						write(50,5000) study_year,Jan_total(k),Feb_total(k),Mar_total(k),Apr_total(k),&
									&Nov_total(k),Dec_total(k)
						write(38,3800)study_year,Jan_zdepth(k),Feb_zdepth(k),Mar_zdepth(k),Apr_zdepth(k),&
									&Nov_zdepth(k),Dec_zdepth(k)
									
						study_year = study_year + 1
					enddo ! end year loop						
				endif
!			write(200,*) year,study_year, month,day,julian,count_day,k,l,mean_depth,Depth_mean(k),Depth_mean_k(k), Depth_mean_sum(k)			
		enddo

25	   				rewind(25)	
					total_mpercent = ((miss_total-40)/total_counter)*100         ! percent of missing data in the whole dataset is calculated here.			
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
			AvgFst7day = 0
			AvgLst7day = 0
		endif
		
		avg_max_mean = (((sum(max_mean,k))/k))/10
		
		tot_snwcover = ((sum(countGT0,k))/(sum(range_count,k)))*100
		
		per25 = ((sum(count25,k))/(sum(range_countb,k)))*100
		per50 = ((sum(count50,k))/(sum(range_countb,k)))*100
		per76 = ((sum(count76,k))/(sum(range_count,k)))*100
		per125 = ((sum(count100,k))/(sum(range_count,k)))*100
		per150 = ((sum(count125,k))/(sum(range_count,k)))*100
		sum_range = sum(range_count,k)		! For December to March
		sum_rangeb = sum(range_countb,k)
		
		AbsAverageElevation = (sum(AvgElevation,k))/k
!		write(200,*) i,j,latitude,longitude,sum(range_count,k),sum(count125,k)
		
		if (per76 .lt. 25) then
			region = 1
		elseif ((per76 .ge. 25) .and. (per76 .lt. 50)) then
			region = 2
		elseif ((per76 .ge. 50) .and. (per76 .lt.75)) then
			region = 3
		elseif (per76 .ge. 75) then
			region = 4
		endif
		
		
		write(80,3600) i,j,latitude,longitude,AbsAverageElevation,total_mpercent,miss_total,annPercent_76,annCounter_76,avg_max_mean,&
		&AvgMaxCount,abs_max,max_year,AvgFst7day,AvgLst7day,AvgFstLstDiff,sumflag,total_counter,total_max_reporting,&
		TotalZeroReportYears,TotalReporting25  !	percent missing is wrtitten out here. 
		write(81,3700) i,j,latitude,longitude,sum_range,tot_snwcover,per25,per50,per76,per125,per150,region

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
		
		Jan_zmean = ((sum(Jan_zdepth,k))/(sum(Jan_count,k)))*100
		Feb_zmean = ((sum(Feb_zdepth,k))/(sum(Feb_count,k)))*100
		Mar_zmean = ((sum(Mar_zdepth,k))/(sum(Mar_count,k)))*100
		Apr_zmean = ((sum(Apr_zdepth,k))/(sum(Apr_count,k)))*100
		May_zmean = ((sum(May_zdepth,k))/(sum(May_count,k)))*100
		Jun_zmean = ((sum(Jun_zdepth,k))/(sum(Jun_count,k)))*100
		Jul_zmean = ((sum(Jul_zdepth,k))/(sum(Jul_count,k)))*100
		Aug_zmean = ((sum(Aug_zdepth,k))/(sum(Aug_count,k)))*100
		Sep_zmean = ((sum(Sep_zdepth,k))/(sum(Sep_count,k)))*100
		Oct_zmean = ((sum(Oct_zdepth,k))/(sum(Oct_count,k)))*100
		Nov_zmean = ((sum(Nov_zdepth,k))/(sum(Nov_count,k)))*100
		Dec_zmean = ((sum(Dec_zdepth,k))/(sum(Dec_count,k)))*100

		Jan_zzmean = ((sum(Jan_zzdepth,k))/(sum(Jan_count,k)))*100
		Feb_zzmean = ((sum(Feb_zzdepth,k))/(sum(Feb_count,k)))*100
		Mar_zzmean = ((sum(Mar_zzdepth,k))/(sum(Mar_count,k)))*100
		Apr_zzmean = ((sum(Apr_zzdepth,k))/(sum(Apr_count,k)))*100
		May_zzmean = ((sum(May_zzdepth,k))/(sum(May_count,k)))*100
		Jun_zzmean = ((sum(Jun_zzdepth,k))/(sum(Jun_count,k)))*100
		Jul_zzmean = ((sum(Jul_zzdepth,k))/(sum(Jul_count,k)))*100
		Aug_zzmean = ((sum(Aug_zzdepth,k))/(sum(Aug_count,k)))*100
		Sep_zzmean = ((sum(Sep_zzdepth,k))/(sum(Sep_count,k)))*100
		Oct_zzmean = ((sum(Oct_zzdepth,k))/(sum(Oct_count,k)))*100
		Nov_zzmean = ((sum(Nov_zzdepth,k))/(sum(Nov_count,k)))*100
		Dec_zzmean = ((sum(Dec_zzdepth,k))/(sum(Dec_count,k)))*100		
		write(200,*) sum(Jan_zdepth,k),sum(Jan_count,k)
		
		
		! AnnDecadeSum1 = ((sum(count76(3:12))/(sum(range_count(3:12)))))*100				! Calculating number of days above 7.6cm for annual values.
		! AnnDecadeSum2 = ((sum(count76(13:22))/(sum(range_count(13:22)))))*100
		! AnnDecadeSum3 = ((sum(count76(23:32))/(sum(range_count(23:32)))))*100
		! AnnDecadeSum4 = ((sum(count76(33:42))/(sum(range_count(33:42)))))*100
		! AnnDecadeSum5 = ((sum(count76(43:52))/(sum(range_count(43:52)))))*100

		! AnnDecadeDif_1 = per76-AnnDecadeSum1					! Calculating Percentage Anamolies
		! AnnDecadeDif_2 = per76-AnnDecadeSum2
		! AnnDecadeDif_3 = per76-AnnDecadeSum3
		! AnnDecadeDif_4 = per76-AnnDecadeSum4
		! AnnDecadeDif_5 = per76-AnnDecadeSum5

		!!!!!!!!!!!!!!!!!!!!!!! Decadal above 7.6 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		DMDecadeSum1 = ((sum(count76(3:12))/(sum(range_count(3:12)))))*100				! Calculating number of days above 7.6cm by decade between November and March
		DMDecadeSum2 = ((sum(count76(13:22))/(sum(range_count(13:22)))))*100
		DMDecadeSum3 = ((sum(count76(23:32))/(sum(range_count(23:32)))))*100
		DMDecadeSum4 = ((sum(count76(33:42))/(sum(range_count(33:42)))))*100
		DMDecadeSum5 = ((sum(count76(43:52))/(sum(range_count(43:52)))))*100

		DMDecadeDif_1 = DMDecadeSum1 - per76				! Calculating Percentage Anamolies
		DMDecadeDif_2 = DMDecadeSum2 - per76
		DMDecadeDif_3 = DMDecadeSum3 - per76
		DMDecadeDif_4 = DMDecadeSum4 - per76
		DMDecadeDif_5 = DMDecadeSum5 - per76
		!write(*,*) Decade
		
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Season Length	!!!!!!!!!!!!!!!!!!!!!!!!
	! Taking the decadal average of the season length and then caculating the difference in anamoly.
		SLDecadeSum1 = ((sum(FstLstDiff(3:12)))/10)				! SL == Season Length
		SLDecadeSum2 = ((sum(FstLstDiff(13:22)))/10)
		SLDecadeSum3 = ((sum(FstLstDiff(23:32)))/10)
		SLDecadeSum4 = ((sum(FstLstDiff(33:42)))/10)
		SLDecadeSum5 = ((sum(FstLstDiff(43:52)))/10)
		
		SLDecadeDif_1 = SLDecadeSum1- AvgFstLstDiff					! Calculating  Anamolies
		SLDecadeDif_2 = SLDecadeSum2- AvgFstLstDiff
		SLDecadeDif_3 = SLDecadeSum3- AvgFstLstDiff
		SLDecadeDif_4 = SLDecadeSum4- AvgFstLstDiff
		SLDecadeDif_5 = SLDecadeSum5- AvgFstLstDiff		 !WRITE OUT
		
		
write(38,3810) "Percent Cover",Jan_zmean,Feb_zmean,Mar_zmean,Apr_zmean,&
&Nov_zmean,Dec_zmean


write(50,5010) "Mean",Jan_Mean,Feb_Mean,Mar_Mean,Apr_Mean,Nov_Mean,Dec_Mean	
! write(50,5010) "Mean",Jan_Mean,Feb_Mean,Mar_Mean,Apr_Mean,May_Mean,Jun_Mean,Jul_Mean,&
! &Aug_Mean,Sep_Mean,Oct_Mean,Nov_Mean,Dec_Mean				
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
! Writing the average mean snow cover greater than 0cm for each month.
write(82,8200) i,j,latitude,longitude,Jan_zmean,Feb_zmean,Mar_zmean,Apr_zmean,May_zmean,&
&Jun_zmean,Jul_zmean,Aug_zmean,Sep_zmean,Oct_zmean,Nov_zmean,Dec_zmean

write(83,8200) i,j,latitude,longitude,Jan_zzmean,Feb_zzmean,Mar_zzmean,Apr_zzmean,May_zzmean,&
&Jun_zzmean,Jul_zzmean,Aug_zzmean,Sep_zzmean,Oct_zzmean,Nov_zzmean,Dec_zzmean

write(84,8400) i,j,latitude,longitude,DMDecadeSum1,DMDecadeSum2,DMDecadeSum3,DMDecadeSum4,DMDecadeSum5,&
&DMDecadeDif_1,DMDecadeDif_2,DMDecadeDif_3,DMDecadeDif_4,DMDecadeDif_5

write(85,8400) i,j,latitude,longitude,SLDecadeSum1,SLDecadeSum2,SLDecadeSum3,SLDecadeSum4,SLDecadeSum5,&
&SLDecadeDif_1,SLDecadeDif_2,SLDecadeDif_3,SLDecadeDif_4,SLDecadeDif_5
		
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Taking sum of average snowfall on a given day for every year.
		leap_count = 13
		do k = 1,52
			do l = 1,366
				read(25,1500,end=26)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&
					&max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
					&station_25,station_50,station_100,NOAA_area
					 ! if ((month .eq. 2) .and. (day .eq. 29)) then
						! read(25,1500,end=26)Datestring,year,month,day,julian,count_day,i,j,longitude,latitude,&
						! &max_elevation,min_elevation,mean_depth,median_depth,station_num,station_10,&
						! &station_25,station_50,station_100,NOAA_area
					 ! endif
				if (julian .EQ. l) then
					if (mean_depth .NE. -99999) then
						k_count(l) = k_count(l) + 1
						mean_sum(l) = mean_sum(l) + mean_depth
						median_sum(l) = median_sum(l) + median_depth
					endif
				endif
			enddo
		enddo
		

26		write(40,4100) "DayOfYear","MeanOfDay","MedianOfDay"				
		do l = 1,366		! This may need to be redone becaue of leap year addition.
			DOY = l
			if (l .eq. 60) then
				! day_mean(l) = (mean_sum(l)/leap_count) * 0.1
				! day_median(l) = (median_sum(l)/leap_count) * 0.1
				continue
			else
				day_mean(l) = (mean_sum(l)/k_count(l))	* 0.1				! Put into cm.
				day_median(l) = (median_sum(l)/k_count(l)) * 0.1
				if (l .ge. 60) then
					write(40,4000) DOY-1,day_mean(l),day_median(l),k_count(l)
				else
					write(40,4000) DOY,day_mean(l),day_median(l),k_count(l)	
				endif
			endif
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
 3600 format(2(I5),2(f14.6),I6,4(f14.3),I14,f14.2,5(I14),2(f14.1),3(I6))
 3610 format(2(a5),2(a14),5(a14),a14,a14)
 3700 format(2(I5),2(f12.4),I8,6(f8.2),I8)
 3800 format(I14,12(f14.2))
 3810 format(a14,12(f14.2))
 3820 format(13(a14))
 4000 format(I11,2(f15.3),I8)
 4100 format(a11,2(a15))
 5000 format(I10,10(f10.2))
 5100 format(11(a10))
 5010 format(a10,10(f10.2))
 8200 format(2(I5),2(f12.4),12(f12.2))
 8400 format(2(I5),2(f12.4),10(f12.2))
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









