#!/bin/bash
################################################################
#1.  Date
#2.  j coordinate in 89x89 grid
#3.  i coordinate in 89x89 grid
#4.  Longitude of 89x89 cell (decimal degrees)
#5.  Latitude of 89x89 cell (decimal degrees)
#6.  Maximum elevation of stations reporting snowfall in cell (m)
#7.  Minimum elevation of stations reporting snowfall in cell (m)
#8.  Mean snow depth of stations reporting snowfall in cell (mm)
#9.  Median snow depth of stations reporting snowfall in cell (mm)
#10. Number of stations reporting in file
#11. Number of stations reporting > 10mm snowfall in cell
#12. Number of stations reporting > 25mm snowfall in cell
#13. Number of stations reporting > 50mm snowfall in cell
#14. Number of stations reporting > 100mm snowfall in cell
#15. NOAA cell area
#16. NOAA snow cover flag (1=snow covered) (not included)
 

 pgm=SnowDepth_extract

# set -x


datadir=/mnt/e/School/Thesis/SnowDepth_Data/snwd
outputdir=/mnt/e/School/Thesis/SnowDepth_Data/Outputs

start_time=$(date +%s)
echo $(date)

Blacklist="2741 2538 2441 2440 2439 2438 2341 2340 2339 2241 2240 2141" 

for i in {12..27}
do
	for j in {33..41}
		do

		if [[ $Blacklist =~ $i$j ]]; then
			continue 							# If the number is in the list of cells not being used then it will skip this cell and proceed to the next cell.
		fi
		list+=($i$j)
		
#		 mkdir -p /mnt/e/School/Thesis/Python_Code/Snowfall_Data/1960s/Outputs
		 mkdir -p /mnt/e/School/Thesis/SnowDepth_Data/Outputs/$i$j

done
done

			
#		 for filein in `ls -d $datadir/*.txt`
		   

	#	   do
#			  filename=`basename $filein|sed 's/\.txt//'`
			fileout=$outputdir/




	  #run the excutable
#	 ./$pgm
#	  awk '$2==$i {print FILENAME $0}' *.txt >"${fileout}${i}_${j}_Snowfall_1960s.txt"
#	  awk -v i=$i -v j=$j '$2==j && $3==i {print $0}' $datadir/*.txt >"${fileout}${i}_${j}_SnowDepth.txt"   # This function will find a matching i and j then extract to file.

# This function will find a matching i and j then extract to file.
awkarray=$(
  # substitute space for newline
  <<<${list[@]} tr ' ' '\n' | 
  # remove empty newlines
  sed '/^$/d' | 
  # add 'a[' to the beginning of the line
  # and add ']' to the ending of the line
  # so we have a[number] a[anothernumber] etc.
  sed 's/^/a[/; s/$/]/'
)
awk -vfileout="$fileout" '
  BEGIN {'"$awkarray"'}
  $3 $2 in a { 
    print $0 > fileout $3 "_" $2 "_SnowDepth.txt"
  }
' "$datadir"/*.txt


for string in ${list[@]}	
do
	 i=${string:0:2}
	 j=${string:2:2}
	mv /$outputdir/${i}_${j}_SnowDepth.txt /$outputdir/$i$j/${i}_${j}_SnowDepth.txt
done
		

		
	  if [ $? -ne 0 ]; then
		echo "$run finished with error"
		  exit
	  fi
   
		echo "Moving to next file..."
		echo
	  
		   



#rm -f data1.bin
end_time=$(date +%s)
runtime=$((($(date +%s)-$start_time)/60))
echo "END OF SCRIPT at $(date) in $runtime minutes"
#echo "Moving to next decade..."
