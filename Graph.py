import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def read_file(CellList,filelist,input_location):
    while True:
        if input_location == "1":
            list_files = ["76SnowDepth.txt","DaySnowDepth.txt","MeanSnowDepth.txt","MonthlyAverage.txt","SDQuality.txt","SeasonalSnowDepth.txt"]
            print(list_files)
            file = input("Input a file name: ")               
            try:   
                for cell in CellList:            
                    open("U:\Research\SnowDepth_Data\Outputs\%s\SnowDepth\%s_%s" %(cell,cell,file))
                    filelist.append("U:\Research\SnowDepth_Data\Outputs\%s\SnowDepth\%s_%s" %(cell,cell,file))
    #            print(filelist)
                return file,filelist
            except FileNotFoundError:
                print("file doesn't exist")
        elif input_location == "2":
            list_files = ["AllPercent_Decade76.txt","AllPercent_Depth.txt","AllPercent_missing.txt","AllPercent_Monthly76.txt","AllPercent_MonthlySD.txt"]
            print(list_files)
            file = input("Input a file name: ") 
            try:
   #             print(file)  

                filelist = pd.read_fwf("U:\Research\SnowDepth_Data\Outputs\All\PercentMiss\%s" %(file),header=None)
 #               print(filelist)
  #              print(file)
                return file,filelist
            except:
 #               print(filelist)
                print("file doesn't exist")
                break
        else:
            print("try again")

def region_define(region_select):
    CellList = []
    reg_file = pd.read_fwf("U:\Research\SnowDepth_Data\Outputs\All\PercentMiss\AllPercent_Depth.txt",header=None)

    sort_region = reg_file.loc[reg_file[11] == int(region_select)]
#    cell_reg = sort_region[[0,1,11]]
    cell_reg = sort_region[0].astype(str) + sort_region[1].astype(str)  
    CellList = cell_reg.tolist()
    
    return CellList
def get_header(filelist):
    for f in filelist:
        f_open= open(f)
        line = f_open.readline()
    header=[]
   # print(line)
    split_line = line.split()
  #  print(split_line)
    for l in line:
        header.append(split_line[1])
    header = header[0]
    f_open.close()
    return header,line,split_line

def pick_var(var):
    while True:
        input4 = input("Select from the list of columns: ")             
        if input4 in var:
            head = input4
            var_index = var.index(head)        
            return head,var_index                  
        else:
             print("variable does not exist, try again")       
        
def row_file(Blacklist,x1,x2):
    row_list = []   
    for ij in range(int(x1),int(x2)+1):
         if str(ij) in str(Blacklist):
            continue
         else:
            row_list.append(str(ij)) 
    return row_list

def col_file(Blacklist,y1,y2):
    col_list = []   
    for ij in range(int(y1),int(y2)+1,100):
         if str(ij) in str(Blacklist):
            continue
         else:
            col_list.append(str(ij)) 
    return col_list        

def total_file(Blacklist):   
    CellList = []
    for i in range(12,28):
        for j in range(33,42):
            if (str(i)+str(j)) in str(Blacklist):
                continue
            else:
                CellList.append(str(i)+str(j))
    return CellList

def Monthly_plot(filelist,header,plt_index,selection,region):
    y_list = []
    y=[]
    fname_lst = []
    plt.figure(figsize=(20,10))
    for fname in filelist:
        try:
    #        data=np.loadtxt(fname,skiprows=1)
            data= np.loadtxt((open(fname).readlines()[:-1]), skiprows=1, dtype=None)
            plt.title(header)
            x = data[:,0]
            y_temp = data[:,plt_index]
            y_list.append(y_temp)
      #      plt.plot(x,y,label=fname[35:39])
        except IOError:
            continue 
    y_list= np.array(y_list).tolist()
    y= np.mean(y_list,axis=0)
    ymin = np.min(y_list,axis=0)
    ymax = np.max(y_list,axis=0)
    
    if selection == "all": 
        title = header + " "+ selection + " " + "Cells"
    elif selection == "region":
         title = header + " "+ selection + " " + region      
    else:
        title = header + " "+ selection + " " + "%s to %s" %(fname_lst[0],fname_lst[-1])
    
    
    plt.title(title)                    # Plotting title from above
    plt.plot(x,y,label="Average") # change to "Average"?
    plt.plot(x,ymin,label="Min",linestyle="dashed")
    plt.plot(x,ymax,label="Max",linestyle="dashed")
    plt.legend()
def Plot76(filelist,header,plt_index):
    plt.figure(figsize=(20,10))
    for fname in filelist:
        try:
         #   data=np.loadtxt(fname,skiprows=1,usecols=(1,2,3))
            data=np.loadtxt(fname,skiprows=1)
  #          header = get_header()
            plt.title(header)
            x=data[:,0]
            y=data[:,plt_index]
            plt.plot(x,y,label=fname[35:39])
            plt.legend()            
        except IOError:
            continue 

                
def Day_mean(filelist,header,plt_index,selection):
    y_list = []
    y=[]
    fname_lst = []
    plt.figure(figsize=(20,10))
    for fname in filelist:
        try:
         #   data=np.loadtxt(fname,skiprows=1,usecols=(1,2,3))
            data=np.loadtxt(fname,skiprows=1)
  #          header = get_header()
            x=data[:,0]
            y_temp=data[:,plt_index]
            y_list.append(y_temp)
            fname_lst.append(fname[35:39])

        except IOError:
            continue 
    y_list= np.array(y_list).tolist()
  #  print(y_list)
    y= np.mean(y_list,axis=0)
    ymin = np.min(y_list,axis=0)
    ymax = np.max(y_list,axis=0)
  #  print(fname_lst)
    title = header + " "+ selection + " " + "%s to %s" %(fname_lst[0],fname_lst[-1])
  #  y_list=np.ndarray.tolist(y_list)  
    plt.title(title)
   
    plt.plot(x,y,label="Average") # change to "Average"?
    plt.plot(x,ymin,label="Min",linestyle="dashed")
    plt.plot(x,ymax,label="Max",linestyle="dashed")
    plt.legend()
    plt.savefig("U:\\Research\\GIS\\Maps\\figures\\MeanOfDay.png")

def Plot_Decade76(file,CellList,col):           # DOES NOT WORK AS PLANNED NEED TO RETHINK
    file['ij'] = file['i'].astype(str)+ file['j'].astype(str)  
    plotlist = file[file['ij'].isin(CellList)]
    plotlist['ij'] = plotlist['ij'].astype(int)
    print(plotlist)          
    print(len(plotlist['ij']))
  #  x = plotlist[["1967-1977","1977-1987","1987-1997","1997-2007","2007-2017"]].values
  #  y = plotlist[["ij"]].values
    ax =  plotlist.plot(kind='scatter',y='ij',x="1967-1977")
    plotlist.plot(kind='scatter',y='ij',x="1977-1987",ax=ax)
    plotlist.plot(kind='scatter',y='ij',x="1987-1997",ax=ax)
    plotlist.plot(kind='scatter',y='ij',x="1997-2007",ax=ax)
    plotlist.plot(kind='scatter',y='ij',x="2007-2017",ax=ax)
    
    plt.show()
#    y = CellList
#    plt.plot(x,y)    
def SnowDepthMean(file,header,plt_index,split_line):
  #  print(header)
    for f in file:
        data=pd.read_csv(f,skiprows=1)
       # data["year"]#,index_col=["year","MaxReporting"]) 
     #   usecols=["year","MaxReporting"])

   # print(data)

def main():
    pd.set_option('display.max_columns', None)  
    pd.set_option('display.expand_frame_repr', False)
    pd.set_option('max_colwidth', -1)
    filelist = []
    Blacklist=[2741,2740,2739,2738,2737,2736,2641,2640,2639,2638,2637,2636,2541,2540,2539,2538,2441,2440,2439,2438,2341,2340,2339,2241,2240,2141]       
    while True:
        input_location = input("input (1) for individual files, (2) for concatenated files: ")
        if input_location == "1":
            selection = input("display row,column, all,region or end?: ")
            if selection == "end":
                break
            if selection == "all":
                CellList = total_file(Blacklist)
            elif selection == "row" or selection == "column":
                input1= input("Input coordinate 1,or 'end': " )
                if input1.lower() == 'end':
                     break
                else:    
                    input2= (input("Input coordinate 2: " ))
                    if input2.lower() == 'end':
                        break
                    else:        
        #                CellList = row_file(Blacklist,input1,input2)
                        if selection.lower() == "row":
                            CellList = row_file(Blacklist,input1,input2)
                        elif selection.lower() == "column":
                            CellList = col_file(Blacklist,input1,input2)
            elif selection == "region":
                region_select = input("Select a region between 1-4: ")
                if region_select.lower() == 'end':
                    break
                else:
                    CellList = region_define(region_select)
            
            input3,filelist = read_file(CellList,filelist,input_location)            
            header,var,split_line = get_header(filelist)        
            print(var)            
            header,plt_index = pick_var(split_line)                
            
            if input3 == "MonthlyAverage.txt":
                Monthly_plot(filelist,header,plt_index,selection,region_select)
                plt.show()
#                plt.clf()
            elif input3 == "76SnowDepth.txt":
                Plot76(filelist,header,plt_index)
                plt.show()
                plt.clf()
            elif input3 == "DaySnowDepth.txt":                  # Going to attempt to find average and max/min to graph. Will implement if it works.
                Day_mean(filelist,header,plt_index,selection)
                plt.show()
#                plt.clf()
            elif input3 == "MeanSnowDepth.txt":
                SnowDepthMean(filelist,header,plt_index,split_line)
                plt.show()
                plt.clf()
        elif input_location == "2":
            selection = input("display row,column,region, all or end?: ")
            if selection == "end":
                break
            if selection == "all":
                CellList = total_file(Blacklist)
            elif selection == "row" or selection == "column":
                input1= input("Input coordinate 1,or 'end': " )
                if input1.lower() == 'end':
                     break
                else:    
                    input2= (input("Input coordinate 2: " ))
                    if input2.lower() == 'end':
                        break
                    else:       
                        if selection.lower() == "row":
                            CellList = row_file(Blacklist,input1,input2)
                        elif selection.lower() == "column":
                            CellList = col_file(Blacklist,input1,input2)
            elif selection == "region":
                region_select = (input("Select a region between 1-4: "))
                if region_select.lower() == 'end':
                    break
                else:
                    CellList = region_define(region_select)
                
                            
            input3,filelist = read_file(CellList,filelist,input_location)
            print(input3)
            if input3 == "AllPercent_Decade76.txt":
                col=["i","j","lat","lon","1967-1977","1977-1987","1987-1997",\
                "1997-2007","2007-2017","DecDif 1-2","DecDif 2-3","DecDif 3-4","DecDif 4-5"]
                filelist.columns = col
            #    print(filelist)                
                Plot_Decade76(filelist,CellList,col)
                
        else:
        #    input3.close()
            break
#        plt.figure(figsize=(20,10))
    


    else:
        quit
    
if __name__ == "__main__":
    main()        
