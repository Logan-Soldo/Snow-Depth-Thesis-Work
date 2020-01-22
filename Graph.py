import numpy as np
import matplotlib.pyplot as plt
from matplotlib.ticker import AutoMinorLocator
import pandas as pd
from scipy.interpolate import interp1d as interp
from scipy.stats import linregress
from scipy.stats import stats


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
    '''
    This section selects cells only with the region selected by user.
    '''
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

def title_gen():
    title = input("Input a title for this graph: ")
    yaxis = input("Input a y-axis title: ")
    xaxis = input("Input an x-axis title: ")
    savefig = input("Input a file name: ")
    return title,yaxis,xaxis,savefig


######## Below this are all of the plotting functions. ##########
def Monthly_plot(filelist,header,plt_index,selection,region):               # Use this example for global plotting application
    '''
    Introducing subplotting here as a demo.
    '''
    data_list = []
    for fname in filelist:
        try:
            data= np.loadtxt((open(fname).readlines()[:-1]), skiprows=1, dtype=None)  # Coming out of loop too early, need to build a list of lists.
           # plt.title(header)
#            x = data[:,0]
#            y_temp = data[:,1]          # plt_index gets its index when variable is chosen by user.
#            y_list.append(y_temp)
      #      plt.plot(x,y,label=fname[35:39])
            data_list.append(data)
        except IOError:
            continue
  #  data_list = np.array(data_list).tolist()

    plt_select1(selection,header,region,data_list)

def Plot76(filelist,header,plt_index,selection,region):           # Added global plotting     
    y_list = []
    for fname in filelist:
        try:
            data=np.loadtxt(fname,skiprows=1)
           # plt.title(header)
            x = data[:,0]
            y_temp = data[:,plt_index]
            y_list.append(y_temp)
      #      plt.plot(x,y,label=fname[35:39])
        except IOError:
            continue
    
    plt_select1(x,y_list,selection,header,region)

def Day_mean(filelist,header,plt_index,selection,region):                  # Added global plotting
    
    y_list = []
    for fname in filelist:
        try:
            data=np.loadtxt(fname,skiprows=1)
          #  plt.title(header)
            x = data[:,0]
            y_temp = data[:,plt_index]
            y_list.append(y_temp)
      #      plt.plot(x,y,label=fname[35:39])
        except IOError:
            continue
    
    plt_select1(x,y_list,selection,header,region)
    
def plt_select1(selection,header,region,data_list):

    fname_lst = []
    generate_title = input("Would you like to generate a title? (y/n): ")    
    if generate_title == "n":
        if selection == "all": 
            title = header + " "+ selection + " " + "Cells"
            yaxis = "test"
            xaxis = "test"            
            figname = input("Input a file name: ")
        elif selection == "region":
             title = header + " "+ "Region" + " " + region 
             yaxis = "test"
             xaxis = "test"             
             figname = input("Input a file name: ")
        else:
            title = header + " "+ selection + " " + "%s to %s" %(fname_lst[0],fname_lst[-1])
            yaxis = "test"
            xaxis = "test"
            figname = input("Input a file name: ")
    else:
        title,yaxis,xaxis,figname = title_gen()
    

#    r_round = round(r[0],3)
#    m_round = round(m,3)

    c = color_select()
    for i in range(len(c)):
        r,g,b = c[i]
        c[i] = (r / 255., g / 255., b / 255.)
        
#    f = interp(x,y)
#    f2 = interp(x,y,kind="cubic")
#    xnew = np.linspace(1966, 2017,num=1000)
    plt.rc('font', family='serif')  
    plt.rc('xtick', labelsize='x-small')
    plt.rc('ytick', labelsize='x-small')    
    
    fig,sub =  plt.subplots(5, 2, figsize=(12,8),constrained_layout=True)
#    plt.figure(figsize=(12,8)) 
    fig.suptitle(title,fontsize=12)                    # Plotting title from above.
    #fig.autoscale()

    months = ["January","February","March","April","May","June",
              "September","October","November","December"]
    sub = sub.ravel()  
 #   sub = fig.add_subplot(221)
    for i in range(10):
        y_list = []
        y=[]
        ymin = []
        ymax= []
        for j in data_list:
          #  print(j[:,0])
            x = j[:,0]
            y_temp = j[:,i+1]          # plt_index gets its index when variable is chosen by user.
            y_list.append(y_temp)
        #    print(y_list)
            y_list = np.array(y_list).tolist()
            y = np.mean(y_list,axis=0)
          #  print(y)
            ymin = np.min(y_list,axis=0)
         #   print(ymin)
            ymax = np.max(y_list,axis=0) 
         #   print(ymax)
    
            regress = linregress(x,y)
            lin_m = regress.slope
            lin_b = regress.intercept
            lin_r = stats.pearsonr(x,y)
            
         #   plt.tick_params(which="minor",axis="y",direction="inout")
        
      #  sub[i].plt.grid()        
        sub[i].set_xticks(range(1966,2017,5))#,fontsize=8)
        sub[i].set_xticklabels(range(1966,2017,5),fontsize=9) 
        sub[i].locator_params(axis="y",tight=True,nbins=8)
        sub[i].tick_params(axis="y",labelsize=9)                  # Needs to be changed for every plot.
        sub[i].set_ylabel(yaxis)
        sub[i].set_title(label=months[i])
        sub[i].plot(x,y,color=c[0],label="Average",linewidth="2") # change to "Average"?
        if header != "MeanOfDay":
            sub[i].plot(x,lin_m*x+lin_b,color=c[0],label="Linear Regression",linewidth="2",linestyle="dashed")   # Regression of average
        sub[i].plot(x,ymin,color=c[1],label="Min",linewidth="2",linestyle="dashed")
        sub[i].plot(x,ymax,color=c[1],label="Max",linewidth="2",linestyle="dashed")
        
       # sub[i].set_xlabel(xaxis,fontsize=12)
   #     sub[i].set_xticks(range(1966,2017,5))
    #    sub[i].set_yticks(fontsize=10)
           # plt.plot(xnew,f2(xnew),color=c[0],label="Average",markevery=100) # change to "Average"?
        
    #    print(y_temp)    
    #    print(y_list)
    #    print(y)
    #    print(ymin)
    #    print(ymax) 
        #  sub = plt.legend(fontsize=10)
      #  fig.add_subplot(221)
    
    #    plt.savefig("U:\\Research\\GIS\\Maps\\figures\\%s.png" %(figname))  # Need to vary this formatting between graphics.
    plt.show()                    


    
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

def color_select():
    '''
    This function is called to generate colors for graphing.
    Color maps can be found here: 
    https://tableaufriction.blogspot.com/2012/11/finally-you-can-use-tableau-data-colors.html
    '''
    
    tableau20blind = [(0, 107, 164), (255, 128, 14), (171, 171, 171), (89, 89, 89),
             (95, 158, 209), (200, 82, 0), (137, 137, 137), (163, 200, 236),
             (255, 188, 121), (207, 207, 207)]
    return tableau20blind


def main():
    pd.set_option('display.max_columns', None)  
    pd.set_option('display.expand_frame_repr', False)
    pd.set_option('max_colwidth', -1)
    Blacklist=[2741,2740,2739,2738,2737,2736,2641,2640,2639,2638,2637,2636,2541,2540,2539,2538,2441,2440,2439,2438,2341,2340,2339,2241,2240,2141]       
    while True:
        filelist = []
        input_location = input("input (1) for individual files, (2) for concatenated files: ")
        if input_location == "1":
            selection = input("display row,column, all,region or end?: ")
            if selection == "end":
                break
            if selection == "all":
                CellList = total_file(Blacklist)
                region_select = 0
            elif selection == "row" or selection == "column":
                region_select = 0
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
#            var_prompt = input("All(1) or a Single Variable(2)?: ")
#            if var_prompt == 2:            
            header,plt_index = pick_var(split_line)             # Subplot changes should begin here.           
            
            if input3 == "MonthlyAverage.txt":
                Monthly_plot(filelist,header,plt_index,selection,region_select)
#                plt.show()
#                plt.clf()
            elif input3 == "76SnowDepth.txt":
                Plot76(filelist,header,plt_index,selection,region_select)
 #               plt.show()
  #              plt.clf()
            elif input3 == "DaySnowDepth.txt":                  # Going to attempt to find average and max/min to graph. Will implement if it works.
                Day_mean(filelist,header,plt_index,selection,region_select)
   #             plt.show()
#                plt.clf()
            elif input3 == "MeanSnowDepth.txt":
                SnowDepthMean(filelist,header,plt_index,split_line)
#                plt.show()
#                plt.clf()
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
