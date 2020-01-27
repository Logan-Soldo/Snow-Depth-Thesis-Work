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
            list_files = ["76SnowDepth.txt","DaySnowDepth.txt","MeanSnowDepth.txt"
                          ,"MonthlyAverage.txt","SDQuality.txt","SeasonalSnowDepth.txt",
                          "MonthAboveX.txt"]
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
    xaxis = "test"
    yaxis = "test"
    savefig = input("Input a file name: ")
    return title,yaxis,xaxis,savefig


######## Below this are all of the plotting functions. ##########
def Set_Month(filelist,header,input3,selection,region):               # Use this example for global plotting application
    '''
    Introducing subplotting here as a demo.
    '''
    data_list = []
    for fname in filelist:
        try:
            data= np.loadtxt((open(fname).readlines()[:-1]), skiprows=1, dtype=None)  # Coming out of loop too early, need to build a list of lists.
            data_list.append(data)
        except IOError:
            continue

    title,yaxis,xaxis,figname = plt_select1(selection,header,region,data_list)
    if input3 != "MonthAboveX.txt":
        yaxis = "Sum of Snow\nDepth(cm)"
        xaxis = "Year"
    else:
        yaxis = "Days"
        xaxis = "Year"
    Month_plot(title,data_list,header,yaxis,xaxis,input3)
    return figname
    
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
    
    title,yaxis,xaxis,figname = plt_select1(x,y_list,selection,header,region)
    Gen_plot(x,y_list,selection,header,region)
    return figname

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
    
    title,yaxis,xaxis,figname = plt_select1(selection,header,region,y_list)
    yaxis = "Snow Depth(cm)"
    xaxis = "Day of Year"
    Gen_plot(x,y_list,selection,header,region,xaxis,yaxis,title)
    return figname
    
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
    

    plt.rc('font', family='serif')  
    plt.rc('xtick', labelsize='x-small')
    plt.rc('ytick', labelsize='x-small')

    return title,yaxis,xaxis,figname    
 
def Month_plot(title,data_list,header,yaxis,xaxis,input3):
    '''
    Used for plotting Monthly Data, use this as an example for future
    figures that will require subplots.
    '''
    fig,sub =  plt.subplots(3, 2, figsize=(12,8),constrained_layout=True)
#    plt.figure(figsize=(12,8)) 
    fig.suptitle(title,fontsize=12)                    # Plotting title from above.
    #fig.autoscale()
    
    c = color_select()
    for i in range(len(c)):
        r,g,b = c[i]
        c[i] = (r / 255., g / 255., b / 255.)
        
    months = ["January","February","March","April","November","December"]
    sub = sub.ravel()  
 #   sub = fig.add_subplot(221)

    for i in range(6):
        y_list = []
        y=[]
        ymin = []
        ymax= []
        for j in data_list:
            x = j[:,0]
            y_temp = j[:,i+1]          # plt_index gets its index when variable is chosen by user.
            y_list.append(y_temp)
            y_list = np.array(y_list).tolist()
            y = np.mean(y_list,axis=0)
            ymin = np.min(y_list,axis=0)
            ymax = np.max(y_list,axis=0)
            per_90 = np.percentile(y_list,90,axis=0)
            per_10 = np.percentile(y_list,10,axis=0)
    
            regress = linregress(x,y)
            lin_m = regress.slope
            lin_b = regress.intercept
#            lin_r = stats.pearsonr(x,y)
#            r_round = round(lin_r[0],3)
#            r2_round = round(lin_r[1],3)
        #    print(r_round)

#            m_round = round(m,3)            
        
        sub[i].set_xlim(1966,2017)
        if input3 != "MonthAboveX.txt":
            sub[i].set_ylim(bottom=0,top=max(ymax)+100)
        else:
            sub[i].set_ylim(bottom=0,top=32)
            

        sub[i].set_xticks(range(1966,2017,5))#,fontsize=8)
        sub[i].set_xticklabels(range(1966,2017,5),fontsize=10) 
        sub[i].locator_params(axis="y",tight=True,nbins=8)   # change nbins integer to reduce number of y ticks
        sub[i].tick_params(axis="y",labelsize=10)                  # Needs to be changed for every plot.
        sub[i].set_ylabel(yaxis,fontsize=10,multialignment="center")
      
        #  sub[i].labelsize('medium')
        sub[i].set_title(label=months[i])
        sub[i].scatter(x,ymax,s=10)
        sub[i].scatter(x,ymin,s=10)
        sub[i].plot(x,y,color=c[0],label="Average",linewidth="2") # change to "Average"?
        if header != "MeanOfDay":
            reg_plot = sub[i].plot(x,lin_m*x+lin_b,color=c[0],label="Linear Regression",linewidth="2",linestyle="dashed")   # Regression of average
    #    sub[i].plot(x,ymin,color=c[1],label="Min",linewidth="2",linestyle="dashed")
     #   sub[i].plot(x,ymax,color=c[1],label="Max",linewidth="2",linestyle="dashed")
        sub[i].fill_between(x, per_10, per_90, alpha=0.25, linewidth=0, color=c[0])
#        sub[i].plot(x,10_per,color=c[1],label="10Per",linewidth="2",linestyle="dashed")
#        sub[i].plot(x,90_per,color=c[1],label="90Per",linewidth="2",linestyle="dashed")
        sub[i].grid()
   #     sub[i].legend(r_round,reg_plot)

def Gen_plot(x,y_list,selection,header,region,xaxis,yaxis,title):
    y=[]
    fname_lst = []
    
    y_list = np.array(y_list).tolist()
    y = np.mean(y_list,axis=0)
    ymin = np.min(y_list,axis=0)
    ymax = np.max(y_list,axis=0)
    per_90 = np.percentile(y_list,90,axis=0)
    per_10 = np.percentile(y_list,10,axis=0)
    
    regress = linregress(x,y)
    lin_m = regress.slope
    lin_b = regress.intercept
    lin_r = stats.pearsonr(x,y)
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
    
    plt.figure(figsize=(12,8))    
    plt.title(title,fontsize=12)                    # Plotting title from above
    plt.grid()
 #   plt.tick_params(which="minor",axis="y",direction="inout")
    plt.ylabel(yaxis,fontsize=12)                  # Needs to be changed for every plot.
    plt.xlabel(xaxis,fontsize=12)
    plt.xticks(range(1,365,10),fontsize=10,rotation=45) 
    plt.xlim(1,365)
    plt.ylim(0,max(ymax)+5)
#    plt.xticks(range(1966,2017,5),fontsize=10)
    plt.yticks(fontsize=10)
   # plt.plot(xnew,f2(xnew),color=c[0],label="Average",markevery=100) # change to "Average"?
    
    plt.plot(x,y,color=c[0],label="Average",linewidth="2",markevery=100) # change to "Average"?
    if header != "MeanOfDay":
        plt.plot(x,lin_m*x+lin_b,color=c[0],label="Linear Regression",linewidth="2",linestyle="dashed")   # Regression of average
    plt.fill_between(x, per_10, per_90, alpha=0.25, linewidth=0, color=c[0])
#    plt.plot(x,ymin,color=c[1],label="Min",linewidth="2",linestyle="dashed")
#    plt.plot(x,ymax,color=c[1],label="Max",linewidth="2",linestyle="dashed")
    plt.legend(fontsize=10)
        

def savefig(figname):    
    plt.savefig("U:\\Research\\GIS\\Maps\\figures\\%s.pdf" %(figname))  # Need to vary this formatting between graphics.
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
            
            if input3 == "MonthlyAverage.txt":
                figname = Set_Month(filelist,header,input3,selection,region_select)                
                savefig(figname)
                
#                plt.show()
#                plt.clf()
            elif input3 == "76SnowDepth.txt":
                header,plt_index = pick_var(split_line)             # Subplot changes should begin here.           
                figname = Plot76(filelist,header,plt_index,selection,region_select)
                savefig(figname)

 #               plt.show()
  #              plt.clf()
            elif input3 == "DaySnowDepth.txt":  # Goal is to get 4 subplots for this file with a plot for each region.
                header,plt_index = pick_var(split_line)             # Subplot changes should begin here.                         
                figname = Day_mean(filelist,header,plt_index,selection,region_select)
                savefig(figname)

   #             plt.show()
#                plt.clf()
            elif input3 == "MeanSnowDepth.txt":
                header,plt_index = pick_var(split_line)             # Subplot changes should begin here.                           
                figname = SnowDepthMean(filelist,header,plt_index,split_line)
                savefig(figname)
            elif input3== "MonthAboveX.txt":
                figname = Set_Month(filelist,header,input3,selection,region_select)                
                savefig(figname)                

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
