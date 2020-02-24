import pandas as pd


meta = pd.read_table("U:\Research\ElevationData\Rutgers-dem.all.txt",header=0,usecols=[0,1,2,3,4,5,6,7,8,9,10])

def Define_ij():
    CellList = []
    i_list = []
    j_list = []
    Blacklist=[2741,2740,2739,2738,2737,2736,2641,2640,2639,2638,2637,2636,2541,2540,2539,2538,2441,2440,2439,2438,2341,2340,2339,2241,2240,2141]       

    for i in range(12,28):
        for j in range(33,42):
            if (str(i)+str(j)) in str(Blacklist):
                continue
            else:
                CellList.append(str(i)+str(j))
                i_list.append(str(i))
                j_list.append(str(j))
    return CellList,i_list,j_list

ij,i_list,j_list = Define_ij()

meta["ij"] = meta['j_row'].astype(str) + meta['i_col'].astype(str)
meta = meta.fillna(0)
meta = meta.astype({'ij': 'int64'})
print(meta.dtypes)
sort_meta = pd.DataFrame([])
sort_meta = meta[meta['ij'].isin(ij)]
sort_meta.reset_index(drop=True)
print(sort_meta)

sort_meta.to_csv(path_or_buf='U:\Research\ElevationData\meta.csv')