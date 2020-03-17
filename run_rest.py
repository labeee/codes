from multiprocessing import Pool  # possibilita processos multithread
import os
import pandas as pd
import output_processing

FOLDER = 'sg_multi_ar'
OUTPUT_PROCESSED = 'outputs_sg_multi_ar_13-03-20_13-39' # 'outputs_multi3_31-01-20_18-45'  # 'outputs_'+FOLDER
SUP_LIMITS = 'sup_limits.json'
NUM_CLUSTERS = 1  # int(os.cpu_count()/3)
SIZE =  99  # 
NAME_STDRD = 'sgmar'
EXTENSION = 'idf'

name_length = '{:0'+str(len(str(SIZE)))+'.0f}'

def runep(line):
    epw = line['epw']
    case = line['case']  # name_length.format()
    
    if line['zone'] == 'AC ERROR':
        mode = 'ac'
    else:
        mode = 'vn'
        
    print('case: '+case+'    mode: '+mode+'    '+epw)
        
    file_name = NAME_STDRD+'_'+case+'_'+mode
    os.system('energyplus -x -w epws/'+epw+' -p '+FOLDER+'/'+epw+'/'+file_name+' -r '+FOLDER+'/'+file_name+'.'+EXTENSION)

df = pd.read_csv(FOLDER+'/'+OUTPUT_PROCESSED+'.csv')

df_vn = df[df['zone'] == 'VN ERROR']
df_ac = df[df['zone'] == 'AC ERROR']
df_csv = df[df['zone'] == 'CSV ERROR']

if len(df_csv) > 0:
    print('CSV ERROR!!!')

df_base = df_vn.append(df_ac, ignore_index = True)
df_base['case'] = df_base['case'].apply(name_length.format)

print('\nRUNNING SIMULATIONS\n')

p = Pool(NUM_CLUSTERS)  # abre os multi processos

# manda rodar a funcao process_outputs para os multiprocessos
result_map = p.starmap(runep, zip(
    [df_base.iloc[i] for i in range(len(df_base))]
))

os.chdir(FOLDER)

print('\nPROCESSING OUTPUT\n')
output_processing.main(df_base, SUP_LIMITS, OUTPUT_PROCESSED+'_rest',NUM_CLUSTERS,NAME_STDRD)

df_rest = pd.read_csv(OUTPUT_PROCESSED+'_rest.csv')

df_final = df[(df['zone'] != 'VN ERROR') & (df['zone'] != 'AC ERROR')]

df_final = df_final.append(df_rest, ignore_index = True)

df_final.to_csv(OUTPUT_PROCESSED+'_final.csv', index=False)
