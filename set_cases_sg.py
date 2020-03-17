
import datetime
import glob
import itertools
import json
import os
import pandas as pd

import dict_update
from idf_bundler import idf_bundler
import sample_gen
import runep_subprocess
import output_processing
from unique import unique

update = dict_update.update
        
start_time = datetime.datetime.now()
date = start_time.strftime("%d-%m-%y_%H-%M")

# Globals
FOLDER = 'sg_uni'  # 'sg_uni_80'  # 
NUM_CLUSTERS = 10  #int(os.cpu_count())  # DAQUI EH DEFINIFO O NUMERO E CORES
NAME_STDRD = 'sg'  # 'sg80'  # 
EXTENSION = 'idf'
REMOVE_ALL_BUT = [EXTENSION, 'csv', 'err']
EPW_NAMES = [
    'BRA_PR_Curitiba.838420_INMET.epw',  # 
    'BRA_RJ_Duque.de.Caxias-Xerem.868770_INMET.epw',
    'BRA_SC_Florianopolis.838970_INMET.epw',
    'BRA_TO_Palmas.866070_INMET.epw'
]
SUP_LIMITS = 'sup_limits.json'
OUTPUT_PROCESSED = 'outputs_'+FOLDER+'_'+date

# To choose what to run in the code
GEN_MODELS = False
RUN_MODELS = False
PROCESSESS_OUTPUT = True  # True
RUN_ALL = False  # defines GEN_MODELS, RUN_MODELS, PROCESSESS_OUTPUT = True

SLICES_FOLDER = 'slices/'
SUB_SLICES_FOLDER = 'Uni/'  # 'Multi/'  # 'Ulsni/'  # 
SLICE_PATTERN = 'u_'  # 'u_'  # 

MAIN = sorted(glob.glob(SLICES_FOLDER+'main*'))  # ['slices/main_materials_fixed.txt', 'slices/main.txt']
VN_FILE = [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'afn.txt']
AC_FILE = [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'idealloads.txt']

PARAMETERS = {
    'geometria': [SUB_SLICES_FOLDER+SLICE_PATTERN+'g_geom_0_0_0.txt'],  # 'u_g_geom_0_0_0.txt'],  # 

    'azimute': ['rotation_000.txt'],

    'veneziana': [SUB_SLICES_FOLDER+SLICE_PATTERN+'blind_on.txt'],  # 'u_blind_on.txt'],  # 

    'componente':  [SUB_SLICES_FOLDER+SLICE_PATTERN+'construction_sfiso.txt'],  # 'u_construction_sfiso.txt'],  # 

    'absortancia': ['abs_20.txt','abs_60.txt'],  # ,'abs_80.txt'

    'vidro': ['glass_double-fs87.txt', 'glass_fs39.txt', 'glass_fs87.txt','glass_double-fs39-87.txt'],
    
    'open_fac': ['afn_openingfactor100.txt'],

    'sombreamento': [SUB_SLICES_FOLDER+SLICE_PATTERN+'shade_000_geom_0_0_0.txt',SUB_SLICES_FOLDER+'u_shade_050_geom_0_0_0.txt',SUB_SLICES_FOLDER+'u_shade_150_geom_0_0_0.txt'],  # ,SUB_SLICES_FOLDER+'m_shade_120_geom_0_0_0.txt'],  # 'u_shade_000_geom_0_0_0.txt'

    'paf': [SUB_SLICES_FOLDER+SLICE_PATTERN+'fenes_17_geom_0_0_0.txt',SUB_SLICES_FOLDER+SLICE_PATTERN+'wwr_50_geom_0_0_0.txt']  # 'u_fenes_17_geom_0_0_0.txt',SUB_SLICES_FOLDER+'u_wwr_50_geom_0_0_0.txt']  # 
}


SIZE = 1
for k in PARAMETERS.keys():
    SIZE *= len(PARAMETERS[k])

# Dependents

if RUN_ALL:
    GEN_MODELS = True
    RUN_MODELS = True
    PROCESSESS_OUTPUT = True

col_names = list(PARAMETERS)
name_length = '{:0'+str(len(str(SIZE)))+'.0f}'

if os.name == 'posix':
    sep = '/'
else:
    sep = '\\'

print('\nCREATING DIRECTORIES\n')

os.system('mkdir '+FOLDER)
for epw in EPW_NAMES:
    os.system('mkdir '+FOLDER+sep+epw)
 
# Set cases
if GEN_MODELS:
    print('\nGENERATING MODELS\n')

df = pd.DataFrame(columns=col_names+['case'])  # 'folder',

cases_list = [PARAMETERS[k] for k in PARAMETERS.keys()]
cases_list = list(itertools.product(*cases_list))
cases_list = [list(x) for x in cases_list]

line = 0

for i in range(len(cases_list)):
    
    model_values = cases_list[line]
    
    case = name_length.format(line)
    
    output = (NAME_STDRD+'_{}'.format(case))
    df = df.append(pd.DataFrame([[model_values[param].split('/')[-1] for param in range(len(model_values))]+[case]],columns=col_names+['case']))  # 'cluster'+name_length_cluster.format(cluster_n),  # 'folder',
    # print(output)

    if GEN_MODELS:
        # AC
        idf_bundler([SLICES_FOLDER+model_values[param] for param in range(len(model_values))]+MAIN+AC_FILE, output_name = FOLDER+sep+output+'_ac.'+EXTENSION)
            
        # VN
        idf_bundler([SLICES_FOLDER+model_values[param] for param in range(len(model_values))]+MAIN+VN_FILE, output_name = FOLDER+sep+output+'_vn.'+EXTENSION)
        
    line += 1

df_base = pd.DataFrame()
for epw in EPW_NAMES:
    df['epw'] = epw
    df_base = df_base.append(df, ignore_index = True)

os.chdir(FOLDER)
if RUN_MODELS:
    print('\nRUNNING SIMULATIONS\n')
    runep_subprocess.main(NUM_CLUSTERS, EXTENSION, REMOVE_ALL_BUT, epw_names=EPW_NAMES)  # list_epjson_names,

if PROCESSESS_OUTPUT:
    print('\nPROCESSING OUTPUT\n')
    output_processing.main(df_base, SUP_LIMITS, OUTPUT_PROCESSED,NUM_CLUSTERS,NAME_STDRD)

end_time = datetime.datetime.now()
total_time = (end_time - start_time)
print("Total processing time: " + str(total_time))
