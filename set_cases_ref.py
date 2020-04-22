
import datetime
import glob
import json
import os
import pandas as pd

import dict_update
from idf_bundler import idf_bundler
#import sample_gen
import runep_subprocess
import output_processing
from unique import unique

update = dict_update.update
        
start_time = datetime.datetime.now()
date = start_time.strftime("%d-%m-%y_%H-%M")

# Globals
FOLDER = 'refs_uni_8'  # 
NUM_CLUSTERS = 1  # 2*int(os.cpu_count()/3)  # AQUI EH DEFINIFO O NUMERO DE CORES (RAYNER = 5)
NAME_STDRD = 'U'
EXTENSION = 'idf'
REMOVE_ALL_BUT = [EXTENSION, 'csv', 'err']
EPW_NAMES = [
#    'BRA_GO_Itumbiara.867740_INMET.epw','BRA_MG_Uberlandia.867760_INMET.epw','BRA_PR_Curitiba.838420_INMET.epw',  # 
#    'BRA_RJ_Duque.de.Caxias-Xerem.868770_INMET.epw','BRA_RS_Santa.Maria.839360_INMET.epw','BRA_SC_Florianopolis.838970_INMET.epw','BRA_TO_Palmas.866070_INMET.epw'
    'BRA_MA_Sao.Luis.817150_INMET.epw'
]
SUP_LIMITS = 'sup_limits.json'
OUTPUT_PROCESSED = 'outputs_'+FOLDER+'_'+date

# To choose what to run in the code
GEN_MODELS = False
RUN_MODELS = False
PROCESSESS_OUTPUT = True # False  #  
RUN_ALL = True  # defines GEN_MODELS, RUN_MODELS, PROCESSESS_OUTPUT = True

SLICES_FOLDER = 'slices/'
SUB_SLICES_FOLDER = 'Uni/'
SLICE_PATTERN = 'u_'

MAIN = sorted(glob.glob(SLICES_FOLDER+'main*'))  # ['slices/main_materials_fixed.txt', 'slices/main.txt']
VN_FILE = [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'afn.txt']
AC_FILE = [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'idealloads.txt']

PARAMETERS = {
    'geometria': sorted(glob.glob(SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'?_geom*')), 
    'azimute': sorted(glob.glob(SLICES_FOLDER+'rotation*'))
}

FIX_PARAMETERS = {
    'veneziana': [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'blind_off.txt'],
    'componente': [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'construction_ref8.txt'],
    'absortancia': [SLICES_FOLDER+'abs_60.txt'],
    'vidro': [SLICES_FOLDER+'glass_fs87.txt'],
    'open_fac': [SLICES_FOLDER+'afn_openingfactor045.txt']
}

DEPENDENT_PARAMETERS = {
        'sombreamento': [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'shade_000'],
    'paf': [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'fenes_17']
}

# Dependents

if RUN_ALL:
    GEN_MODELS = True
    RUN_MODELS = True
    PROCESSESS_OUTPUT = True

SIZE = len(PARAMETERS['geometria'])*len(PARAMETERS['azimute'])
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

col_names = ['geometria','azimute','case']
df = pd.DataFrame(columns=col_names)  # 'folder',
line = 0

for geom in PARAMETERS['geometria']:
    for azi in PARAMETERS['azimute']:
    
        case = name_length.format(line)

        output = (NAME_STDRD+'_{}'.format(case))
        df = df.append(pd.DataFrame([[geom,azi,case]],columns=col_names)) 
        
        somb = DEPENDENT_PARAMETERS['sombreamento'][0]+geom.split(sep)[-1][3:]
        fenes = DEPENDENT_PARAMETERS['paf'][0]+geom.split(sep)[-1][3:]
        inputs = [geom,azi,somb,fenes]+[FIX_PARAMETERS[slc][0] for slc in FIX_PARAMETERS.keys()] 
        if GEN_MODELS:
            # AC
            idf_bundler(inputs+MAIN+AC_FILE, output_name = FOLDER+sep+output+'_ac.'+EXTENSION)
                
            # VN
            idf_bundler(inputs+MAIN+VN_FILE, output_name = FOLDER+sep+output+'_vn.'+EXTENSION)
            
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
