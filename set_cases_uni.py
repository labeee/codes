
import datetime
import glob
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

# Globals
FOLDER = 'test_uni'  # 
SIZE =  10  # 
SOBOL = False  # True  # 
SAMPLE_NAME = 'sample_'+FOLDER+'_sobol_'+str(SOBOL)  # 
GEN_SAMPLE = False  # True  # 
SAMPLE_PARTS = (1,1)
NUM_CLUSTERS = 3  # int(os.cpu_count()/2)
NAME_STDRD = 'U'
EXTENSION = 'idf'
REMOVE_ALL_BUT = [EXTENSION, 'csv', 'err']
EPW_NAMES = [
    'BRA_GO_Itumbiara.867740_INMET.epw','BRA_MG_Uberlandia.867760_INMET.epw','BRA_PR_Curitiba.838420_INMET.epw',  # 
    'BRA_RJ_Duque.de.Caxias-Xerem.868770_INMET.epw','BRA_RS_Santa.Maria.839360_INMET.epw','BRA_SC_Florianopolis.838970_INMET.epw',
    'BRA_MA_Sao.Luis.817150_INMET.epw','BRA_TO_Palmas.866070_INMET.epw'
]
SUP_LIMITS = 'sup_limits.json'
OUTPUT_PROCESSED = 'outputs_'+FOLDER

# To choose what to run in the code
GEN_MODELS = False
RUN_MODELS = False
PROCESSESS_OUTPUT = True  # True
RUN_ALL = False  # defines GEN_MODELS, RUN_MODELS, PROCESSESS_OUTPUT = True

SLICES_FOLDER = 'slices/'
SUB_SLICES_FOLDER = 'Uni/'
SLICE_PATTERN = 'u_'

MAIN = glob.glob(SLICES_FOLDER+'main*')  # ['slices/main_materials_fixed.txt', 'slices/main.txt']
VN_FILE = [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'afn.txt']
AC_FILE = [SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'idealloads.txt']

SOMB_PATTERN = 'shade'
SOMB_SUFIX = '_000'

somb_pattern = SLICE_PATTERN+SOMB_PATTERN
sombreamento = glob.glob(SLICES_FOLDER+SUB_SLICES_FOLDER+somb_pattern+'*')
sombreamento = [somb[:len(SLICES_FOLDER+SUB_SLICES_FOLDER+somb_pattern+SOMB_SUFIX)] for somb in sombreamento]
sombreamento = unique(sombreamento)

FENES_PATTERN = 'fenes'
FENES_SUFIX = '_17'

fenes_pattern = SLICE_PATTERN+FENES_PATTERN
fenestration = glob.glob(SLICES_FOLDER+SUB_SLICES_FOLDER+fenes_pattern+'*')
fenestration = [fenes[:len(SLICES_FOLDER+SUB_SLICES_FOLDER+fenes_pattern+FENES_SUFIX)] for fenes in fenestration]
fenestration = unique(fenestration)

PARAMETERS = {
    'geometria': glob.glob(SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'?_geom*'),  # [SLICE+'Multi/m_geom_0_0_0.txt',SLICE+'Multi/m_geom_0_0_1.txt',SLICE+'Multi/m_geom_1_0_0.txt', SLICE+'Multi/m_geom_1_0_1.txt',SLICE+'Multi/m_geom_2_0_0.txt'],  # area, ratio, pe-direito, janelas

    'azimute': glob.glob(SLICES_FOLDER+'rotation*'),  # [SLICE+'rotation_000.txt',SLICE+'rotation_090.txt',SLICE+'rotation_180.txt',SLICE+'rotation_270.txt'],

    'veneziana': glob.glob(SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'blind*'),  # [SLICE+'Multi/m_blind_off.txt',SLICE+'Multi/m_blind_on.txt'],

    'componente':  glob.glob(SLICES_FOLDER+SUB_SLICES_FOLDER+SLICE_PATTERN+'construction*'),  # [SLICE+'Multi/m_construction_ref.txt',SLICE+'Multi/m_construction_sfiso.txt',SLICE+'Multi/m_construction_tijmacico20.txt',SLICE+'Multi/m_construction_tv.txt'],  # paredes, piso e coertura

    'absortancia': glob.glob(SLICES_FOLDER+'abs*'),  # [SLICE+'abs_60.txt',SLICE+'abs_20.txt',SLICE+'abs_80.txt'],  # paredes e cobertura

    'vidro': glob.glob(SLICES_FOLDER+'glass*'),  # [SLICE+'vidro_fs39.txt',SLICE+'vidro_fs87.txt',SLICE+'vidro_duplo-fs39-87.txt',SLICE+'vidro_duplo-fs87.txt']#,  # simples/duplo e FS
    
    'open_fac': glob.glob(SLICES_FOLDER+'afn_openingfactor*'),

    'sombreamento': sombreamento,  # [SLICE+'Multi/m_shade_050_geom_0_0_0.txt',SLICE+'Multi/m_shade_120_geom_0_0_0.txt'],

    'paf': fenestration  # []
}

# Dependents

if RUN_ALL:
    GEN_MODELS = True
    RUN_MODELS = True
    PROCESSESS_OUTPUT = True

col_names = list(PARAMETERS)
name_length = '{:0'+str(len(str(SIZE)))+'.0f}'

def parameter_file(key, i):
    n_files = len(PARAMETERS[key])
    file_name = PARAMETERS[key][int(n_files*i)]
    if key == 'sombreamento':
        file_name = file_name+parameter_file('geometria', i).split('/')[-1][3:]
    elif key == 'paf':
        file_name = file_name+parameter_file('geometria', i).split('/')[-1][3:]
        
    return file_name

print('\nCREATING DIRECTORIES\n')

os.system('mkdir '+FOLDER)
for epw in EPW_NAMES:
    os.system('mkdir '+FOLDER+'/'+epw)
    
# Generate sample
if GEN_SAMPLE:
    print('\nGENERATING SAMPLE\n')
    sample = sample_gen.main(SIZE, col_names, SAMPLE_NAME, sobol=SOBOL, scnd_order = False)
else:
    print('\nREADING SAMPLE\n')
    sample = pd.read_csv(SAMPLE_NAME+'.csv')
    
    n = len(sample)//SAMPLE_PARTS[1]
    sample_chuncks = [sample.iloc[i:i + n] for i in range(0, len(sample), n)]
    if SAMPLE_PARTS[0] == SAMPLE_PARTS[1] and len(sample)%SAMPLE_PARTS[1] != 0:
        sample = sample_chuncks[SAMPLE_PARTS[0]-1].append(sample_chuncks[SAMPLE_PARTS[0]], ignore_index = True).reset_index(drop=True)
    else:
        sample = sample_chuncks[SAMPLE_PARTS[0]-1].reset_index(drop=True)
        
print(sample)
if SOBOL:
    sample = (sample+1)/2

# Set cases
print('\nGENERATING MODELS\n')

df = pd.DataFrame(columns=col_names+['case'])  # 'folder',
line = 0

for i in range(len(sample)):
    
    sample_line = list(sample.iloc[i])
    
    model_values = dict((param,parameter_file(param, sample.loc[i, param])) for param in col_names)
    
    case = name_length.format(line)
    
    output = (NAME_STDRD+'_{}'.format(case))
    df = df.append(pd.DataFrame([sample_line+[case]],columns=col_names+['case']))  # 'cluster'+name_length_cluster.format(cluster_n),  # 'folder',
    # print(output)

    if GEN_MODELS:
        # AC
        idf_bundler([model_values[param] for param in model_values.keys()]+MAIN+AC_FILE, output_name = FOLDER+'/'+output+'_ac.'+EXTENSION)
            
        # VN
        idf_bundler([model_values[param] for param in model_values.keys()]+MAIN+VN_FILE, output_name = FOLDER+'/'+output+'_vn.'+EXTENSION)
        
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
