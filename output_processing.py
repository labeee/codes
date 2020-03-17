# Return EHF from multiple simulation results of Operative Temperature

## BIBLIOTECAS
import json
from multiprocessing import Pool  # possibilita processos multithread
import numpy as np
import os  # possibilita comandos no sistema operacional
import pandas as pd  # facilita trabalhar com dataframes
from unique import unique

OCCUP = ':Schedule Value [](Hourly)'  # ':People Occupant Count [](Hourly)'  #
# SCH_OCUP_DORM:Schedule Value [](Hourly) 
# ZONES = ['SALA','DORM1','DORM2']
INF_LIM = 18

# n_errors = 0

def schedule_name(zone, occup):
    return('SCH_OCUP_'+zone+occup)
 # 'SCH_OCUPACAO:Schedule Value [](Hourly)'

def name_file(case,pattern,mode,epw):
    # Define o nome do arquivo.

    file_name =  pattern+'_'+case+'_'+mode+'out.csv'  # '_'+epw+
    # file_name =  mode+'_'+epw+'.csv'
    return(file_name)


def process_outputs(line, sup_lim, pattern):
    # Le o output do energyplus para o caso especificado e calcula a
    # temperatura operativa média, temperatura operativa máxima, a
    # média das trocas de ar por hora (ACH), e o EHF.
    # Inputs:
    # line = a linha do dataframe que especifica o caso avaliado.
    # sup_lim = limite superior de temperatura para o calculo do PHFT
    # pattern = the pattern of the name being used
    # Output:
    # df_temp = dicionario com todas as informacoes desejadas
    
    # dicioniario (dataframe) criado para adicinar as informacoes
    df_temp = {
        # 'folder': [],
        # 'case': [],
        # 'file_pattern': [],
        'floor': [],
        'zone': [],
        # 'epw': [],
        'ph_inf': [],
        'phft': [],
        'ph_sup': [],
        't_min': [],
        't_max': [],
        'cgtr_cooling': [],
        'cgtr_heating': []
    }
    
    for col in df_temp.keys():
        try:
            del(line[col])
        except:
            pass    
    for col in line.index:
        df_temp[col] = []
        
    # define o nome do output do energyplus a partir do nome do idf
    vn_csv_file = name_file(line['case'],pattern,'vn',line['epw'])  # '{:03.0f}'.format(
    ac_csv_file = name_file(line['case'],pattern,'ac',line['epw'])
    
    # le o output como um dataframe, utilizando o pandas
    try:
        df_vn = pd.read_csv(line['epw']+'/'+vn_csv_file)
        df_ac = pd.read_csv(line['epw']+'/'+ac_csv_file)
        
        if len(df_ac) == len(df_vn):
            # printa o nome do arquivo para podermos acompanhar o processo
            print('case: ',line['case'],'    ',line['epw'])  # , end='\r')

            cols = df_ac.columns

            zones = []

            for col in cols:
                if not OCCUP in col:
                    if 'DORM' in col or 'SALA' in col:
                        zones.append(col.split(' ')[0])
                
            zones = unique(zones)

            for zn in zones:

                if len(zn.split('_')) > 1:
                    df_temp['floor'].append(zn.split('_')[0])
                else:
                    df_temp['floor'].append('unique')

                if 'SALA' in zn:
                    zn_occup = schedule_name('SALA',OCCUP)
                elif 'DORM' in zn:
                    zn_occup = schedule_name('DORM',OCCUP)

                # df_temp['folder'].append(line['folder'])
                # df_temp['case'].append(line['case'])  
                # df_temp['epw'].append(line['epw']) 
                 
                for col in line.index:
                    df_temp[col].append(line[col])
                    
                df_temp['zone'].append(zn) 

                try:
                    df_temp['t_min'].append((df_vn[zn+':Zone Operative Temperature [C](Hourly)'][df_vn[zn_occup] > 0]).min())
                except:
                    zn_occup = zn_occup+' '
                    df_temp['t_min'].append((df_vn[zn+':Zone Operative Temperature [C](Hourly)'][df_vn[zn_occup] > 0]).min())
                df_temp['t_max'].append((df_vn[zn+':Zone Operative Temperature [C](Hourly)'][df_vn[zn_occup] > 0]).max())

                df_vn['inf_lim'] = 0
                df_vn.loc[df_vn[zn+':Zone Operative Temperature [C](Hourly)'] < INF_LIM, 'inf_lim'] = 1
                df_temp['ph_inf'].append(df_vn['inf_lim'][df_vn[zn_occup] > 0].mean())

                df_vn['sup_lim'] = 0
                df_vn.loc[df_vn[zn+':Zone Operative Temperature [C](Hourly)'] >= sup_lim, 'sup_lim'] = 1
                df_temp['ph_sup'].append(df_vn['sup_lim'][df_vn[zn_occup] > 0].mean())

                df_temp['phft'].append(1 - df_temp['ph_inf'][-1] - df_temp['ph_sup'][-1])


                try:
                    df_temp['cgtr_heating'].append(df_ac[zn+' IDEAL LOADS AIR SYSTEM:Zone Ideal Loads Zone Total Heating Energy [J](Hourly)'][df_vn['inf_lim'] > 0].sum())
                except:
                    df_temp['cgtr_heating'].append(df_ac[zn+' IDEAL LOADS AIR SYSTEM:Zone Ideal Loads Zone Total Heating Energy [J](Hourly) '][df_vn['inf_lim'] > 0].sum())
                try:
                    df_temp['cgtr_cooling'].append(df_ac[zn+' IDEAL LOADS AIR SYSTEM:Zone Ideal Loads Zone Total Cooling Energy [J](Hourly) '][df_vn['sup_lim'] > 0].sum())
                except:
                    df_temp['cgtr_cooling'].append(df_ac[zn+' IDEAL LOADS AIR SYSTEM:Zone Ideal Loads Zone Total Cooling Energy [J](Hourly)'][df_vn['sup_lim'] > 0].sum())
        else:            
            print('ERROR!!!    CSV INCOMPLETE!!!    case: ',line['case'],' ',line['epw'])  # , end='\r')
            error = 'CSV ERROR' 
                 
            for col in line.index:
                df_temp[col].append(line[col])
            
            # df_temp['case'].append(line['case'])
            df_temp['floor'].append(error) 
            df_temp['zone'].append(error) 
            # df_temp['epw'].append(line['epw'])
            df_temp['ph_inf'].append(error) 
            df_temp['phft'].append(error) 
            df_temp['ph_sup'].append(error) 
            df_temp['t_min'].append(error) 
            df_temp['t_max'].append(error) 
            df_temp['cgtr_cooling'].append(error) 
            df_temp['cgtr_heating'].append(error) 

    except:
        try:
            df_vn = pd.read_csv(line['epw']+'/'+vn_csv_file)          
        
            # printa o nome do arquivo para podermos acompanhar o processo
            print('ERROR!!!    mode: AC    case: ',line['case'],' ',line['epw'])  # , end='\r')
            error = 'AC ERROR' 
                  
        except:
            try:
                df_ac = pd.read_csv(line['epw']+'/'+ac_csv_file) 
                # printa o nome do arquivo para podermos acompanhar o processo
                print('ERROR!!!    mode: VN    case: ',line['case'],' ',line['epw'])  # , end='\r') 
                error = 'VN ERROR' 
            except:
                # n_errors += 1
                error = 'CSV ERROR'
                 
        for col in line.index:
            df_temp[col].append(line[col])
        
        # df_temp['case'].append(line['case'])
        df_temp['floor'].append(error) 
        df_temp['zone'].append(error) 
        # df_temp['epw'].append(line['epw'])
        df_temp['ph_inf'].append(error) 
        df_temp['phft'].append(error) 
        df_temp['ph_sup'].append(error) 
        df_temp['t_min'].append(error) 
        df_temp['t_max'].append(error) 
        df_temp['cgtr_cooling'].append(error) 
        df_temp['cgtr_heating'].append(error) 
            
    return(df_temp)

def main(df_base, sup_limits, output_name, num_cluster, pattern):
    # Manda calcular os outputs desejados para cada caso do dataframe,
    # utilizando processos multiplos (multithread)
    # Inputs:
    # df_base = dataframe com as informações de todos os casos.
    # sup_lim = nome do arquivo com os limites superiores do PHFT.
    # output_name = nome com que o dataframe gerada sera salvo.
    
    # dicioniario (dataframe) criado para adicinar as informacoes
    df_final = {
        # 'folder': [],
        # 'case': [],
        # 'file_pattern': [],
        'floor': [],
        'zone': [],
        # 'epw': [],
        'ph_inf': [],
        'phft': [],
        'ph_sup': [],
        't_min': [],
        't_max': [],
        'cgtr_cooling': [],
        'cgtr_heating': []
    }
    
    for col in df_base.columns:
        df_final[col] = []
    
    # Month means ja foi calculado e eh lido como um dataframe, utilizando pandas (neste caso o clima eh sempre o mesmo)
    with open('../'+sup_limits, 'r') as file:            
        sup_limits = json.loads(file.read())

    # o numero de clusters eh defido a partir do numero de pastas (nao precisa ser assim)
    # num_cluster = len(df_base['folder'].unique())
    
    p = Pool(num_cluster)  # abre os multi processos

    # manda rodar a funcao process_outputs para os multiprocessos
    result_map = p.starmap(process_outputs, zip(
        # [df_base.query('folder == "cluster'+str(i)+'"') for i in range(num_cluster)],
        [df_base.iloc[i] for i in range(len(df_base))],
        [sup_limits[df_base['epw'][i]] for i in range(len(df_base))],
        [pattern for i in range(len(df_base))]
    ))
    p.close()
    p.join()

    # passa os resultados dos multi processos para o df_final
    for df_temp in result_map:
        # if len(df_temp['zone']) != len(df_temp['ph_inf']):
            # print(df_temp['case'][0],df_temp['epw'][0],len(df_temp['zone']),len(df_temp['ph_inf']))
        for key in df_final.keys():
            for i in range(len(df_temp[key])):
                df_final[key].append(df_temp[key][i])
    
    # salva o df_final como csv
    # for col in df_final.keys():
    #     print(col,len(df_final[col]))

    n_errors = df_final['zone'].count('AC ERROR') + df_final['zone'].count('VN ERROR') + df_final['zone'].count('CSV ERROR')
    print('\n\tNumber of simulations with errors: '+str(n_errors)+'\n')
    df_output = pd.DataFrame(df_final)
    df_output.to_csv(output_name+'.csv', index=False)
    print('\tDone processing!')

'''
phft = np.mean((l <26)*l/l)

Teste da funcao sem depender de outros codigos:

os.chdir('teste_outputprocess')

df_base = pd.read_csv('df_base.csv')
main(df_base, 'sup_limits.csv', 'multi_outputs')
'''
