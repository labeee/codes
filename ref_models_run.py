
from multiprocessing import Pool  # possibilita processos multithread
import os

def run_ep(epw,caso,file):
    os.chdir(epw.split('_')[1])
    print(epw.split('_')[1])
    if 'vn' in file:
        mode = 'vn'
    else:
        mode = 'ac'
    os.system('energyplus -x -w ../epws/'+epw+' -p ref_'+caso+'_'+mode+'_'+epw.split('_')[1]+' -r ../'+file.split('.')[0]+'_'+caso+'.idf')
    os.chdir('..')

zb1a6 = ['BRA_GO_Itumbiara.867740_INMET.epw','BRA_MG_Uberlandia.867760_INMET.epw','BRA_PR_Curitiba.838420_INMET.epw',
'BRA_RJ_Duque.de.Caxias-Xerem.868770_INMET.epw','BRA_RS_Santa.Maria.839360_INMET.epw','BRA_SC_Florianopolis.838970_INMET.epw']

zb7e8 =['BRA_MA_Sao.Luis.817150_INMET.epw','BRA_TO_Palmas.866070_INMET.epw']

## MULTI
epws = zb1a6+zb7e8

for epw in epws:
    os.system('rm -r '+epw.split('_')[1])
    os.system('mkdir '+epw.split('_')[1])

f = open('files_list_multi.csv','r')

first = True
for line in f:
    if not first:
        caso = line.split(',')[0]
        file = line.split(',')[-1]
        print('\n'+file+' '+ caso +'\n')

        # if 'ZB1a6' in file:  # PARA UNI
        #     epws = zb1a6
        # else:
        #     epws = zb7e81

        num_cluster = len(epws)
    
        p = Pool(num_cluster)  # abre os multi processos

        # manda rodar a funcao process_outputs para os multiprocessos
        result_map = p.starmap(run_ep, zip(
            epws,
            [caso for i in range(len(epws))],
            [file for i in range(len(epws))]
        ))
    # p.close()
    # p.join()

    #     for epw in epws:

    #         print('\n'+epw+'\n')

    #         # os.system('energyplus -x -w epws/'+epw+' -p ref_'+caso+'_'+epw.split('_')[1]+' -r '+file.split('.')[0]+'_'+caso+'.idf')
    #         os.system('energyplus -x -w epws/'+epw+' -p ref_'+caso+'_'+epw.split('_')[1]+' -r '+file.split('.')[0]+'_'+caso+'.idf')
    #         # print('energyplus -x -w epws/'+epw+' -p ref_'+caso+'_'+epw.split('_')[1]+' -r '+file.split('.')[0]+'_'+caso+'.idf')
    else:
        first = False
