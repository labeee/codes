# define o ambiente de trabalho
import math
import json

# carrega o arquivo json que contém os componentes construtivos
with open('/home/rodox/git/nbr/constructions.json') as arquivo:
    cons = json.load(arquivo)

# cria um dicionário com as transmitâncias e capacidades térmicas de cada componente construtivo
saida = {c: {'paredes_externas': {}, 'cobertura': {}} for c in cons['componentes_construtivos'].keys()}

# base para os cálculos
    # resistência térmica = espessura / condutividade térmica
    # resistência térmica total = resistência superficial externa + somatório das resistências térmicas das camadas + resistência superficial interna
    # transmitância térmica = 1 / resistência térmica total
    # capacidade térmica = espessura * calor específico * densidade
    # capacidade térmica total = somatório das capacidades térmicas das camadas
# variáveis
    # espessura
    # condutividade térmica
    # calor específico
    # densidade

comps = cons['componentes_construtivos']
mats = cons['materiais']
res = cons['resistencias']
isos = cons['isolantes']
for ck, cv in comps.items():
    for sk, sv in cv.items():
        props = [mats[i] for i in sv.values() if i in mats.keys()]
        rt = 0
        rt = sum([isos[i] for i in sv.values() if i in isos.keys()])
        rt += sum([i for i in list(res[sk].values())])
        ct = 0
        for p in props:
            ct += p['espessura']*p['calor_especifico']*p['densidade']
            rt += p['espessura']/p['condutividade_termica']
        saida[ck][sk].update({'transmitancia': round(1/rt, 2)})
        saida[ck][sk].update({'capacidade_termica': round(ct)})

# cria um novo arquivo json com os dados de saída (transmitâncias e capacidades térmicas)
with open('/home/rodox/git/nbr/properties.json', 'w') as arquivo:
    json.dump(saida, arquivo, indent = 4)
