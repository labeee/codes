library(ggplot2)
library(reshape2)

setwd('~/codes') #Marcelo
# setwd("D:/Projetos/Saint-Gobain_2019/Analises/BaseDados_Multi")  #Amanda
#

strip = function(string, split_char='_', position=4){
  # para identificar caracter??sticas de geometria a partir do nome do slice
  as.character(strsplit(as.character(string),split_char)[[1]][position])
}

df = read.csv('outputs_uni_uh.csv')   #Uni
# df <- read.csv("outputs_multi_uh.csv")  #Multi

#converte J para kWh
df[, c("cgtr_cooling","cgtr_heating","cgtr_cooling_ref","cgtr_heating_ref")] <- df[, c("cgtr_cooling","cgtr_heating","cgtr_cooling_ref","cgtr_heating_ref")]/3600000

# diferencas entre caso e ref
df$phft_dif = df$phft - df$phft_ref
df$cgtr_dif = df$cgtr_cooling + df$cgtr_heating - df$cgtr_cooling_ref - df$cgtr_heating_ref
df$tmax_dif = df$t_max - df$t_max_ref
df$tmin_dif = df$t_min - df$t_min_ref

#area da uh base
area_uh = 38.58 #Uni
# area_uh_canto <- 34.72 #Multi canto
# area_uh_meio <- 33.81  #Multi meio


# identifica caracteristicas a partir do nome do slice
df$ratio = sapply(df$geometria, strip, position = 4)  # strsplit(as.character(df$geometria),'_')[4]
df$height = sapply(df$geometria, strip, position = 5)  # strsplit(as.character(df$geometria),'_')[[1]][5]
df$area = sapply(df$geometria, strip, position = 6)  # strsplit(as.character(df$geometria),'_')[[1]][6]

#Uni
df$area = ifelse(
  substr(df$area,1,1) == '0', area_uh, ifelse(
    substr(df$area,1,1) == '1', 1.5*area_uh,
    2*area_uh
  )
)

#Multi
# df$area = ifelse(
#   substr(df$area,1,1) == '0' & df$uh_expo == "CANTO", 1*area_uh_canto, ifelse(
#     substr(df$area,1,1) == '1' & df$uh_expo == "CANTO", 1.5*area_uh_canto, ifelse(
#       substr(df$area,1,1) == '0' & df$uh_expo == "MEIO", 1*area_uh_meio, 
#       1.5*area_uh_meio
#     )))

df$cgtr_dif = df$cgtr_dif/df$area
df$somb = sapply(df$sombreamento, strip, position = 3)  # strsplit(as.character(df$sombreamento),'_')[[1]][3]
df$wwr = sapply(df$paf, strip, position = 3)  # strsplit(as.character(df$paf),'_')[[1]][3]

# separe por estado
df_SC = df[df$estado == 'SC',]
df_PR = df[df$estado == 'PR',]
df_RS = df[df$estado == 'RS',]
df_GO = df[df$estado == 'GO',]
df_MA = df[df$estado == 'MA',]
df_MG = df[df$estado == 'MG',]
df_RJ = df[df$estado == 'RJ',]
df_TO = df[df$estado == 'TO',]

# #Cria df por clima, filtrando os resultados de uma uh especifica para analise (canto 12 na cobertura)
# df_SC = df[df$estado == 'SC' & df$floor == "CO" & df$uh_expo == "CANTO"  & df$uh_position == 12,]
# df_PR = df[df$estado == 'PR' & df$floor == "CO" & df$uh_expo == "CANTO" & df$uh_position == 12,]
# df_RS = df[df$estado == 'RS' & df$floor == "CO" & df$uh_expo == "CANTO" & df$uh_position == 12,]
# df_GO = df[df$estado == 'GO' & df$floor == "CO" & df$uh_expo == "CANTO" & df$uh_position == 12,]
# df_MA = df[df$estado == 'MA' & df$floor == "CO" & df$uh_expo == "CANTO" & df$uh_position == 12,]
# df_MG = df[df$estado == 'MG' & df$floor == "CO" & df$uh_expo == "CANTO" & df$uh_position == 12,]
# df_RJ = df[df$estado == 'RJ' & df$floor == "CO" & df$uh_expo == "CANTO" & df$uh_position == 12,]
# df_TO = df[df$estado == 'TO' & df$floor == "CO" & df$uh_expo == "CANTO" & df$uh_position == 12,]

#Grafico para exploracao dos resultados
ggplot(data=df_PR, aes(x=cgtr_dif, y=phft_dif))+
  geom_point()+
  geom_text(aes(label=case))

#Cria df com as maiores diferencas de phft, em relacao a referencia
heroes_phft <- rbind(df_SC[df_SC$phft_dif == max(df_SC$phft_dif),], 
                     df_PR[df_PR$phft_dif == max(df_PR$phft_dif),],
                     df_RS[df_RS$phft_dif == max(df_RS$phft_dif),],
                     df_GO[df_GO$phft_dif == max(df_GO$phft_dif),],
                     df_MA[df_MA$phft_dif == max(df_MA$phft_dif),],
                     df_MG[df_MG$phft_dif == max(df_MG$phft_dif),],
                     df_RJ[df_RJ$phft_dif == max(df_RJ$phft_dif),],
                     df_TO[df_TO$phft_dif == max(df_TO$phft_dif),])

#Cria df com as maiores diferencas de carga termica, em relacao a referencia
heroes_cgtt <- rbind(df_SC[df_SC$cgtr_dif == min(df_SC$cgtr_dif),],
                     df_PR[df_PR$cgtr_dif == min(df_PR$cgtr_dif),],
                     df_RS[df_RS$cgtr_dif == min(df_RS$cgtr_dif),],
                     df_GO[df_GO$cgtr_dif == min(df_GO$cgtr_dif),],
                     df_MA[df_MA$cgtr_dif == min(df_MA$cgtr_dif),],
                     df_MG[df_MG$cgtr_dif == min(df_MG$cgtr_dif),],
                     df_RJ[df_RJ$cgtr_dif == min(df_RJ$cgtr_dif),],
                     df_TO[df_TO$cgtr_dif == min(df_TO$cgtr_dif),])

# GRAFICOS ----

df_graph <- heroes_phft[, c("case","estado","ph_inf","ph_sup","phft","ph_inf_ref","ph_sup_ref","phft_ref","cgtr_cooling","cgtr_heating","cgtr_cooling_ref","cgtr_heating_ref")]
df_graph <- melt(df_graph, id=c("case","estado"))
df_graph$modelo <- NA
df_graph$var <- NA

for(i in 1:nrow(df_graph)){
  if(grepl("ref",df_graph[i, "variable"]) == TRUE){
    df_graph[i, "modelo"] <- "ref"
  }else{
    df_graph[i, "modelo"] <- "real"
  }
  if(grepl("phft",df_graph[i, "variable"]) == TRUE){
    df_graph[i, "var"] <- "phft"
  }
  if(grepl("ph_inf",df_graph[i, "variable"]) == TRUE){
    df_graph[i, "var"] <- "ph_inf"
  }
  if(grepl("ph_sup",df_graph[i, "variable"]) == TRUE){
    df_graph[i, "var"] <- "ph_sup"
  }
  if(grepl("cgtr_cooling",df_graph[i, "variable"]) == TRUE){
    df_graph[i, "var"] <- "cgtr_cooling"
  }
  if(grepl("cgtr_heating",df_graph[i, "variable"]) == TRUE){
    df_graph[i, "var"] <- "cgtr_heating"
  }
}

Labels_tipo <- c(PR = "ZB 1", RS = "ZB 2", SC = "ZB 3", MG = "ZB 4", RJ = "ZB 5",
                 GO ="ZB 6", TO ="ZB 7", MA ="ZB 8") #Muda labels do facet_grid

df_graph$var <- factor(df_graph$var, levels=c("ph_sup","ph_inf","phft","cgtr_heating","cgtr_cooling"))
df_graph$modelo <- factor(df_graph$modelo, levels=c("ref","real"))
df_graph$estado <- factor(df_graph$estado, levels=c("PR","RS","SC","MG","RJ","GO","TO","MA"))

# setwd("D:/Projetos/Saint-Gobain_2019/Analises/Graphs")  #Amanda


# ZB 1 (PR) ----
#PHFT
png(filename = 'PHFT_heroes_ZB1_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="phft"|var=="ph_inf"|var=="ph_sup") & 
                          (estado=="PR")), 
            aes(x=modelo, y=value*100, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value*100, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=4.5,                                                                                    #Tamanho do texto
                 position = position_stack(vjust = 0.5))+
       # geom_text(aes(label=case), position=position_identity())+
       coord_cartesian(ylim=(0:100))+                                                                        #Limites do eixo y
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Percentual de Horas ocupadas na Faixa \nde Temperatura operativa (%)")+
       scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem
                         values=c("#FFA05C",
                                  "#A8DDFF",
                                  "#96D5C0"),
                         labels=c("\u2265 26?C ou 28?C", "\u2264 18?C", "18?C < T < 26?C ou 28?C"))+                        #Nome dos itens da legenda
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()

#Grafico carga termica
png(filename = 'CgTT_heroes_ZB1_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="cgtr_cooling"|var=="cgtr_heating") &
                          (estado=="PR")), 
            aes(x=modelo, y=value, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=3.0, angle=90,                                                                                   #Tamanho do texto
                 position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Carga t?rmica anual (kWh/m?)")+
       scale_alpha_manual(values = c(0,0))+                                                               #Transparencia do texto
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
                         values=c("red","deepskyblue3"),
                         labels=c("Aquecimento","Refrigera??o"))+                        #Nome dos itens da legenda                              
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()



# ZBs 2 e 3 (RS e SC) ----
#PHFT
png(filename = 'PHFT_heroes_ZB2-3_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="phft"|var=="ph_inf"|var=="ph_sup") & 
                          (estado=="RS"|estado=="SC")), 
            aes(x=modelo, y=value*100, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value*100, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=4.5,                                                                                    #Tamanho do texto
                 position = position_stack(vjust = 0.5))+
       # geom_text(aes(label=case), position=position_identity())+
       coord_cartesian(ylim=(0:100))+                                                                        #Limites do eixo y
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Percentual de Horas ocupadas na Faixa \nde Temperatura operativa (%)")+
       scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem
                         values=c("#FFA05C",
                                  "#A8DDFF",
                                  "#96D5C0"),
                         labels=c("\u2265 26?C ou 28?C", "\u2264 18?C", "18?C < T < 26?C ou 28?C"))+                        #Nome dos itens da legenda
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()

#Grafico carga termica
png(filename = 'CgTT_heroes_ZB2-3_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="cgtr_cooling"|var=="cgtr_heating") &
                          (estado=="RS"|estado=="SC")), 
            aes(x=modelo, y=value, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=3.0, angle=90,                                                                                   #Tamanho do texto
                 position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Carga t?rmica anual (kWh/m?)")+
       scale_alpha_manual(values = c(0,0))+                                                               #Transparencia do texto
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
                         values=c("red","deepskyblue3"),
                         labels=c("Aquecimento","Refrigera??o"))+                        #Nome dos itens da legenda                              
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()

# ZBs 5 e 6 (RJ e GO) ----
#PHFT
png(filename = 'PHFT_heroes_ZB5-6_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="phft"|var=="ph_inf"|var=="ph_sup") & 
                          (estado=="GO"|estado=="RJ")), 
            aes(x=modelo, y=value*100, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value*100, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=4.5,                                                                                    #Tamanho do texto
                 position = position_stack(vjust = 0.5))+
       # geom_text(aes(label=case), position=position_identity())+
       coord_cartesian(ylim=(0:100))+                                                                        #Limites do eixo y
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Percentual de Horas ocupadas na Faixa \nde Temperatura operativa (%)")+
       scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem
                         values=c("#FFA05C",
                                  "#A8DDFF",
                                  "#96D5C0"),
                         labels=c("\u2265 26?C ou 28?C", "\u2264 18?C", "18?C < T < 26?C ou 28?C"))+                        #Nome dos itens da legenda
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()

#Grafico carga termica
png(filename = 'CgTT_heroes_ZB5-6_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="cgtr_cooling"|var=="cgtr_heating") &
                          (estado=="GO"|estado=="RJ")), 
            aes(x=modelo, y=value, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=3.0, angle=90,                                                                                   #Tamanho do texto
                 position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Carga t?rmica anual (kWh/m?)")+
       scale_alpha_manual(values = c(0,0))+                                                               #Transparencia do texto
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
                         values=c("red","deepskyblue3"),
                         labels=c("Aquecimento","Refrigera??o"))+                        #Nome dos itens da legenda                              
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()


# ZBs 4 e 7 (MG e TO) ----
#PHFT
png(filename = 'PHFT_heroes_ZB4e7_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="phft"|var=="ph_inf"|var=="ph_sup") & 
                          (estado=="MG"|estado=="TO")), 
            aes(x=modelo, y=value*100, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value*100, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=4.5,                                                                                    #Tamanho do texto
                 position = position_stack(vjust = 0.5))+
       # geom_text(aes(label=case), position=position_identity())+
       coord_cartesian(ylim=(0:100))+                                                                        #Limites do eixo y
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Percentual de Horas ocupadas na Faixa \nde Temperatura operativa (%)")+
       scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem
                         values=c("#FFA05C",
                                  "#A8DDFF",
                                  "#96D5C0"),
                         labels=c("\u2265 26?C ou 28?C", "\u2264 18?C", "18?C < T < 26?C ou 28?C"))+                        #Nome dos itens da legenda
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()

#Grafico carga termica
png(filename = 'CgTT_heroes_ZB4e7_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="cgtr_cooling"|var=="cgtr_heating") &
                          (estado=="MG"|estado=="TO")), 
            aes(x=modelo, y=value, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=3.0, angle=90,                                                                                   #Tamanho do texto
                 position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Carga t?rmica anual (kWh/m?)")+
       scale_alpha_manual(values = c(0,0))+                                                               #Transparencia do texto
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
                         values=c("red","deepskyblue3"),
                         labels=c("Aquecimento","Refrigera??o"))+                        #Nome dos itens da legenda                              
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()


# ZB 8 (MA) ----
#PHFT
png(filename = 'PHFT_heroes_ZB8_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="phft"|var=="ph_inf"|var=="ph_sup") & 
                          (estado=="MA")), 
            aes(x=modelo, y=value*100, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value*100, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=4.5,                                                                                    #Tamanho do texto
                 position = position_stack(vjust = 0.5))+
       # geom_text(aes(label=case), position=position_identity())+
       coord_cartesian(ylim=(0:100))+                                                                        #Limites do eixo y
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Percentual de Horas ocupadas na Faixa \nde Temperatura operativa (%)")+
       scale_alpha_manual(values = c(0, 0, 1))+                                                               #Transparencia do texto - apenas PHOCT nao eh transparente
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem
                         values=c("#FFA05C",
                                  "#A8DDFF",
                                  "#96D5C0"),
                         labels=c("\u2265 26?C ou 28?C", "\u2264 18?C", "18?C < T < 26?C ou 28?C"))+                        #Nome dos itens da legenda
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()

#Grafico carga termica
png(filename = 'CgTT_heroes_ZB8_uni.png',                                                           #MUDAR - Nome da figura
    width = 10, height = 15, units = "cm", res = 500)                                                         #Dimensoes da figura
plot(ggplot(data=subset(df_graph, (var=="cgtr_cooling"|var=="cgtr_heating") &
                          (estado=="MA")), 
            aes(x=modelo, y=value, fill=var))+                                                            #MUDAR - Nome do dataframe e variaveis
       geom_bar(stat="identity")+                                                                             #Grafico de barras
       geom_text(aes(label=(round(value, digits=0)),                                                          #Plota valores do PHOCT
                     alpha=var),                                                                         #Transparencia do texto conforme variavel
                 color="black",                                                                               #Cor do texto
                 size=3.0, angle=90,                                                                                   #Tamanho do texto
                 position = position_stack(vjust = 0.5))+                                                     #Texto no meio da barra
       labs(x="",                                                                                            #MUDAR - Titulo do eixo x
            y="Carga t?rmica anual (kWh/m?)")+
       scale_alpha_manual(values = c(0,0))+                                                               #Transparencia do texto
       scale_fill_manual("",                                                                                  #Titulo da legenda de cores - nao tem 
                         values=c("red","deepskyblue3"),
                         labels=c("Aquecimento","Refrigera??o"))+                        #Nome dos itens da legenda                              
       scale_x_discrete(labels=c("Ref.", "Real"))+
       scale_y_continuous(labels=function(x) format(x, big.mark = ".", scientific = FALSE))+                  #Adiciona divisao de milhar
       guides(alpha=FALSE)+                                                                                   #Remove legenda para transparencias
       theme(legend.position= "bottom",                                                                       #Posicao da legenda
             legend.text = element_text(size=11.5),                                                             #Tamanho do texto da legenda
             plot.title = element_text(size=15),                                                              #Tamanho do titulo do grafico
             plot.subtitle = element_text(size=13),                                                           #Tamanho do subtitulo do grafico
             axis.title.x = element_blank(),                                                                  #Tamanho do titulo do eixo x
             axis.title.y = element_text(size=14),                                                            #Tamanho do titulo do eixo y
             axis.text.x = element_text(size=14, vjust = 0.5, hjust=0.95),                                                #Tamanho do texto do eixo x, rotacionado 90 graus, alinhado com o centro da barra
             axis.text.y = element_text(size=14),                                                             #Tamanho do texto do eixo y
             strip.text.x = element_text(size = 16),                                                          #Tamanho do texto do facet grid no eixo x
             strip.text.y = element_text(size = 16))+                                                         #Tamanho do texto do facet grid no eixo y
       facet_grid(.~estado,                                                                                     #Cria divisao do grafico entre casos e cidades
                  labeller=labeller(estado = Labels_tipo))                                                      #Corrige nomes das divisoes do grafico - tem que rodar o Labels_tipo
)
dev.off()




############################# RESTO CODIGO MARCELO ----







# pega os 5% melhores de cada estado
df_heroes = data.frame()

for(uf in unique(df$estado)){
  df_UF = df[df$estado == uf,]
  df_heroes = rbind(df_heroes, subset(df_UF, df_UF$phft_dif > quantile(df_UF$phft_dif, .95)))
  # df_heroes = rbind(df_heroes, subset(df_UF, df_UF$cgtr_dif < quantile(df_UF$cgtr_dif, .05)))
}

# para facilitar, chamando sempre o "df"
df_base = df
# df = df_base
df_heroes_phft = df_heroes
# df_heroes_cgtr = df_heroes
df = df_heroes

# plota os casos separado por estado
slices = c("ratio", "height", "area", "azimute", "veneziana", "componente", "absortancia", "vidro", "open_fac", "somb", "wwr")
df$area = as.character(df$area)

for(slice in slices){
  png(filename = paste0(slice,'_phft.png'),
      # png(filename = paste0(slice,'_cgtr.png'), 
      width = 33.8, height = 19, units = "cm", res = 500)  
  plot(
    ggplot(df, aes(df[,slice])) +
      geom_bar() +  
      ggtitle(paste('PHft',slice)) +
      # ggtitle(paste('CgTr',slice)) +
      theme(axis.text.x = element_text(angle=90)) +
      facet_grid(.~estado)
  )
  dev.off()
}
#### ----

ggplot(df, aes(df$cgtr_cooling_ref, df$cgtr_cooling)) +
  geom_point() +  #col(stat= 'identity') +
  geom_point(aes(df$cgtr_heating_ref, df$cgtr_heating), color='red') +
  geom_abline() +
  xlim(c(0,95939348682))+ #(c(0,95939348682))+
  ylim(c(0,95939348682)) +
  # theme(axis.text.x = element_text(angle=90)) +
  facet_grid(.~estado)

df_UF = df_SC
df_plot = df_UF[df_UF$phft_dif > quantile(df_UF$phft_dif, .5)]

ggplot(df_plot, aes(df_plot$phft)) +
  geom_histogram(binwidth = .05)

ggplot(df_plot, aes(df_plot$phft_dif, fill = df_plot$absortancia)) +
  geom_histogram(binwidth = .05)

ggplot(df_plot, aes(df_plot$phft_dif, fill = df_plot$open_fac)) +
  geom_histogram(binwidth = .05)
