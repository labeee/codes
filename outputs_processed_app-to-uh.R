library(reshape2)
library(plyr)
library(stringr)

#MULTIFAMILIAR ----

# setwd("D:/_NBR-15575/Banco_Dados/outputs_processed_uh") #Amanda
setwd("~/codes") #Marcelo

#Le csv
# outputs_multi <-read.csv("dados_ref_multi_02_14_12_01.csv")
# outputs_multi <-read.csv("dados_multi_03_12_18_00.csv")
# outputs_multi <-read.csv("dados_sg_multi_ar_03_17_14_30.csv")
outputs_multi <-read.csv("dados_sg_multi_80_03_17_14_34.csv")


#Inclui coluna com descricao da uh, caso e cidade
city <- data.frame(str_split_fixed(outputs_multi$epw, "_", 3))
colnames(city) <- c("Brasil","estado","resto")
uh_info <- data.frame(str_split_fixed(outputs_multi$zone, "_", 4))
colnames(uh_info) <- c("floor","uh_expo","uh_position","app")
uh_info$uh_name <- paste(uh_info$floor,"_",uh_info$uh_expo,"_",uh_info$uh_position,"_",outputs_multi$case,"_",city$estado,sep="")
outputs_multi <- cbind(outputs_multi, uh_info[c(2,3,5)], city[2])
remove(uh_info)
remove(city)

#Inclui coluna de contato com o solo
solo <- data.frame(str_split_fixed(outputs_multi$geometria, "_", 5))
colnames(solo) <- c("1","solo","3","4","5")
outputs_multi <- cbind(outputs_multi, solo[2])
remove(solo)


#Calcula outputs para as uhs
outputs_multi_uh <- ddply(outputs_multi, .(uh_name), summarize,  ph_inf=mean(ph_inf), ph_sup=mean(ph_sup), phft=mean(phft), t_max=max(t_max), t_min=min(t_min), cgtr_cooling=sum(cgtr_cooling), cgtr_heating=sum(cgtr_heating), 
                      geometria=(geometria[1]), azimute=(azimute[1]), veneziana=(veneziana[1]), componente=(componente[1]), absortancia=(absortancia[1]), vidro=(vidro[1]), open_fac=(open_fac[1]), sombreamento=(sombreamento[1]), 
                      paf=(paf[1]), solo=(solo[1]), case=(case[1]), estado=(estado[1]), floor=(floor[1]), uh_expo=(uh_expo[1]), uh_position=(uh_position[1]),
                      ph_inf_ref=mean(ph_inf_ref), ph_sup_ref=mean(ph_sup_ref), phft_ref=mean(phft_ref), t_max_ref=max(t_max_ref), t_min_ref=min(t_min_ref), cgtr_cooling_ref=sum(cgtr_cooling_ref), cgtr_heating_ref=sum(cgtr_heating_ref))


#subset teste - conferir b com as primeiras tres linhas do outputs_multi_uh - TALVEZ TENHA QUE MUDAR, DEPENDENDO DOS CASOS INCLUIDOS NO DF
teste <- subset(outputs_multi, (zone=="CO_CANTO_11_SALA" | zone=="CO_CANTO_11_DORM1" | zone=="CO_CANTO_11_DORM2") & case==0 & epw =="BRA_GO_Itumbiara.867740_INMET.epw" & floor=="CO")

#Escreve csv
# write.csv(outputs_multi_uh, "outputs_multi_uh.csv")
# write.csv(outputs_multi_uh, "outputs_multi_ar_uh.csv")
write.csv(outputs_multi_uh, "outputs_multi_80_uh.csv")



#UNIFAMILIAR ----

# setwd("D:/_NBR-15575/Banco_Dados/outputs_processed_uh")  #Amanda

#Le csv
# outputs_uni <-read.csv("dados_uni_03_17_14_36.csv")
outputs_uni <-read.csv("dados_sg_uni_03_17_14_45.csv")
# outputs_uni <-read.csv("dados_uni_03_13_16_14.csv")
# outputs_uni <-read.csv("dados_sg_uni_ar_03_17_14_22.csv")
# outputs_uni = read.csv('dados_sg_uni_80_03_17_14_23.csv')

mean(outputs_uni$cgtr_heating[grepl('PR',outputs_uni$epw)])
mean(outputs_uni$cgtr_heating_ref)

#Inclui coluna com descricao da uh, caso e cidade
city <- data.frame(str_split_fixed(outputs_uni$epw, "_", 3))
colnames(city) <- c("Brasil","estado","resto")
# outputs_uni <- cbind(outputs_uni, city[2])
uh_info <- data.frame(outputs_uni$case)
colnames(uh_info) <- c("case")
uh_info$uh_name <- paste(outputs_uni$case,"_",city$estado,sep="")
outputs_uni <- cbind(outputs_uni, uh_info[2], city[2])
remove(uh_info)
remove(city)

#Inclui coluna de contato com o solo
solo <- data.frame(str_split_fixed(outputs_uni$geometria, "_", 5))
colnames(solo) <- c("1","solo","3","4","5")
outputs_uni <- cbind(outputs_uni, solo[2])
remove(solo)


#Calcula outputs para as uhs
outputs_uni_uh <- ddply(outputs_uni, .(uh_name), summarize,  ph_inf=mean(ph_inf), ph_sup=mean(ph_sup), phft=mean(phft), t_max=max(t_max), t_min=min(t_min), cgtr_cooling=sum(cgtr_cooling), cgtr_heating=sum(cgtr_heating), 
                          geometria=(geometria[1]), azimute=(azimute[1]), veneziana=(veneziana[1]), componente=(componente[1]), absortancia=(absortancia[1]), vidro=(vidro[1]), open_fac=(open_fac[1]), sombreamento=(sombreamento[1]), 
                          paf=(paf[1]), solo=(solo[1]), case=(case[1]), estado=(estado[1]), floor=(floor[1]),
                          ph_inf_ref=mean(ph_inf_ref), ph_sup_ref=mean(ph_sup_ref), phft_ref=mean(phft_ref), t_max_ref=max(t_max_ref), t_min_ref=min(t_min_ref), cgtr_cooling_ref=sum(cgtr_cooling_ref), cgtr_heating_ref=sum(cgtr_heating_ref))


#subset teste - conferir b com as primeiras tres linhas do outputs_uni_uh - TALVEZ TENHA QUE MUDAR, DEPENDENDO DOS CASOS INCLUIDOS NO DF
teste <- subset(outputs_uni, (zone=="SALA" | zone=="DORM1" | zone=="DORM2") & case==0 & epw =="BRA_GO_Itumbiara.867740_INMET.epw")

mean(outputs_uni_uh$cgtr_heating[outputs_uni_uh$estado == 'PR'])
mean(outputs_uni_uh$cgtr_heating_ref[outputs_uni_uh$estado == 'PR'])
mean(outputs_uni_uh$cgtr_heating[outputs_uni_uh$estado == 'GO'])
mean(outputs_uni_uh$cgtr_heating_ref[outputs_uni_uh$estado == 'GO'])

#Escreve csv
# write.csv(outputs_uni_uh, "outputs_uni_uh.csv")
write.csv(outputs_uni_uh, "outputs_sg_uni_uh.csv")
# write.csv(outputs_uni_uh, "outputs_uni_uh_ar.csv")
# write.csv(outputs_uni_uh, "outputs_uni_uh_80.csv")
