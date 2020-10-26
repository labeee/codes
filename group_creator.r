library('stringr')

idf_names_04 = matrix(list.files(path='C:/Users/Rodolfo/Desktop/LETI/abs_04',pattern='.idf',full.names=TRUE))
idf_names_06 = matrix(list.files(path='C:/Users/Rodolfo/Desktop/LETI/abs_06',pattern='.idf',full.names=TRUE))

group = NULL
for (i in 1:length(idf_names_04)){
  group = paste0(group,idf_names_04[i],',','C:/Users/Rodolfo/Desktop/LETI/epw/BRA_RJ_Rio.de.Janeiro-Santos.Dumont.AP.837550_TMYx.2003-2017.epw',',','C:/Users/Rodolfo/Desktop/LETI/resultados/rio_de_janeiro_',str_remove(str_remove(idf_names_04[i],'C:/Users/Rodolfo/Desktop/LETI/abs_04/'),'.idf'),',1\n',
                 idf_names_04[i],',','C:/Users/Rodolfo/Desktop/LETI/epw/BRA_AP_Macapa-Alcolumbre.Intl.AP.820980_TMYx.2003-2017.epw',',','C:/Users/Rodolfo/Desktop/LETI/resultados/macapa_',str_remove(str_remove(idf_names_04[i],'C:/Users/Rodolfo/Desktop/LETI/abs_04/'),'.idf'),',1\n')
}
for (i in 1:length(idf_names_06)){
  group = paste0(group,idf_names_06[i],',','C:/Users/Rodolfo/Desktop/LETI/epw/BRA_SP_Sao.Paulo-Congonhas.AP.837800_TMYx.2003-2017.epw',',','C:/Users/Rodolfo/Desktop/LETI/resultados/sao_paulo_',str_remove(str_remove(idf_names_06[i],'C:/Users/Rodolfo/Desktop/LETI/abs_06/'),'.idf'),',1\n',
                 idf_names_06[i],',','C:/Users/Rodolfo/Desktop/LETI/epw/BRA_RS_Santa.Maria.AB.839370_TMYx.2003-2017.epw',',','C:/Users/Rodolfo/Desktop/LETI/resultados/santa_maria_',str_remove(str_remove(idf_names_06[i],'C:/Users/Rodolfo/Desktop/LETI/abs_06/'),'.idf'),',1\n',
                 idf_names_06[i],',','C:/Users/Rodolfo/Desktop/LETI/epw/BRA_PR_Curitiba-Bacacheri.AP.838420_TMYx.2003-2017.epw',',','C:/Users/Rodolfo/Desktop/LETI/resultados/curitiba_',str_remove(str_remove(idf_names_06[i],'C:/Users/Rodolfo/Desktop/LETI/abs_06/'),'.idf'),',1\n')
}


group = gsub('/','\\\\',group)

write.csv(group,'C:/Users/Rodolfo/Desktop/LETI/group.epg')


