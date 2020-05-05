

setwd('codes/')

df = read.csv('files_list.csv', header = FALSE)

df_vn1a6 = df[substr(df$V6,1,12) == 'vn_Ref_ZB1a6',]
df_vn7e8 = df[substr(df$V6,1,12) == 'vn_Ref_ZB7e8',]
zb1a6 = c('GO','MG','PR','RJ','RS','SC')
zb7e8 = c('MA','TO')
epws = c(zb1a6,zb7e8)
  
df_base = df
apps = c('sala','dorm1','dorm2')

df_len = rep(NA, nrow(df_base)/4*length(apps)*length(epws))
df_vn_sub = data.frame('index'=df_len, 'x'=df_len, 'y'=df_len, 'z'=df_len, 'area_fac'=df_len, 'APP'=df_len, 'epw'=df_len, 'PHFT'=df_len)
df_ac_sub = data.frame('index'=df_len, 'x'=df_len, 'y'=df_len, 'z'=df_len, 'area_fac'=df_len, 'APP'=df_len, 'epw'=df_len, 'CgTr'=df_len)

ac_i = 0
vn_i = 0
for(row in 1:nrow(df_base)){
  for(epw in epws){
    if((epw %in% zb1a6 & substr(df_base$V6[row],10,12) == '1a6') | (epw %in% zb7e8 & substr(df_base$V6[row],10,12) == '7e8')){
      index = df_base$V1[row]
      x = df_base$V3[row]
      y = df_base$V4[row]
      z = df_base$V2[row]
      area_fac = df_base$V5[row]
      output = read.csv(paste0('ref_experiment/ref_',sprintf("%03d", index),'_',epw,'out.csv'))

      if(substr(df_base$V6[row],1,2) == 'vn'){
        if(substr(df_base$V6[row],10,12) == '1a6'){
          lim_sup = 26
        }else{
          lim_sup = 28
        }
        over_sala = mean(ifelse(output$SALA.Zone.Operative.Temperature..C..Hourly. > lim_sup & output$SALA1.People.Occupant.Count....Hourly. > 0, 1, 0))
        over_dorm1 = mean(ifelse(output$DORM1.Zone.Operative.Temperature..C..Hourly. > lim_sup & output$DORMITORIO1.People.Occupant.Count....Hourly. > 0, 1, 0))
        over_dorm2 = mean(ifelse(output$DORM2.Zone.Operative.Temperature..C..Hourly. > lim_sup & output$DORMITORIO2.People.Occupant.Count....Hourly. > 0, 1, 0))
        phft_results = c(over_sala,over_dorm1,over_dorm2)
        j = 0
        for(app in apps){
          j = j + 1
          vn_i = vn_i + 1
          df_vn_sub$index[vn_i] = index
          df_vn_sub$x[vn_i] = x
          df_vn_sub$y[vn_i] = y
          df_vn_sub$z[vn_i] = z
          df_vn_sub$area_fac[vn_i] = area_fac
          df_vn_sub$APP[vn_i] = app
          df_vn_sub$PHFT[vn_i] = phft_results[j]
          df_vn_sub$epw[vn_i] = epw
        }
      }else{
        cgtr_sala = sum(output$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly.)
        cgtr_dorm1 = sum(output$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly.)
        cgtr_dorm2 = sum(output$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly.)
        cgtr_results = c(cgtr_sala,cgtr_dorm1,cgtr_dorm2)
        j = 0
        for(app in apps){
          j = j+1
          ac_i = ac_i + 1
          df_ac_sub$index[ac_i] = index
          df_ac_sub$x[ac_i] = x
          df_ac_sub$y[ac_i] = y
          df_ac_sub$z[ac_i] = z
          df_ac_sub$area_fac[ac_i] = area_fac
          df_ac_sub$APP[ac_i] = app
          df_ac_sub$CgTr[ac_i] = cgtr_results[j]
          df_ac_sub$epw[ac_i] = epw
        }
      }
    }
  }
}

df_vn = df_vn_sub
df_ac = df_ac_sub

df_vn$CgTr[df_vn$x == df_ac$x & df_vn$y == df_ac$y & df_vn$z == df_ac$z & df_vn$area_fac == df_ac$area_fac & df_vn$APP == df_ac$APP & df_vn$epw == df_ac$epw] = df_ac$CgTr[df_vn$x == df_ac$x &
                          df_vn$y == df_ac$y &
                          df_vn$z == df_ac$z &
                          df_vn$area_fac == df_ac$area_fac &
                          df_vn$APP == df_ac$APP &
                          df_vn$epw == df_ac$epw
                          ]

write.csv(df_vn, 'output_ref_01-21.csv')

df_test = read.csv('output_ref_01-21.csv')


out = read.csv('multi/teste_schout.csv')
# out = read.csv('MA/ref_000_vn_MAout.csv') 
out = data.frame('dorm'=out$SCH_OCUP_DORM.Schedule.Value....Hourly.,'sala'=out$SCH_OCUP_SALA.Schedule.Value....Hourly.)

lim_sup = 28
f = 'AM.csv'
output = read.csv(paste0('teste_outputprocess/vn_',f))
over_sala = ifelse(output$SALA.Zone.Operative.Temperature..C..Hourly. >= lim_sup, 1, 0)
over_dorm1 = ifelse(output$DORM1.Zone.Operative.Temperature..C..Hourly. >= lim_sup, 1, 0)
over_dorm2 = ifelse(output$DORM2.Zone.Operative.Temperature..C..Hourly. >= lim_sup, 1, 0)
over_sala = over_sala[output$SALA.People.Occupant.Count....Hourly. > 0]
over_dorm1 = over_dorm1[output$DORM1.People.Occupant.Count....Hourly. > 0]
over_dorm2 = over_dorm2[output$DORM2.People.Occupant.Count....Hourly. > 0]
sum(over_sala)
sum(over_dorm1)
sum(over_dorm2)
mean(over_sala)
mean(over_dorm1)
mean(over_dorm2)

min(output$SALA.Zone.Operative.Temperature..C..Hourly.[output$SALA.People.Occupant.Count....Hourly. > 0])
max(output$SALA.Zone.Operative.Temperature..C..Hourly.[output$SALA.People.Occupant.Count....Hourly. > 0])
min(output$DORM1.Zone.Operative.Temperature..C..Hourly.[output$DORM1.People.Occupant.Count....Hourly. > 0])
max(output$DORM1.Zone.Operative.Temperature..C..Hourly.[output$DORM1.People.Occupant.Count....Hourly. > 0])
min(output$DORM2.Zone.Operative.Temperature..C..Hourly.[output$DORM1.People.Occupant.Count....Hourly. > 0])
max(output$DORM2.Zone.Operative.Temperature..C..Hourly.[output$DORM1.People.Occupant.Count....Hourly. > 0])

output = read.csv(paste0('teste_outputprocess/ac_',f))
cgtr_sala = sum(output$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly. * over_sala)
cgtr_dorm1 = sum(output$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly. * over_dorm1)
cgtr_dorm2 = sum(output$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly. * over_dorm2)

# xs = unique(df_vn$x)
# ys = unique(df_vn$y)
# zs = unique(df_vn$z)
# area_facs = unique(df_vn$area_fac)
# apps = unique(df_vn$APP)
# epws = unique(df_vn$epw)
# 
# df_len = rep(NA, nrow(df_vn))
# df_final = data.frame('x'=df_len,'y'=df_len,'z'=df_len,'area_fac'=df_len,'app'=df_len,'epw'=df_len,'PHFT'=df_len,'CgTr'=df_len)
# 
# i = 0
# for(x in xs){
#   for(y in ys){
#     for(z in zs){
#       for(area in area_facs){
#         for(app in apps){
#           for(epw in epws){
#             sub_df = subset(df_vn, df_vn$x == x &
#                               df_vn$x == df_ac$y &
#                               df_vn$x == df_ac$z &
#                               df_vn$x == df_ac$area_fac &
#                               df_vn$x == df_ac$APP &
#                               df_vn$x == df_ac$epw
#                             ]
#             if(nrow(df_vn[df,]))
#           }
#         }
#       }
#     }
#   }
# }
