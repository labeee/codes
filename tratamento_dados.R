library(ggplot2)

setwd('~/codes')
#
ref.to.df = function(line, param){
  return(
    df_ref[
      df_ref$epw == df$epw[line] & df_ref$floor == df$floor[line] & df_ref$zone == df$zone[line] &
        df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line], param
      ]
  )
}
#
#### baixar os outputs ----
df_uni1 = read.csv('uni1/outputs_uni1_12-03-20_15-51.csv')
df_uni2 = read.csv('uni2/outputs_uni2_12-03-20_15-52.csv')
df_uni3 = read.csv('uni3/outputs_uni3_12-03-20_15-52.csv')
# 
# df_multi1 = read.csv('multi1/outputs_multi1_03-02-20_13-45_final.csv')
# df_multi2 = read.csv('multi2/outputs_multi2_31-01-20_18-45_final.csv')
# df_multi3 = read.csv('multi3/outputs_multi3_31-01-20_18-45_final.csv')
# df_multi4 = read.csv('outputs_multi4_03-02-20_16-06_final.csv')
# df_multi5 = read.csv('outputs_multi5_03-02-20_16-05_final.csv')
# df_multi6 = read.csv('outputs_multi6_03-02-20_16-04_final.csv')
# df_multi7 = read.csv('outputs_multi7_03-02-20_16-03_final.csv')
# 
# df_multi$caseold = df_multi$case
# 
# df_multi2$case = df_multi2$case + 40*1
# unique(df_multi2$case)
# df_multi3$case = df_multi3$case + 40*2
# unique(df_multi3$case)
# df_multi4$case = df_multi4$case + 40*3
# unique(df_multi4$case)
# df_multi5$caseold = df_multi5$case
# df_multi5$case = df_multi5$case + 40*4
# unique(df_multi5$case)
# df_multi6$case = df_multi6$case + 40*5
# unique(df_multi6$case)
# df_multi7$case = df_multi7$case + 40*6
# unique(df_multi7$case)

unique(df_uni1$case)
df_uni2$case = df_uni2$case + 416*1
unique(df_uni2$case)
df_uni3$case = df_uni3$case + 416*2
unique(df_uni3$case)

df_ref_uni = read.csv('refs_uni/outputs_refs_uni_13-03-20_15-41.csv')
# df_ref_multi = read.csv('refs_multi/outputs_refs_multi_07-02-20_09-34.csv')

df_uni = rbind(df_uni1,df_uni2,df_uni3)
# df_multi = rbind(df_multi1,df_multi2,df_multi3,df_multi4,df_multi5,df_multi6,df_multi7)

# write.csv(df_multi, 'outputs_multi.csv', row.names = FALSE)
write.csv(df_uni, 'outputs_uni.csv', row.names = FALSE)
####  ----

df_uni = read.csv('outputs_uni.csv')
# df_uni = read.csv('sg_uni/outputs_sg_uni_17-03-20_15-02.csv')
# df_uni = read.csv('sg_uni_ar/outputs_sg_uni_ar_17-03-20_14-10.csv')
# df_uni = read.csv('sg_uni_80/outputs_sg_uni_80_13-03-20_13-37.csv')
# df_multi = read.csv('sg_multi/outputs_sg_multi_12-03-20_13-47_final.csv')
# df_multi = read.csv('sg_multi_ar/outputs_sg_multi_ar_13-03-20_13-39_final.csv')
# df_multi = read.csv('sg_multi_80/outputs_sg_multi_80_13-03-20_13-39.csv')
# df_multi = read.csv('outputs_multi.csv')
df_ref_uni = read.csv('refs_uni/outputs_refs_uni_13-03-20_15-41.csv')
# df_ref_multi = read.csv('refs_multi/outputs_refs_multi_07-02-20_09-34.csv')

df_ref_uni$geometria = substr(df_ref_uni$geometria,12,29)
df_ref_uni$azimute = substr(df_ref_uni$azimute,8,23)

# df_ref_multi$geometria = substr(df_ref_multi$geometria,14,31)
# df_ref_multi$azimute = substr(df_ref_multi$azimute,8,23)
# levels(df_multi$epw) = levels(df_ref_multi$epw)
# levels(df_multi$azimute) = levels(df_ref_multi$azimute)
# levels(df_multi$geometria) = levels(df_ref_multi$geometria)

# unique(df_ref_uni$geometria)
# unique(df_uni$geometria)

# df = df_multi
# df_ref = df_ref_multi
df = df_uni
df_ref = df_ref_uni

df$epw = as.character(df$epw)
df_ref$epw = as.character(df_ref$epw)

max(df$cgtr_heating[grepl('TO',df$epw)])
max(df$cgtr_heating[grepl('PR',df$epw)])
mean(df$ph_inf[grepl('PR',df$epw)])

df$ph_inf_ref = NA
df$ph_sup_ref = NA
df$phft_ref = NA
df$t_min_ref = NA
df$t_max_ref = NA
df$cgtr_cooling_ref = NA
df$cgtr_heating_ref = NA

# for(line in 1:nrow(df)){
#   df$ph_inf_ref[line] = df_ref$ph_inf[df_ref$epw == df$epw[line] & df_ref$zone == df$zone[line] &
#                                           df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line]][1]
#   df$ph_sup_ref[line] = df_ref$ph_sup[df_ref$epw == df$epw[line] & df_ref$zone == df$zone[line] &
#                                           df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line]][1]
#   df$phft_ref[line] = df_ref$phft[df_ref$epw == df$epw[line] & df_ref$zone == df$zone[line] &
#                                           df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line]][1]
#   df$t_min_ref[line] = df_ref$t_min[df_ref$epw == df$epw[line] & df_ref$zone == df$zone[line] &
#                                           df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line]][1]
#   df$t_max_ref[line] = df_ref$t_max[df_ref$epw == df$epw[line] & df_ref$zone == df$zone[line] &
#                                           df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line]][1]
#   df$cgtr_cooling_ref[line] = df_ref$cgtr_cooling[df_ref$epw == df$epw[line] & df_ref$zone == df$zone[line] &
#                                           df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line]][1]
#   df$cgtr_heating_ref[line] = df_ref$cgtr_heating[df_ref$epw == df$epw[line] & df_ref$zone == df$zone[line] &
#                                           df_ref$geometria == df$geometria[line] & df_ref$azimute == df$azimute[line]][1]
# }

a =  Sys.time()

for(line in 1:nrow(df_ref)){
  df$ph_inf_ref[
    df$epw == df_ref$epw[line] & df$floor == df_ref$floor[line] & df$zone == df_ref$zone[line] &
      df$geometria == df_ref$geometria[line] & df$azimute == df_ref$azimute[line]
    ] = df_ref$ph_inf[line]

  df$ph_sup_ref[
    df$epw == df_ref$epw[line] & df$floor == df_ref$floor[line] & df$zone == df_ref$zone[line] &
      df$geometria == df_ref$geometria[line] & df$azimute == df_ref$azimute[line]
    ] = df_ref$ph_sup[line]

  df$phft_ref[
    df$epw == df_ref$epw[line] & df$floor == df_ref$floor[line] & df$zone == df_ref$zone[line] &
      df$geometria == df_ref$geometria[line] & df$azimute == df_ref$azimute[line]
    ] = df_ref$phft[line]

  df$t_min_ref[
    df$epw == df_ref$epw[line] & df$floor == df_ref$floor[line] & df$zone == df_ref$zone[line] &
      df$geometria == df_ref$geometria[line] & df$azimute == df_ref$azimute[line]
    ] = df_ref$t_min[line]

  df$t_max_ref[
    df$epw == df_ref$epw[line] & df$floor == df_ref$floor[line] & df$zone == df_ref$zone[line] &
      df$geometria == df_ref$geometria[line] & df$azimute == df_ref$azimute[line]
    ] = df_ref$t_max[line]

  df$cgtr_cooling_ref[
    df$epw == df_ref$epw[line] & df$floor == df_ref$floor[line] & df$zone == df_ref$zone[line] &
      df$geometria == df_ref$geometria[line] & df$azimute == df_ref$azimute[line]
    ] = df_ref$cgtr_cooling[line]
  
  df$cgtr_heating_ref[
    df$epw == df_ref$epw[line] & df$floor == df_ref$floor[line] & df$zone == df_ref$zone[line] &
      df$geometria == df_ref$geometria[line] & df$azimute == df_ref$azimute[line]
    ] = df_ref$cgtr_heating[line]
  print(line)
}

b =  Sys.time()
tempofor = b-a

mean(df$cgtr_heating[grepl('PR', df$epw)])
mean(df$cgtr_heating[grepl('GO', df$epw)])

# a =  Sys.time()
# 
# df$ph_inf_ref = sapply(1:nrow(df), ref.to.df, param='ph_inf')
# df$ph_sup_ref = sapply(1:nrow(df), ref.to.df, param='ph_sup')
# df$phft_ref = sapply(1:nrow(df), ref.to.df, param='phft')
# df$t_min_ref = sapply(1:nrow(df), ref.to.df, param='t_min')
# df$t_max_ref = sapply(1:nrow(df), ref.to.df, param='t_max')
# df$cgtr_cooling_ref = sapply(1:nrow(df), ref.to.df, param='cgtr_cooling')
# df$cgtr_heating_ref = sapply(1:nrow(df), ref.to.df, param='cgtr_heating')
# 
# b =  Sys.time()
# tempoapply= b-a

agora = format(Sys.time(), "%m_%d_%H_%M")
# write.csv(df, 'dados_uni.csv')
write.csv(df, paste0('dados_uni_',agora,'.csv'))
# write.csv(df, paste0('dados_sg_uni_',agora,'.csv'))
# write.csv(df, paste0('dados_sg_uni_ar_',agora,'.csv'))
# write.csv(df, paste0('dados_sg_uni_80_',agora,'.csv'))
# write.csv(df, paste0('dados_sg_multi_',agora,'.csv'))
# write.csv(df, paste0('dados_sg_multi_ar_',agora,'.csv'))
# write.csv(df, paste0('dados_sg_multi_80_',agora,'.csv'))
# write.csv(df, paste0('dados_multi_',agora,'.csv'))
# write.csv(df, paste0('dados_ref_multi_',agora,'.csv'))

# df = read.csv('dados_uni.csv')

# df = df_ref_multi
# nrow(df[df$floor == 'TE' & grepl('_p_',df$geometria),])
# df$floor = as.character(df$floor)
# df$floor[df$floor == 'TE' & grepl('_p_',df$geometria)] = 'PI'
# df_ref_multi = df

# df_plot = subset(df, grepl('_PR_',df$epw) & grepl('DORM',df$zone) & grepl('TP',df$zone))
# 
# phft_diff = df_plot$phft - df_plot$phft_ref
# 
# ggplot(df_plot, aes(df_plot$phft_ref)) +
#   geom_histogram(binwidth = .05)
# 
# ggplot(df_plot, aes(df_plot$phft)) +
#   geom_histogram(binwidth = .05)
# 
# ggplot(df_plot, aes(phft_diff)) +
#   geom_histogram(binwidth = .05)
# 
# # data explore ----
# climate = '_MA_'
# df = df_ref_uni
# 
# 
# 
# mean(df$phft[grepl(climate,df$epw)])
# median(df$phft[grepl(climate,df$epw)])
# 
# df_plot = df[grepl(climate,df$epw), ]
# ggplot(df_plot, aes(df_plot$phft, fill = df_plot$zone)) +
#   geom_histogram(binwidth = .05)
# 
#   length(unique(df$case))
# 
# df[df$case == 3,]
