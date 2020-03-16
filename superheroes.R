library(ggplot2)

setwd('~/codes')
#

dac = read.csv('sg_uni/BRA_PR_Curitiba.838420_INMET.epw/sg_37_acout.csv')
dvn = read.csv('sg_uni/BRA_PR_Curitiba.838420_INMET.epw/sg_37_vnout.csv')

sum(dac$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..Hourly.[dvn$SCH_OCUP_SALA.Schedule.Value....Hourly. > 0 & dvn$SALA.Zone.Operative.Temperature..C..Hourly. < 18]) +
  sum(dac$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..Hourly.[dvn$SCH_OCUP_DORM1.Schedule.Value....Hourly. > 0 & dvn$DORM1.Zone.Operative.Temperature..C..Hourly. < 18]) +
  sum(dac$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Heating.Energy..J..Hourly.[dvn$SCH_OCUP_DORM2.Schedule.Value....Hourly. > 0 & dvn$DORM2.Zone.Operative.Temperature..C..Hourly. < 18])

sum(dac$SALA.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly.[dvn$SCH_OCUP_SALA.Schedule.Value....Hourly. > 0 & dvn$SALA.Zone.Operative.Temperature..C..Hourly. >= 26]) +
  sum(dac$DORM1.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly.[dvn$SCH_OCUP_DORM1.Schedule.Value....Hourly. > 0 & dvn$DORM1.Zone.Operative.Temperature..C..Hourly. >= 26]) +
  sum(dac$DORM2.IDEAL.LOADS.AIR.SYSTEM.Zone.Ideal.Loads.Zone.Total.Cooling.Energy..J..Hourly.[dvn$SCH_OCUP_DORM2.Schedule.Value....Hourly. > 0 & dvn$DORM2.Zone.Operative.Temperature..C..Hourly. >= 26])


strip = function(string, split_char='_', position=4){
  # para identificar características de geometria a partir do nome do slice
  as.character(strsplit(as.character(string),split_char)[[1]][position])
}

# tree ---------

# df = read.csv('sg_uni/outputs_sg_uni_11-03-20_16-39_final.csv')
df = read.csv('dados_sg_uni_03_11_18_20_uh.csv')

# -------
df = read.csv('outputs_uni_uh.csv')
# df = read.csv('outputs_multi_uh.csv')

hist(df$phft)

df$zb[df$estado == "PR"] = "1"
df$zb[df$estado == "RS"] = "2"
df$zb[df$estado == "SC"] = "3"
df$zb[df$estado == "MG"] = "4"
df$zb[df$estado == "RJ"] = "5"
df$zb[df$estado == "GO"] = "6"
df$zb[df$estado == "TO"] = "7"
df$zb[df$estado == "MA"] = "8"

# diferencas entre caso e ref
df$phft_dif = df$phft - df$phft_ref
df$cgtr_dif = df$cgtr_cooling + df$cgtr_heating - df$cgtr_cooling_ref - df$cgtr_heating_ref

#area da uh base
area_uh = 38.58

# identifica caracteristicas a partir do nome do slice
df$ratio = sapply(df$geometria, strip, position = 4)  # strsplit(as.character(df$geometria),'_')[4]
df$height = sapply(df$geometria, strip, position = 5)  # strsplit(as.character(df$geometria),'_')[[1]][5]
df$area = sapply(df$geometria, strip, position = 6)  # strsplit(as.character(df$geometria),'_')[[1]][6]
df$area = ifelse(
  substr(df$area,1,1) == '0', area_uh, ifelse(
    substr(df$area,1,1) == '1', 1.5*area_uh,
    2*area_uh
  )
)
df$cgtr_dif = df$cgtr_dif/df$area

df$somb = sapply(df$sombreamento, strip, position = 3)  # strsplit(as.character(df$sombreamento),'_')[[1]][3]
df$wwr = sapply(df$paf, strip, position = 3)  # strsplit(as.character(df$paf),'_')[[1]][3]

df = subset(df, grepl('sfiso',df$componente))
# df = subset(df, grepl('000',df$azimute))
# df = subset(df, grepl('0_0_0',df$geometria))

# separe por estado
df_SC = df[df$estado == 'SC',]
df_PR = df[df$estado == 'PR',]
df_RS = df[df$estado == 'RS',]
df_GO = df[df$estado == 'GO',]
df_MA = df[df$estado == 'MA',]
df_MG = df[df$estado == 'MG',]
df_RJ = df[df$estado == 'RJ',]
df_TO = df[df$estado == 'TO',]

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

ggplot(df, aes(df$wwr)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle=90)) +
  facet_grid(somb~zb)

# plota os casos separado por estado
slices = c("ratio", "height", "area", "azimute", "veneziana", "componente", "absortancia", "vidro", "open_fac", "somb", "wwr")
df$area = as.character(df$area)

for(slice in slices){
  # png(filename = paste0(slice,'_phft.png'),
  # png(filename = paste0(slice,'_cgtr.png'), 
      # width = 33.8, height = 19, units = "cm", res = 500)  
  plot(
    ggplot(df, aes(df[,slice])) +
    geom_bar() +  
    ggtitle(paste('PHft',slice)) +
    # ggtitle(paste('CgTr',slice)) +
    theme(axis.text.x = element_text(angle=90)) +
    facet_grid(.~zb)
  )
  # dev.off()
}
  1#### ----

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
