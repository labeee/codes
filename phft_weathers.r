# base functions ####
# calculate percentage of hours feeling uncomfortable (hot or cold)
CalcPH = function (op_temp, occup, tbs, feel) {
  # op_temp: operative temperature vector
  # occup: occupancy vector
  # tbs: dry bulb temperature annual mean
  # feel: 'hot' or 'cold'
  
  lim_sup = DefLimSup(tbs)
  if (feel == 'hot') {
    ph = sum(occup > 0 & op_temp > lim_sup) / sum(occup > 0) * 100
  } else {
    if (tbs < 25) {
      ph = sum(occup > 0 & op_temp < 18) / sum(occup > 0) * 100
    } else {
      ph = 0
    }
  }
  return(ph)
}

# define superior limit
DefLimSup = function(tbs) ifelse(tbs < 25, 26, ifelse(tbs < 27, 28, 30))

# main function ####
outs_dir = '/home/rodox/Seafile/NBR15575/17_Simulacoes_Referencia/'
inmet_path = '/home/rodox/git/nbr_15575/inmet_tbs_zb.csv'
load(paste0(outs_dir, 'outputs_uni.rdata'))
inmet = read.csv(inmet_path, stringsAsFactors = FALSE)

CalcResults = function(df, tag, tbs) {
  colnames(df) = c('date_time', 'op_temp_liv', 'op_temp_dorm1', 'op_temp_dorm2',
                   'op_temp_bwc', 'occup_dorm', 'occup_liv')
  op_temps = list('liv' = df$op_temp_liv, 'dorm1' = df$op_temp_dorm1,
                  'dorm2' = df$op_temp_dorm2)
  occups = list(df$occup_liv, df$occup_dorm, df$occup_dorm)
  ph_sup = mapply(CalcPH, op_temps, occups, tbs, 'hot')
  ph_sup = sum(ph_sup)/length(ph_sup)
  ph_inf = mapply(CalcPH, op_temps, occups, tbs, 'cold')
  ph_inf = sum(ph_inf)/length(ph_inf)
  phft = 100 - (ph_sup + ph_inf)
  return(data.frame(phft, ph_sup, ph_inf))
}

results = mapply(CalcResults, dfs_list, names(dfs_list), inmet$tbs,
           SIMPLIFY = TRUE)
results = as.data.frame(results)
results = as.data.frame(t(results), row.names = FALSE)
results = apply(results, 2, as.numeric)
results = round(results, 1)
results = cbind(inmet, results)
write.csv(results, file = '/home/rodox/git/nbr_15575/results_uni.csv')


