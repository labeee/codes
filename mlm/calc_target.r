# setup environment ####
library(data.table)
library(dplyr)
library(stringr)
inmet = read.csv('./inmet_list.csv')
occup = read.csv('./occup.csv')

# base functions ####
# calculate percentage of hours feeling uncomfortable (ph)
CalcPH = function (lim, op_temp, occup, mean_temp) {
  lim_sup = DefLimSup(mean_temp)
  occup = occup > 0
  if (lim == 'sup') {
    ph = sum(occup & op_temp > lim_sup)/sum(occup)*100
  } else {
    if (mean_temp < 25) {
      ph = sum(occup & op_temp < 18)/sum(occup)*100
    } else {
      ph = 0
    }
  }
  return(ph)
}
# define superior limit
DefLimSup = function(x) ifelse(x < 25, 26, ifelse(x < 27, 28, 30))
# calc phft
CalcPHFT = function(op_temp, occup, mean_temp) {
  phs = sapply(c(ph_sup = 'sup', ph_inf = 'inf'), CalcPH, op_temp, occup, mean_temp)
  phft = 100 - sum(phs)
  return(phft)
}
# calculate phft for the whole dwelling
CalcTarget = function(input_path, weather, occup, inmet) {
  df = read.csv(input_path, nrows = 1)
  cols = df %>% colnames() %>% str_which('LIV|DORM')
  df = input_path %>%
    fread(nrows = 52560, select = cols, colClasses = 'numeric') %>%
    as.data.frame()
  index = match(weather, inmet$municipio)
  zones = df %>% colnames() %>% str_extract('LIV|DORM') %>% str_to_lower()
  target = mapply(CalcPHFT, df, occup[zones], MoreArgs = list(inmet[index, 'tbsm'])) %>% mean()
  return(target)
}

# application ####
files = dir('~/rolante/test/', '\\.csv$', full.names = TRUE)
results = lapply(files, CalcTarget, 'rio_de_janeiro', occup, inmet)
names(results) = files
