# base functions ####
# calculate percentage of hours feeling comfortable (phft)
CalcPHFT = function(op_temp, occup, mean_temp) {
  lim_sup = DefLimSup(mean_temp)
  occup = occup > 0
  phfts = occup & op_temp < lim_sup
  if (mean_temp < 25) {
    phfti = occup & op_temp > 18
  } else {
    phfti = occup
  }
  phft = phfti & phfts
  return(phft)
}
# define zones
DefZones = function(df) str_to_lower(str_extract(colnames(df), 'LIV|DORM\\d'))
# define superior limit
DefLimSup = function(x) ifelse(x < 25, 26, ifelse(x < 27, 28, 30))

# main function ####
CalcTargets = function(sample, occup, typo, inmet, unit = 'kwh', timestep = 6) {
  div = ifelse(unit == 'kwh', 3600000, 1000)
  input_paths = dir(unique(sample$output_dir), '\\.csv', full.names = TRUE)
  weather = sample %>% pull(epw_path) %>% unique() %>% basename() %>% str_remove('\\.epw')
  if (typo == 'uni') {
    pattern = ''
  } else {
    storey = sample %>% pull(storey) %>% unique()
    pattern = paste0('F', storey, '_')
  }
  cols = input_paths %>% lapply(read.csv, nrows = 1) %>%
    lapply(function(x) str_which(colnames(x), paste0(pattern, '(LIV|DORM)')))
  args = list(nrows = 8760*timestep, colClasses = 'numeric')
  dfs = mapply(fread, input_paths, select = cols, MoreArgs = args) %>% lapply(as.data.frame)
  index = match(weather, inmet$arquivo_climatico)
  conds = sapply(c(afn = 'afn', hvac = 'hvac'), function(x, y) str_which(y, x), names(dfs))
  zones = DefZones(dfs[[conds['afn']]])
  rooms = str_remove(zones, '\\d')
  phft = mapply(CalcPHFT, dfs[[conds['afn']]], occup[rooms],
                inmet[index, 'tbsm'], SIMPLIFY = FALSE)
  cols = zones %>% lapply(function(x, y) which(grepl(toupper(x), y)),
                          colnames(dfs[[conds['hvac']]]))
  cgtt = mapply(function(x, y, z) sum(colSums(z[!x, y])), phft, cols,
                MoreArgs = list(dfs[[conds['hvac']]]))
  cgtt = sum(cgtt)/div/unique(sample$area) %>% round(2)
  phft = mapply(function(x, y) sum(x)/sum(y > 0)*100, phft, occup[rooms]) %>%
    mean() %>% round(2)
  cols = c('seed_path', 'case', 'prefix', 'model_path',
           'output_dir', 'epw_path', 'cond', 'outputs')
  sample = sample %>% select(-all_of(cols)) %>% unique()
  sample = cbind(sample, phft, cgtt)
  return(sample)
}
