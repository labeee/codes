# main functions ####
# join samples
JoinSamples = function(saltelli_path, sample_path) {
  sample = read.csv(sample_path)
  ncols = ncol(sample)
  cols = colnames(sample)[-c(ncols, ncols - 1)]
  saltelli_path %>%
    read.csv() %>%
    left_join(sample, by = cols) %>%
    write.csv(saltelli_path, row.names = FALSE)
}
# tidy sample
TidySample = function(sample_path, seeds_dir, models_dir, epws_dir, typo, inmet) {
  sample = read.csv(sample_path)
  epw = sapply(sample$dbt, function(x, y) which.min(abs(x - y)), inmet$tbsm)
  if (typo == 'uni') {
    qual_vars = c('shell_wall', 'shell_roof', 'blind', 'mirror')
  } else {
    qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof', 'blind', 'facade', 'mirror')
  }
  sample = sample %>%
    mutate_at(qual_vars, floor) %>%
    mutate(dbt = inmet$tbsm[epw])
  if (typo == 'uni') {
    sample = mutate(sample, seed_path = paste0(seeds_dir, 'seed_u1.json'), .before = 1)
  } else {
    sample = mutate(sample, balcony = ifelse(balcony <= 0.5, 0, balcony),
                    seed_path = paste0(seeds_dir, 'seed_m', seed, 'c', facade, '.json'),
                    .before = 1)
  }
  write.csv(sample, sample_path, row.names = FALSE)
  sample = sample %>%
    mutate(epw_path = paste0(epws_dir, inmet$arquivo_climatico[epw], '.epw'),
           .before = 2) %>%
    unique() %>%
    mutate(prefix = paste0('case', str_pad(1:n(), 6, 'left', 0)),
           model_path = paste0(models_dir, prefix, '.epJSON'),
           output_dir = paste0(output_dir, prefix, '/'),
           .before = 2)
  return(sample)
}