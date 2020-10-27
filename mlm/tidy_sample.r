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
TidySample = function(sample_path, seeds_dir, models_dir, epws_dir, inmet) {
  qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof', 'blind', 'facade', 'mirror')
  sample = sample_path %>%
    read.csv()
  epw = sapply(sample$dbt, function(x, y) which.min(abs(x - y)), inmet$tbsm)
  sample = sample %>%
    mutate_at(qual_vars, floor) %>%
    mutate(balcony = ifelse(balcony <= 0.5, 0, balcony),
           dbt = inmet$tbsm[epw])
  write.csv(sample, sample_path, row.names = FALSE)
  sample = sample %>%
    mutate(seed_path = paste0(seeds_dir, 'seed', seed, 'c', facade, '.json'),
           epw_path = paste0(epws_dir, inmet$arquivo_climatico[epw], '.epw'),
           .before = 1) %>%
    unique() %>%
    mutate(prefix = paste0('case', str_pad(1:n(), 6, 'left', 0)),
           model_path = paste0(models_dir, prefix, '.epJSON'),
           output_dir = paste0(output_dir, prefix, '/'),
           .before = 2)
  return(sample)
}