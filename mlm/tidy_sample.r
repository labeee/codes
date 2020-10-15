# main functions ####
# join samples
JoinSamples = function(saltelli_path, sample_path) {
  sample = read.csv(sample_path)
  cols = colnames(sample)[-ncol(sample)]
  saltelli_path %>%
    read.csv() %>%
    left_join(sample, by = cols) %>%
    write.csv(saltelli_path, row.names = FALSE)
}
# tidy sample
TidySample = function(sample_path, seeds_dir, models_dir, epws_dir, inmet) {
  qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof',
                'blind', 'facade', 'mirror', 'epw')
  sample = sample_path %>%
    read.csv() %>%
    mutate_at(qual_vars, floor) %>%
    mutate(balcony = ifelse(balcony <= 0.5, 0, balcony))
  write.csv(sample, sample_path, row.names = FALSE)
  sample = sample %>%
    unique() %>%
    mutate(seed_path = paste0(seeds_dir, 'seed', seed, 'c', facade,
                              ifelse(balcony > 0, 'b', ''), '.json'),
           prefix = paste0('case', str_pad(1:n(), 6, 'left', 0)),
           epw_path = paste0(epws_dir, inmet$arquivo_climatico[epw], '.epw'),
           nstrs = ifelse(storey == 2, 3, 2),
           .before = 1) %>%
    mutate(model_path = paste0(models_dir, prefix, '.epJSON'),
           .before = 2)
  return(sample)
}