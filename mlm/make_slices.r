# functions ####
# sum errors in summary files
AddSumms = function(df) {
  summ = c('Number of simulations with:',
           paste('    Terminal errors =', sum(df[1, ])),
           paste('    Severe errors =', sum(df[2, ])),
           paste('    Terminal errors =', sum(df[3, ])))
  return(summ)
}
# handle slices
HandleSlices = function(pattern, input_dir) {
  file_paths = dir(input_dir, pattern, full.names = TRUE)
  files = lapply(file_paths, readLines)
  if (pattern == 'summary') {
    file = files %>%
      lapply(function(x) as.numeric(str_extract(x, '\\d'))) %>%
      as.data.frame() %>%
      slice(-1) %>%
      AddSumms()
  } else if (pattern == 'description') {
    file = unlist(files)
  } else {
    stop('Pattern is not supported!')
  }
  writeLines(file, paste0(result_dir, 'errors_', pattern, '.txt'))
  file.remove(file_paths)
}
# rename error files
MvErrs = function(input_dir, output_dir, case) {
  file_paths = dir(input_dir, pattern = '\\.txt', full.names = TRUE)
  file.rename(file_paths, paste0(output_dir, case, '_', basename(file_paths)))
}
# remove csv files
RmCSVs = function(input_dir) {
  file_paths = dir(input_dir, pattern = '\\.csv', full.names = TRUE)
  file.remove(file_paths)
}
# write samples
WriteSample = function(pattern, output_path, input_dir) {
  file_paths = dir(input_dir, pattern, full.names = TRUE)
  file_paths %>%
    lapply(read.csv) %>%
    bind_rows() %>%
    write.csv(output_path, row.names = FALSE)
  file.remove(file_paths)
}
# write slice
WriteSlice = function(period, sample, output_dir, case) {
  output_path = paste0(output_dir, case, '_sample_', period, '.csv')
  write.csv(sample, output_path, row.names = FALSE)
}

# main function ####
MakeSlices = function(sample, n, size, cores) {
  sample = split(sample, c(rep(n, each = cores), rep(length(n + 1), nrow(sample) %% cores)))
  return(sample)
}
