# base functions ####
# compile errors into one file
CompErrs = function(input_dir, errs_ind, output_path) {
  # input_dir: error files directory
  # ind: terminal errors count index
  # output_path: compilation file path
  file_paths = dir(input_dir, '.err', full.names = TRUE)
  file.create(output_path)
  sapply(file_paths, LabelErr)
  file.append(output_path, file_paths)
  file.remove(file_paths)
}
# count terminal and simulation errors
CountErrs = function(input_path, errs_ind, output_path) {
  # folder: error files directory
  # ind: terminal errors count index
  # comp_path: compilation file path
  # summ_path: summary file path
  file.create(output_path)
  sum_errs = mapply(SumErrs, list('sev' = 'Warning\\; ',
                                  'warn' = 'Successfully\\-\\- '),
                    c('[1-9] Severe Errors\\;', '[1-9] Warning\\;'),
                    MoreArgs = list(input_path))
  writeLines(c('Number of simulations with:',
               paste0('    Terminal errors = ', sum(errs_ind)),
               paste0('    Severe errors = ', sum_errs[1]),
               paste0('    Warnings = ', sum_errs[2])), output_path)
}
# label error files
LabelErr = function(path) {
  # path: error file path
  text = readLines(path)
  writeLines(c(paste0('Sim File: ', path), '', text, '\n'), path)
}
# remove suffix ('out') from a file name
RnmFile = function(path) {
  # path: file path/name
  file.rename(path, paste0(str_sub(path, 0, -8), str_sub(path, -4, -1)))
}
# remove unsefull files
RmUnsFiles = function(folder, rm_all_but = c('.csv', '.err'),
                      rm_also = c('sqlite.err', 'tbl.csv', 'ssz.csv', 'zsz.csv')) {
  # folder: files directory
  # rm_all_but: files that shouldn't be removed
  rm_all_but = str_flatten(rm_all_but, collapse = '|')
  rm_also = str_flatten(rm_also, collapse = '|')
  files_path = dir(folder, full.names = TRUE)
  index = !grepl(rm_all_but, files_path) | grepl(rm_also, files_path)
  files_path = files_path[index]
  file.remove(files_path)
}
# sum the number of warnings and severe errors in all simulations
SumErrs = function(start, end, comp_path) {
  # start: pattern before the number of simulation errors
  # end: pattern after the number of simulation errors
  # comp_path: compilation file path
  sum_err = comp_path %>%
    readLines() %>%
    str_detect(paste0('(?<=', start, ').*?(?=', end, ')')) %>%
    sum()
  return(sum_err)
}

# main functions ####
# run a single energyplus simulation
RunEPSim = function(model_path, epw_path, prefix, output_dir) {
  # model_path: full model file path
  # epw_path: full weather file path
  # weather: correspondent weather file
  # output_dir: output directory
  args = c('-r', '-w', epw_path, '-d', output_dir, '-p', prefix, model_path)
  err = system2('energyplus', args, stdout = FALSE, stderr = FALSE)
  return(err)
}
# run simulation slice
RunSimSlice = function(sample, temp_dir) {
  # run simulations
  errs_ind = with(sample, mapply(RunEPSim, model_path, epw_path, prefix, output_dir))
  # remove all files but .csv and .err
  RmUnsFiles(temp_dir)
  # list and rename the outputs left
  temp_dir %>%
    dir(full.names = TRUE) %>%
    sapply(RnmFile)
  # compile errors
  case_patt = paste0(result_dir, unique(sample$case))
  desc_path = paste0(case_patt, '_errors_description.txt')
  CompErrs(temp_dir, errs_ind, desc_path)
  # count terminal and severe errors and warnings
  summ_path = paste0(case_patt, '_errors_summary.txt')
  CountErrs(desc_path, errs_ind, summ_path)
}
