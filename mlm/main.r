invisible({
  # setup environment ####
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'reticulate',
           'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'calc_target', 'run_ep_sim', 'tidy_sample')
  codes = paste0('./', codes, '.r')
  lapply(codes, source)
  occup = read.csv('./occup.csv')
  inmet = read.csv('./inmet_list.csv')
  geometry = read_json('./geometry.json')
  construction = read_json('./construction.json')
  fill = read_json('./fill.json')
  setup = read_json('./setup.json')
  
  # variables ####
  saltelli_path = './result/saltelli_sample.csv'
  seeds_dir = './seed/'
  models_dir = '~/rolante/master/model/'
  epws_dir = '~/rolante/weather/'
  output_dir = '~/rolante/master/output/'
  sample_path = './result/sample.csv'
  cores_left = 0
  
  # main code ####
  # generate sample
  py_run_file('./saltelli_sample.py')
  # read and tidy up sample
  sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
  # build cases
  with(sample, mcmapply(BuildModel, seed_path, nstrs, area, ratio, height, azimuth,
                        shell_wall, abs_wall, shell_roof, abs_roof, wwr_liv, wwr_dorm,
                        u_window, shgc, open_factor, blind, balcony, mirror, model_path,
                        MoreArgs = list('op_temp', construction, fill, setup, geometry),
                        mc.cores = detectCores() - cores_left))
  # run simulations
  ProcessEPSims(sample, output_dir, cores_left)
  # calculate targets and add them to the sample
  samples = ApplyCalcTarget(sample, output_dir, occup, inmet)
  # write sample file
  mapply(write.csv, sample, sample_path, MoreArgs = list(row.names = FALSE))
  # join samples
  JoinSamples(saltelli_path, sample_path)
})
