invisible({
  # setup environment ####
  setwd('~/git/labeee/mlm/')
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'reticulate',
           'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'calc_target', 'make_slices', 'run_ep_sim', 'tidy_sample')
  codes = paste0(codes, '.r')
  lapply(codes, source)
  occup = read.csv('occup.csv')
  inmet = read.csv('inmet_list.csv')
  geometry = read_json('geometry.json')
  construction = read_json('construction.json')
  fill = read_json('fill.json')
  setup = read_json('setup.json')
  
  # variables ####
  seeds_dir = './'
  models_dir = '~/rolante/labeee/model/'
  epws_dir = '~/rolante/weather/'
  output_dir = '~/rolante/labeee/output/'
  result_dir = '~/rolante/labeee/result/'
  typo = 'uni'
  cores_left = 0
  
  # functions ####
  # process slices of simulation sample
  ProcessNBRSims = function(sample) {
    # keep case
    case_path = paste0(result_dir, unique(sample$case), '.csv')
    # create temporary directory to hold simulation files
    temp_dir = unique(sample$output_dir)
    dir.create(temp_dir)
    # run simulations
    RunSimSlice(sample, temp_dir)
    # calculate targets and add them to the sample
    sample = CalcTargets(sample, occup, typo, inmet)
    # remove directory with files
    unlink(temp_dir, recursive = TRUE, force = TRUE)
    # write sample file
    write.csv(sample, case_path, row.names = FALSE)
  }
  
  # main code ####
  # define number of core
  cores = detectCores() - cores_left
  # generate sample
  py_run_file(paste0('saltelli_sample_', typo, '.py'))
  # read and tidy up sample
  saltelli_path = paste0('saltelli_sample_', typo, '.csv')
  sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, typo, inmet)
  # define simulations for each case
  sample = sample %>%
    mutate(case = prefix, .before = prefix) %>%
    slice(rep(1:n(), each = 2)) %>%
    mutate(cond = rep(c('afn', 'hvac'), n()/2),
           outputs = rep(c('op_temp', 'ideal_loads'), n()/2),
           .after = epw_path,
           model_path = 'str_sub<-'(model_path, -7, -8, value = paste0('_', cond)),
           prefix = paste0(prefix, '_', cond))
  # build cases
  if (typo == 'uni') {
    nstrs = 1
    balcony = 0
  } else {
    nstrs = 3
  }
  with(sample, mcmapply(BuildModel, seed_path, area, ratio, height, azimuth, shell_wall, abs_wall,
                        shell_roof, abs_roof, wwr_liv, wwr_dorm, u_window, shgc, open_factor, blind,
                        balcony, mirror, cond, model_path, outputs, nstrs, mc.cores = cores,
                        MoreArgs = list(construction, fill, setup, geometry[[typo]])))
  # run simulations in slices
  sample = split(sample, sample$case)
  mclapply(sample, ProcessNBRSims, mc.cores = cores)
  # pile up results
  sample_path = paste0('sample_', typo, '.csv')
  WriteSample('.*\\.csv', sample_path, result_dir)
  lapply(c('summary', 'description'), HandleSlices, result_dir)
  # join samples
  JoinSamples(saltelli_path, sample_path)
})
