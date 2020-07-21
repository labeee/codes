invisible({
  # load global enviroment ####
  pkgs = c('data.table', 'dplyr', 'ggplot2', 'ggrepel',
           'jsonlite', 'parallel', 'purrr', 'scales', 'stringr')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'display_result', 'process_output', 'run_ep_sim', 'split_output')
  codes = paste0('~/git/master/code/', codes, '.r')
  lapply(codes, source)
  load('~/rolante/nbr/outputs.rds')
  
  
  # variables ####
  seed_path = '~/git/master/seed/linear.json'
  models_dir = '~/rolante/nbr/model/'
  epws_dir = '~/rolante/weather/'
  output_dir = '~/rolante/nbr/output/'
  split_dir = '~/rolante/nbr/output/split/'
  result_dir = '~/rolante/nbr/result/'
  cores_left = 0
  
  # base functions ####
  AddOutputs = function(model, outputs) {
    model$'Output:Variable' = append(model$'Output:Variable', outputs)
    return(model)
  }
  ApplyBuildModel = function(boundary, shell_wall, shell_roof, seed_path, output_dir) {
    BuildModel(shell_wall = shell_wall, shell_roof = shell_roof, n_strs = 5, lsm = TRUE,
               boundary = boundary, seed_path = seed_path, output_dir = output_dir,
               fill = fill, construction = construction, setup = setup)
  }
  ChangeSurfContact = function(tag, surf) {
    str_sub(surf$outside_boundary_condition_object, 2, 2) = str_sub(tag, 2, 2)
    return(surf)
  }
  EditModel = function(model_path) {
    model = read_json(model_path)
    tags = names(model$'BuildingSurface:Detailed')
    index = str_detect(tags, 'f[23].*(floor|roof)')
    tags = tags[index]
    model$'BuildingSurface:Detailed'[index] = mapply(ChangeSurfContact, tags, SIMPLIFY = FALSE,
                                                     model$'BuildingSurface:Detailed'[index])
    model_path = str_replace(model_path, '(?<=\\/\\/0)[01]', '2')
    write_json(model, model_path, pretty = T, auto_unbox = T)
  }
  
  # main code ####
  # # build models
  # grid = expand.grid('boundary' = c('surface', 'adiabatic'), 'shell' = c('11', '64'))
  # grid$shell_wall = grid$shell %>% str_sub(1, 1) %>% as.numeric()
  # grid$shell_roof = grid$shell %>% str_sub(2, 2) %>% as.numeric()
  # mapply(ApplyBuildModel, grid$boundary, grid$shell_wall,
  #        grid$shell_roof, seed_path, models_dir)
  # model_paths = dir(models_dir, '\\.epJSON', full.names = TRUE)
  # models_list = model_paths %>% lapply(read_json) %>% lapply(AddOutputs, outputs)
  # mapply(write_json, models_list, model_paths, pretty = TRUE, auto_unbox = TRUE)
  # model_paths = model_paths[grepl('^00', basename(model_paths))]
  # lapply(model_paths, EditModel)
  # # run simulations
  # ProcessEPSims(models_dir = models_dir, epws_dir = epws_dir, load_files = TRUE,
  #               weathers = c('curitiba', 'sorriso', 'teresina'), inmet = inmet,
  #               output_dir = output_dir, cores_left = cores_left)
  # split output
  ApplySplOut(input_dir = output_dir, output_dir = split_dir, n_strs = 5, cores_left = cores_left,
              habs = c('csw', 'msw', 'mse', 'cse', 'cne', 'mne', 'mnw', 'cnw'))
  # # process output
  # grid = expand.grid('sim' = 0:2, 'shell' = c('ref', 'sf'), 'level' = 1:5)
  # mapply(ProcessOutput, '~/rolante/nbr/output/', grid$sim, 'linear',
  #        grid$shell, grid$level, result_dir)
  # df = result_dir %>% dir(full.names = TRUE) %>% lapply(read.csv, stringsAsFactors = FALSE) %>%
  #   bind_rows() %>% write.csv(paste0(result_dir, 'simp_linear.csv'))
})
# # plot
# df = RnmValues(df)
# grid = expand.grid('si' = c('01', '02'), 'ro' = c('Sala', 'Dormitorio'),
#                    'st' = c('Terreo', 'Intermediario', 'Cobertura'))
# grid$id = ifelse(grid$ro == 'Sala', NA, 2)
# others = list(df = df, tp = 'Linear', sh = 'Referencia', po = 'Canto',
#               or = 'NW', we = 'Sao Paulo', output_dir = '~/rolante/nbr/plot_table/')
# mapply(BarPlotTB, si = grid$si, ro = grid$ro, id = grid$id, st = grid$st, MoreArgs = others)