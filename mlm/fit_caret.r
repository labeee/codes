# load libraries and global environment ####
invisible({
  pkgs = c('caret', 'doParallel', 'data.table', 'dplyr',
           'ggplot2', 'hydroGOF', 'Metrics', 'parallel', 'purrr')
  # required packages to fit models
  # pkgs = c(pkgs, 'brnn', 'kernlab', 'plyr', 'xgboost')
  lapply(pkgs, library, character.only = TRUE)
})

# base functions ####
# rename analysis index r squared
RenameRsq = function(x) sub('Rsquared', 'R²', x)

# machine learning model functions ####
# select features according to sobol analysis
SelectFeats = function(data, sa_path, threshold) {
  unvar = sa_path %>%
    RJSONIO::fromJSON() %>%
    keep(names(.) %in% c('S1', 'ST')) %>%
    as.data.frame() %>%
    mutate(var = colnames(data)[-length(data)]) %>%
    filter(ST <= threshold) %>%
    pull(var)
  data = data %>%
    select(-all_of(unvar))
  return(data)
}
# create dummy variables
CreateDummies = function(data) {
  dummy_model = dummyVars(targ ~ ., data = data)
  dummy_data = data.frame(predict(dummy_model, newdata = data))
  dummy_data$targ = data$targ
  return(dummy_data)
}
# split data into train and test sets
SplitData = function(train, data, train_prop, seed = 100) {
  # reproduce
  set.seed(seed)
  train_part = createDataPartition(data$targ, p = train_prop, list = FALSE)
  if(train) {
    data = data[train_part, ]
  } else {
    data = data[-train_part, ]
  }
  return(data)
}
# fit
FitModel = function(train_tech, tune_grid, samp_tech, nfolds, train_data,
                    cores_left, tune_length, eval = 'RMSE', seed = 200) {
  # run training in parallel
  cores = detectCores() - cores_left
  registerDoParallel(cores)
  fit_ctrl = trainControl(samp_tech, nfolds, search = 'random',
                          returnData = FALSE, verboseIter = TRUE)
  # reproduce results
  set.seed(seed)
  if (!is.null(tune_grid)) {
    fit = train(targ ~ ., train_data, trControl = fit_ctrl, tuneGrid = tune_grid,
                method = train_tech, metric = eval, preProcess = 'BoxCox')
  } else {
    fit = train(targ ~ ., train_data, trControl = fit_ctrl, tuneLength = tune_length,
                method = train_tech, metric = eval, preProcess = 'BoxCox')
  }
  registerDoSEQ()
  gc()
  return(fit)
}

# stats and plot functions ####
# stats comparison between models
CompModels = function(models) {
  comp = resamples(models)
  comp$metrics[3] = 'R²'
  names(comp$values)[-1] = names(comp$values)[-1] %>%
    RenameRsq() %>%
    toupper()
  comp$models = toupper(comp$models)
  return(comp)
}
# plot comparison between models
PlotComp = function(models_comp, suffix, output_dir) {
  plot = bwplot(models_comp, main = 'Models Training Accuracy',
                scales = list(x = list(relation = 'free'), y = list(relation = 'free')))
  SavePlot(plot, paste0('models_comp_', suffix), output_dir)
}
# plot training process
PlotFit = function(model, train_tech, suffix, output_dir) {
  k = ifelse(nrow(model$modelInfo$parameters) == 1, 1.5, 2)
  plot = plot(model, asp = 1/k,
              main = paste0(model$modelInfo$label, '\nHyperparameters Optimization'),
              xlab = paste('Hyperparameter Value -', model$modelInfo$parameters$label[1]),
              ylab = 'Validation RMSE (%)')
  SavePlot(plot, paste0('fit_', train_tech, '_', suffix), output_dir)
}
# plot predicted x real
PlotPerf = function(train_tech, pred, targ, suffix, output_dir) {
  df = data.frame(pred, targ)
  plot = ggplot(df, aes(x = targ, y = pred)) +
    geom_point(size = 0.5) +
    geom_abline(slope = 1, colour = 'red', size = 1) +
    labs(title = 'Model Testing Accuracy', subtitle = toupper(train_tech),
         x = 'Simulated Energy Consumption (kWh/m².year)',
         y = 'Predicted Energy Consumption (kWh/m².year)') +
    theme(plot.title = element_text(size = 19, hjust = 0.5),
          plot.subtitle = element_text(size = 18, face = 'bold', hjust = 0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  SavePlot(plot, paste0('perf_', train_tech, '_', suffix), output_dir)
}
# plot variable importances
PlotVarImp = function(train_tech, prediction, predictors, suffix, output_dir) {
  df = predictors %>%
    select(-targ) %>%
    filterVarImp(prediction) %>%
    setDT(keep.rownames = TRUE)
  plot = ggplot(df, aes(x = reorder(rn, Overall), y = Overall)) +
    coord_flip() +
    geom_bar(stat = 'identity') +
    labs(title = paste0('Simplified analysis of the variables influences\n', toupper(train_tech)),
         x = 'Variable', y = 'T-Value') +
    theme(plot.title = element_text(size = 19, hjust = 0.5),
          plot.subtitle = element_text(size = 18, face = 'bold', hjust = 0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 11))
  SavePlot(plot, paste0('var_imp_', train_tech, '_', suffix), output_dir)
}
# save plot
SavePlot = function(plot, plot_name, output_dir, lx = 33.8, ly = 19) {
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = lx, height = ly, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}
# create a summary table
GenAccuracyTable = function(models, pred, targ, suffix, output_dir) {
  table = SummAccuracy %>%
    mapply(models, names(models), pred, MoreArgs = list(targ), SIMPLIFY = FALSE) %>%
    bind_rows() %>% slice(2:n())
  write.csv(table, paste0(output_dir, 'summ_table_', suffix, '.csv'), row.names = FALSE)
}
# summarise fitting results
SummAccuracy = function(model, train_tech, pred, targ) {
  best_tune = model$bestTune
  index = mapply(function(x, y, z) which(z[[4]][y] == x), best_tune,
                 names(best_tune), SIMPLIFY = FALSE, MoreArgs = list(model))
  index = Reduce(intersect, index)
  table = data.frame(Model = toupper(train_tech), Sample = c('Train', 'Test'))
  train = model[[4]][index, c('MAE', 'RMSE', 'Rsquared')]
  test = data.frame('MAE' = mae(targ, pred),
                    'RMSE' = rmse(targ, pred),
                    'Rsquared' = cor(targ, pred)^2)
  table = cbind(table, rbind(train, test, make.row.names = FALSE))
  colnames(table)[5] = 'R²'
  table[1:2] = sapply(table[1:2], as.character)
  return(table)
}

# main function ####
GenMLModels = function(data_path, nfolds, tune_length, tune_grid,
                       sa_path, threshold, save_results, save_models, models_dir,
                       plots_dir, cores_left, inmet) {
  # load data
  raw_data = read.csv(data_path)
  # define qualitative and quantitative variables
  qual_vars = c('seed', 'storey', 'shell_wall',
                'shell_roof', 'blind', 'facade', 'mirror')
  quant_vars = colnames(raw_data[-length(raw_data)])
  quant_vars = quant_vars[!quant_vars %in% qual_vars]
  raw_data[, qual_vars] = lapply(raw_data[, qual_vars], factor)
  # select features according to sensitivity analysis
  raw_data = SelectFeats(raw_data, sa_path, threshold)
  # create dummy variables
  dummy_data = CreateDummies(raw_data)
  summary(raw_data)
  # split data into train and test sets
  raw_data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData, raw_data, 0.01)
  dummy_data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData, dummy_data, 0.01)
  # train
  # models_list = list(lm = 'lm', svmr = 'svmRadial', brnn = 'brnn')
  models_list = list(lm = 'lm', xgbt = 'xgbTree')
  models = mapply(FitModel, models_list, tune_grid, SIMPLIFY = FALSE,
                  MoreArgs = list('cv', nfolds, dummy_data$train, cores_left, tune_length))
  # test
  predictions = models %>%
    lapply(predict, newdata = dummy_data$test) %>%
    as.data.frame()
  # plots and results
  # stats comparison between models
  suffix = paste0('f', nfolds, '_', ncol(raw_data$train))
  models_comp = CompModels(models)
  models_summ = summary(models_comp)
  if (save_results) {
    # plot comparison between models
    PlotComp(models_comp, suffix, plots_dir)
    # plot training process
    mapply(PlotFit, models[-1], names(models[-1]), suffix, MoreArgs = list(plots_dir))
    # plot model performance
    mapply(PlotPerf, names(models), predictions,
           MoreArgs = list(dummy_data$test$targ, suffix, plots_dir))
    # plot variables importance
    mapply(PlotVarImp, names(models), predictions,
           MoreArgs = list(raw_data$test, suffix, plots_dir))
    # generate accuracy table
    GenAccuracyTable(models, predictions, dummy_data$test$targ, suffix, plots_dir)
  }
  if (save_models) {
    saveRDS(models, file = paste0(models_dir, 'models_', suffix, '.rds'))
  } else {
    return(models)
  }
}

# application ####
GenMLModels(
  data_path = './result/sample_year.csv',
  nfolds = 2,
  tune_length = 2,
  tune_grid = list(NULL),
  sa_path = './result/sobol_analysis.json',
  threshold = 0,
  save_results = TRUE,
  save_models = TRUE,
  models_dir = './result/',
  plots_dir = './plot_table/',
  cores_left = 0,
  inmet = inmet
)
