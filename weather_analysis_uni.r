# load libraries ####
pkgs = c('dplyr', 'ggplot2', 'ggrepel', 'stringr', 'tidyr')
lapply(pkgs, library, character.only = TRUE)

# base functions ####
# calculate percentage of hours feeling uncomfortable (ph)
CalcPH = function (op_temp, occup, tbs, feel) {
  # op_temp: operative temperature vector
  # occup: occupancy vector
  # tbs: dry bulb temperature annual mean
  # feel: 'hot' or 'cold'
  lim_sup = DefLimSup(tbs)
  if (feel == 'hot') {
    ph = sum(occup > 0 & op_temp > lim_sup) / sum(occup > 0) * 100
  } else {
    if (tbs < 25) {
      ph = sum(occup > 0 & op_temp < 18) / sum(occup > 0) * 100
    } else {
      ph = 0
    }
  }
  return(ph)
}
# define superior limit
DefLimSup = function(tbs) ifelse(tbs < 25, 26, ifelse(tbs < 27, 28, 30))
# calculate phft, ph superior and ph inferior
CalcResults = function(df, tag, tbs) {
  colnames(df) = c('date_time', 'op_temp_liv', 'op_temp_dorm1', 'op_temp_dorm2',
                   'op_temp_bwc', 'occup_dorm', 'occup_liv')
  op_temps = list('liv' = df$op_temp_liv, 'dorm1' = df$op_temp_dorm1,
                  'dorm2' = df$op_temp_dorm2)
  occups = list(df$occup_liv, df$occup_dorm, df$occup_dorm)
  ph_sup = mapply(CalcPH, op_temps, occups, tbs, 'hot')
  ph_sup = sum(ph_sup)/length(ph_sup)
  ph_inf = mapply(CalcPH, op_temps, occups, tbs, 'cold')
  ph_inf = sum(ph_inf)/length(ph_inf)
  phft = 100 - (ph_sup + ph_inf)
  return(data.frame(phft, ph_sup, ph_inf))
}
# plot scatter
PlotScatter = function(df, metric, output_dir) {
  lim_sup = DefLimSup(results$tbs)
  results$weather = results$weather %>%
    str_sub(8, -14) %>%
    str_replace_all('\\.|\\-', '')
  results$weather = ifelse(lim_sup == 30, results$weather, NA)
  lim_sup = factor(lim_sup)
  results$zbb = factor(results$zbb)
  if (metric == 'phft') {
    plot = ggplot(results) +
      geom_point(aes(phft, tbs, colour = zbb)) +
      geom_text_repel(aes(phft, tbs, label = weather),
                      segment.size = 0.1, size = 3, na.rm = TRUE,
                      parse = TRUE, box.padding = 0.5, point.padding = 0.5) +
      geom_smooth(aes(phft, tbs), method = 'lm', se = FALSE,
                  linetype = 'dashed', size = 0.7) +
      geom_hline(yintercept = c(25, 27), linetype = 'dotted') +
      geom_text(aes(x = max(results$phft) - 2, y = 25, vjust = 1.5,
                    label = 'Lim. Sup. 26 °C'), size = 3.5) +
      geom_text(aes(x = max(results$phft) - 2, y = 25, vjust = -0.5,
                    label = 'Lim. Sup. 28 °C'), size = 3.5) +
      geom_text(aes(x = max(results$phft) - 2, y = 27, vjust = 1.5,
                    label = 'Lim. Sup. 28 °C'), size = 3.5) +
      geom_text(aes(x = max(results$phft) - 2, y = 27, vjust = -0.5,
                    label = 'Lim. Sup. 30 °C'), size = 3.5)
    x_lab = 'PHFT'
  } else if (metric == 'ph_sup') {
    plot = ggplot(results) +
      geom_point(aes(ph_sup, tbs, colour = zbb)) +
      geom_text_repel(aes(ph_sup, tbs, label = weather),
                      segment.size = 0.1, size = 3, na.rm = TRUE,
                      parse = TRUE, box.padding = 0.5, point.padding = 0.5) +
      geom_smooth(aes(ph_sup, tbs), method = 'lm', se = FALSE,
                  linetype = 'dashed', size = 0.7) +
      geom_hline(yintercept = c(25, 27), linetype = 'dotted') +
      geom_text(aes(x = min(results$ph_sup) + 2, y = 25, vjust = 1.5,
                    label = 'Lim. Sup. 26 °C'), size = 3.5) +
      geom_text(aes(x = min(results$ph_sup) + 2, y = 25, vjust = -0.5,
                    label = 'Lim. Sup. 28 °C'), size = 3.5) +
      geom_text(aes(x = min(results$ph_sup) + 2, y = 27, vjust = 1.5,
                    label = 'Lim. Sup. 28 °C'), size = 3.5) +
      geom_text(aes(x = min(results$ph_sup) + 2, y = 27, vjust = -0.5,
                    label = 'Lim. Sup. 30 °C'), size = 3.5)
    x_lab = 'PH Superior'
  } else if (metric == 'ph_inf') {
    results = filter(results, tbs < 25)
    plot = ggplot(results) +
      geom_point(aes(ph_inf, tbs, colour = zbb)) +
      geom_smooth(aes(ph_inf, tbs), method = 'lm', se = FALSE,
                  linetype = 'dashed', size = 0.7)
    x_lab = 'PH Inferior'
  } else {
    stop('Wrong metric!')
  }
  plot = plot +
    scale_shape_manual(values = c(4, 16, 17)) +
    scale_colour_hue() +
    labs(title = paste('Relação TBSm e', x_lab), colour = 'ZBB:') +
    xlab(paste(x_lab, '(%)')) +
    ylab('TBSm (°C)') +
    theme(plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13))
  
  
}

# define characteristics to save the plot
SavePlot = function(plot, plot_name, output_dir) {
  # plot: plot variable
  # in 'PlotHist' and 'PlotBP' functions it's defined as a variable called 'plot'
  # plot_name: file name (without extension)
  # lx: plot width
  # ly: plot height
  # output_dir: output directory
  
  # workflow to plot graphs automatically
  # first call 'png' function, than define ask to plot, with 'plot' function and finalize close the
  # process with 'dev.off' function
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

# main function ####
WeatherAnalysis = function(metric, load_results, results_path,
                           inmet_path, save_plot, output_dir, data_path) {
  
  metric = 'phft'
  load_results = TRUE
  results_path = '/home/rodox/git/nbr_15575/results_uni.csv'
  inmet_path = '/home/rodox/git/nbr_15575/inmet_tbs_zb.csv'
  save_plot = TRUE
  output_dir = '/home/rodox/git/nbr_15575/plot_table/'
  
  inmet = read.csv(inmet_path, stringsAsFactors = FALSE)
  if (load_results) {
    results = read.csv(results_path)
  } else {
    load(data_path)
    results = mapply(CalcResults, dfs_list, names(dfs_list), inmet$tbs,
                     SIMPLIFY = TRUE)
    results = as.data.frame(results)
    results = as.data.frame(t(results), row.names = FALSE)
    results = apply(results, 2, as.numeric)
    results = round(results, 1)
    results = cbind(inmet, results)
  }
  plot = PlotScatter(results, metric, output_dir)
  if (save_plot) {
    plot_name = paste0('weather_analysis_', metric)
    SavePlot(plot, plot_name, output_dir)
  } else {
    return(plot)
  }
}

# application
WeatherAnalysis(metric = 'phft', load_results = TRUE,
                results_path = '/home/rodox/git/nbr_15575/results_uni.csv',
                inmet_path = '/home/rodox/git/nbr_15575/inmet_tbs_zb.csv',
                save_plot = TRUE, output_dir = '/home/rodox/git/nbr_15575/plot_table/')


