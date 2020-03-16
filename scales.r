# typographic conventions ####
# variables: underscore separated (e.g. df_output)
# functions: initial upper camel case (e.g. HighPHFT, FixDF)

# load libraries ####
pkgs = c('dplyr', 'stringr', 'ggplot2')
lapply(pkgs, library, character.only = T)

# auxiliar functions ####
PickHighPHFT = function(df) {
  high_phft = filter(df, phft > phft_ref)
  return(high_phft)
}

PickLowCgTT = function(df) {
  med = median
  low_cgtt = filter(PickHighPHFT(df), cgtt < median(cgtt_ref))
  return(low_cgtt)
}

# data mining functions ####
ShrinkGeom = function(df) {
  df$geometria = ifelse(str_detect(df$geometria, '0.txt'), 'P',
                        ifelse(str_detect(df$geometria, '1.txt'), 'M', 'G'))
  return(df)
}

FixDF = function(df, dwel, area, unit = 'kwh') {
  df$phft = df$phft*100
  df$phft_ref = df$phft_ref*100
  unit = ifelse(unit == 'kwh', 3600000, 1000)
  if (dwel == 'uni') {
    df$area = ifelse(df$geometria == 'P', area*1,
                     ifelse(df$geometria == 'M', area*1.5, area*2))
  } else {
    df$area = ifelse(df$geometria == 'P',
                     ifelse(df$uh_expo == 'CANTO', area[1]*1, area[2]*1),
                     ifelse(df$uh_expo == 'CANTO', area[1]*1.5, area[2]*1.5))
    df$floor = ifelse(df$floor == 'CO', 'Cob.',
                      ifelse(df$floor == 'TP0', 'Tipo', 'Térreo'))
    df$floor = factor(df$floor, levels = c('Cob.', 'Tipo', 'Térreo'))
  }
  df$cgtt = (df$cgtr_cooling + df$cgtr_heating)/(df$area*unit)
  df$cgtt_ref = (df$cgtr_cooling_ref + df$cgtr_heating_ref)/(df$area*unit)
  df$estado = factor(df$estado, levels = c('RS', 'SC', 'PR', 'RJ', 'MG', 'GO', 'TO', 'MA'))
  return(df)
}

# statistics and plot functions ####
CalcStats = function(lvl, df, weather) {
  if (lvl == 'intermediario') {
    summ_table = df %>%
      filter(estado == weather) %>%
      group_by(geometria, floor) %>%
      PickHighPHFT() %>%
      summarize('min' = min(phft),
                '5_percent' = quantile(phft, probs = c(0.05), names = F),
                '1_quart' = quantile(phft, probs = c(0.25), names = F),
                'mean' = mean(phft),
                'median' = median(phft),
                '3_quart' = quantile(phft, probs = c(0.75), names = F),
                '95_percent' = quantile(phft, probs = c(0.95), names = F),
                'max' = max(phft)) %>%
      as.data.frame()
    summ_table[, 3:10] = round(summ_table[, 3:10], 1)
  } else {
    summ_table = df %>%
      filter(estado == weather) %>%
      group_by(geometria, floor) %>%
      PickLowCgTT() %>%
      summarize('min' = min(cgtt),
                '5_percent' = quantile(cgtt, probs = c(0.05), names = F),
                '1_quart' = quantile(cgtt, probs = c(0.25), names = F),
                'mean' = mean(cgtt),
                'median' = median(cgtt),
                '3_quart' = quantile(cgtt, probs = c(0.75), names = F),
                '95_percent' = quantile(cgtt, probs = c(0.95), names = F),
                'max' = max(cgtt)) %>%
      as.data.frame()
    summ_table[, 3:10] = round(summ_table[, 3:10], 1)
  }
  return(summ_table)
}

PlotHist = function(lvl, df, dwel, save_plot, lx, ly, output_dir) {
  plot = ggplot(data = df)
  
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1, scales = 'free')
    bw = ifelse(lvl == 'intermediario', 2.5, 20)
  } else {
    plot = plot + facet_grid(floor ~ estado, scales = 'free')
    bw = ifelse(lvl == 'intermediario', 2.5, 15)
  }
  
  if (lvl == 'intermediario') {
    vl_df = df %>%
      PickHighPHFT %>%
      group_by(estado, floor) %>%
      summarise(median = median(phft))
    plot = plot + geom_histogram(aes(x = phft, group = geometria, colour = geometria),
                                 alpha = 0.5, fill = 'white') +
      geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
      labs(x = 'PHFT (%)')
  } else {
    vl_df = df %>%
      PickLowCgTT() %>%
      group_by(estado, floor) %>%
      summarise(median = median(cgtt))
    plot = plot + geom_histogram(aes(x = cgtt, group = geometria, colour = geometria),
                                 alpha = 0.5, fill = 'white') +
      geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
      labs(x = 'CgTT (kWh/m²)')
  }
  
  plot = plot +
    labs(title = paste0('Histograma - ', str_to_title(dwel), '. - ',
                       ifelse(lvl == 'intermediario', 'Intermediário', 'Superior')),
         y = 'Contagem', colour = 'Área:') +
    scale_shape_manual(values = c(4, 19)) +
    theme(plot.title = element_text(size = 19, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = 'right',
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 13, angle = 75, hjust = 1),
          axis.text.y = element_text(size = 13),
          strip.text.x = element_text(size = 17),
          strip.text.y = element_text(size = 17))
  
  if (save_plot == T) {
    SavePlot(plot, paste0(dwel, '_', lvl, '_hist'), lx, ly, output_dir)
  } else {
    return(plot)
  }
}

PlotBP = function(lvl, df, dwel, save_plot, lx, ly, output_dir) {
  if (lvl == 'intermediario') {
    plot = ggplot(data = PickHighPHFT(df),
                  aes(x = geometria, y = phft, group = geometria, colour = geometria)) +
      labs(y = 'PHFT (%)')
  } else {
    plot = ggplot(data = PickLowCgTT(df),
                  aes(x = geometria, y = cgtt, group = geometria, colour = geometria)) +
      labs(y = 'CgTT (kWh/m²)')
  }
  
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1)
  } else {
    plot = plot + facet_grid(floor ~ estado)
  }
  
  plot = plot +
    geom_boxplot() +
    labs(title = paste0('Box Plot - ', str_to_title(dwel), '. - ',
                       ifelse(lvl == 'intermediario', 'Intermediário', 'Superior')),
         colour = 'Área:') +
    scale_shape_manual(values = c(4, 19)) +
    theme(plot.title = element_text(size = 19, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = 'right',
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 14),
          strip.text.x = element_text(size = 17),
          strip.text.y = element_text(size = 17))
  
  if (save_plot == T) {
    SavePlot(plot, paste0(dwel, '_', lvl, '_bp'), lx, ly, output_dir)
  } else {
    return(plot)
  }
}

SavePlot = function(plot, plot_name, lx, ly, output_dir) {
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = lx, height = ly, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

# main functions ####
CreateScales = function(item, df, dwel, save_plot, lx = 33.8, ly = 19, output_dir) {
  names = c(sort(levels(df[, 'estado'])), 'PLOTS')
  item = vector('list', length = length(names))
  item = lapply(names, DefPerformance, df, dwel)
  names(item) = names
  return(item)
}

DefPerformance = function(weather, df, dwel) {
  item = vector('list', length = 2)
  names = c('intermediario', 'superior')
  if (weather != 'PLOTS') {
    item = lapply(names, CalcStats, df, weather)
  } else {
    item = lapply(names, DefPlots, df, dwel)
  }
  names(item) = names
  return(item)
}

DefPlots = function(lvl, df, dwel) {
  item = vector('list', length = 2)
  names(item) = c('hist', 'bp')
  item$hist = PlotHist(lvl, df, dwel, save_plot = F, lx, ly, output_dir)
  item$bp = PlotBP(lvl, df, dwel, save_plot = F, lx, ly, output_dir)
  return(item)
}

# main code ####
load('/home/rodox/00.git/04.nbr_15575/outputs.RData')
list_dfs = lapply(list_dfs, ShrinkGeom)
list_dfs = mapply(FixDF, list_dfs, names(list_dfs),
                  list(38.58, c(34.72, 33.81)),
                  SIMPLIFY = F)
scales = vector('list', length = length(list_dfs))
names(scales) = names(list_dfs)
scales = mapply(CreateScales, scales, list_dfs, names(scales),
                save_plot = F, SIMPLIFY = F)

# to save the plots aditional arguments should be used in scales(), as follow
# plot width (lx) = 33.8 / plot hight (ly) = 19 / output directory (output_dir) must be filled
# scales = mapply(CreateScales, scales, list_dfs, names(scales), save_plot = T,
#                 33.8, 19, '/home/rodox/00.git/04.nbr_15575/00.plots/', SIMPLIFY = F)

