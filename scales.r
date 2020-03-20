# instructions ####
# to run the code you need to fill the arguments of funtions in main code (last) section as follow:
  # 1st: load()
    # 1) .RData file path
  # 2nd: lapply()
    # 1) list of data frames containing the outputs of simulations, elaborated by marcelo olinger
    # 2) data mining user-defined function 'ShrinkGeom'
  # 3rd: mapply()
    # 1) data mining user-defined function 'FixDF'; 2) list of data frame (same as 2nd.1)
    # 3) vector with the names of the dweling types, exaclty like (c('uni', 'multi')) the names
      # of the list of data frame items
    # 4) list with floor areas for each dwelling (first item = area of uni, second
      # item = area of multi)
    # 5) SIMPLIFY = F guarantee that the output of mapply() is a list
  # 4th: mapply()
    # 1) main user-defined function 'CreateScales'
    # 2) list to save the outputs (it was created in main code, two lines before the 4th function)
    # 3) list of data frames (same as 2nd.1)
    # 3) vector with the names of the dweling types (same as 2nd.3)
    # 4) 'TRUE' for plots with reduction of cgtt / 'FALSE' for plots with absolute cgtt
    # 5) 'TRUE' to save plots / 'FALSE' to concatenate plots inside the outputs list (scales)
      # Obs.1: if 5 is 'FALSE', there is no need to use arguments 6 ~ 8
      # Obs.2: SIMPLIFY (argument 9) is always necessary!
    # 6) plot's width (default is '33.8' cm)
    # 7) plot's hight (default is '19' cm)
    # 8) output directory
    # 9) SIMPLIFY = F guarantee that the output of mapply() is a list

# typographic conventions ####
# variables: underscore separated (e.g. df_output)
# functions: initial upper camel case (e.g. HighPHFT, FixDF)

# load libraries ####
pkgs = c('dplyr', 'stringr', 'ggplot2')
lapply(pkgs, library, character.only = T)

# auxiliar functions ####
# remove cases lower than minimum performance
RmLowPerf = function(df) df %>%
  filter(
    phft > phft_ref &
      t_max < t_max_ref &
        !((estado == 'PR' | estado == 'RS' | estado == 'SC') & t_min < t_min_ref)
  )
# define cases higher than intermediary performance
PickHighPHFT = function(df) filter(RmLowPerf(df), phft > median(phft))
# define superior performance cases based on absolute cgtt
PickLowCgTT = function(df) filter(PickHighPHFT(df), cgtt < median(cgtt))
# define superior performance cases based on cgtt's reduction
PickHighRedCgTT = function(df) filter(PickHighPHFT(df), red_cgtt > median(red_cgtt))

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
    df$area = ifelse(df$geometria == 'P', area,
                     ifelse(df$geometria == 'M', 1.5*area, 2*area))
  } else {
    df$area = ifelse(df$geometria == 'P',
                     ifelse(df$uh_expo == 'CANTO', area[1], area[2]),
                     ifelse(df$uh_expo == 'CANTO', 1.5*area[1], 1.5*area[2]))
    df$floor = ifelse(df$floor == 'CO', 'Cob.',
                      ifelse(df$floor == 'TP0', 'Tipo', 'Térreo'))
    df$floor = factor(df$floor, levels = c('Cob.', 'Tipo', 'Térreo'))
  }
  df$cgtt = (df$cgtr_cooling + df$cgtr_heating)/(df$area*unit)
  df$cgtt_ref = (df$cgtr_cooling_ref + df$cgtr_heating_ref)/(df$area*unit)
  df$red_cgtt = df$cgtt_ref - df$cgtt
  df$estado = factor(df$estado, levels = c('RS', 'SC', 'PR', 'RJ', 'MG', 'GO', 'TO', 'MA'))
  return(df)
}

# statistics and plot functions ####
CalcStats = function(lvl, df, weather) {
  if (lvl == 'intermediario') {
    summ_table = df %>%
      filter(estado == weather) %>%
      group_by(geometria, floor) %>%
      RmLowPerf() %>%
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
      PickHighPHFT() %>%
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

PlotHist = function(lvl, df, dwel, red, save_plot, lx, ly, output_dir) {
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
      group_by(estado, floor) %>%
      RmLowPerf() %>%
      summarise(median = median(phft))
    plot = plot + geom_histogram(aes(x = phft, colour = geometria),
                                 alpha = 0.5, fill = 'white') +
      geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
      labs(x = 'PHFT (%)')
  } else {
    vl_df = df %>%
      group_by(estado, floor) %>%
      PickHighPHFT()
    if (red) {
      vl_df = summarise(vl_df, median = median(red_cgtt))
      plot = plot + geom_histogram(aes(x = red_cgtt, colour = geometria),
                                   alpha = 0.5, fill = 'white') +
        geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
        labs(x = 'Red. CgTT (kWh/m²)')
    } else {
      vl_df = summarise(vl_df, median = median(cgtt))
      plot = plot + geom_histogram(aes(x = cgtt, colour = geometria),
                                   alpha = 0.5, fill = 'white') +
        geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
        labs(x = 'CgTT (kWh/m²)')
    }
  }
  
  plot = plot +
    labs(title = paste0(str_to_title(dwel), '. - Nível ',
                       ifelse(lvl == 'intermediario', 'Intermediário', 'Superior'),
                       ' - Histograma'),
         y = 'Contagem', colour = 'Área:') +
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

  if (save_plot) {
    SavePlot(plot, paste0(dwel, '_', lvl,
                          ifelse(lvl == 'superior' & red, '_red_hist', '_hist')),
             lx, ly, output_dir)
  } else {
    return(plot)
  }
}

PlotBP = function(lvl, df, dwel, red, save_plot, lx, ly, output_dir) {
  if (lvl == 'intermediario') {
    plot = ggplot(data = PickHighPHFT(group_by(df, estado, floor)),
                  aes(x = geometria, y = phft, fill = geometria)) +
      labs(y = 'PHFT (%)')
  } else {
    if (red) {
      plot = ggplot(data = PickHighRedCgTT(group_by(df, estado, floor)),
                    aes(x = geometria, y = red_cgtt, fill = geometria)) +
        labs(y = 'Red. CgTT (kWh/m²)')
    } else {
      plot = ggplot(data = PickLowCgTT(group_by(df, estado, floor)),
                    aes(x = geometria, y = cgtt, fill = geometria)) +
        labs(y = 'CgTT (kWh/m²)')
    }
  }
  
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1)
  } else {
    plot = plot + facet_grid(floor ~ estado)
  }
  
  plot = plot +
    geom_boxplot() +
    labs(title = paste0(str_to_title(dwel), '. - Nível ',
                       ifelse(lvl == 'intermediario', 'Intermediário', 'Superior'),
                       ' - Box Plot'),
         fill = 'Área:') +
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
  
  if (save_plot) {
    SavePlot(plot, paste0(dwel, '_', lvl,
                          ifelse(lvl == 'superior' & red, '_red_bp', '_bp')),
             lx, ly, output_dir)
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
CreateScales = function(item, df, dwel, red = T, save_plot = F,
                        lx = 33.8, ly = 19, output_dir = '') {
  names = c(levels(df[, 'estado']), 'PLOTS')
  item = vector('list', length = length(names))
  item = lapply(names, DefPerformance, df, dwel, red,
                save_plot, lx, ly, output_dir)
  names(item) = names
  return(item)
}

DefPerformance = function(weather, df, dwel, red, save_plot,
                          lx, ly, output_dir) {
  item = vector('list', length = 2)
  names = c('intermediario', 'superior')
  if (weather != 'PLOTS') {
    item = lapply(names, CalcStats, df, weather)
  } else {
    item = lapply(names, DefPlots, df, dwel, red,
                  save_plot, lx, ly, output_dir)
  }
  names(item) = names
  return(item)
}

DefPlots = function(lvl, df, dwel, red, save_plot,
                    lx, ly, output_dir) {
  item = vector('list', length = 2)
  names(item) = c('hist', 'bp')
  item$hist = PlotHist(lvl, df, dwel, red, save_plot,
                       lx, ly, output_dir)
  item$bp = PlotBP(lvl, df, dwel, red, save_plot,
                   lx, ly, output_dir)
  return(item)
}

# main code ####
load('/home/rodox/00.git/01.nbr_15575/outputs.RData')
dfs_list = lapply(dfs_list, ShrinkGeom)
dfs_list = mapply(FixDF, dfs_list, names(dfs_list),
                  list(38.58, c(34.72, 33.81)),
                  SIMPLIFY = F)
scales = vector('list', length = length(dfs_list))
names(scales) = names(dfs_list)
scales = mapply(CreateScales, scales, dfs_list, names(scales), SIMPLIFY = F)

# to save the plots aditional arguments should be used in scales(), as follow
# plot width (lx) = 33.8 / plot hight (ly) = 19 / output directory (output_dir) must be filled
# scales = mapply(CreateScales, scales, dfs_list, names(scales), red = T, save_plot = T,
#                 33.8, 19, '/home/rodox/Desktop/', SIMPLIFY = F)