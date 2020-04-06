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
pkgs = c('dplyr', 'ggplot2', 'stringr')
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
# define superior performance cases based on cgtt's absolute reduction
PickHighRedAbsCgTT = function(df) filter(PickHighPHFT(df), red_cgtt > median(red_cgtt))
# define superior performance cases based on cgtt's relative reduction
PickHighRedRelCgTT = function(df) filter(PickHighPHFT(df), red_rel_cgtt > median(red_rel_cgtt))
# create a data frame with levels vertical lines for histogram plots
DefVertLinesDF = function(df, lvl, red) {
  vl_df = df %>%
    group_by(estado, floor)
  if (lvl != 'superior') {
    vl_df = vl_df %>%
      RmLowPerf() %>%
      summarise(min = min(phft), median = median(phft))
  } else {
    vl_df = PickHighPHFT(vl_df)
    if (is.null(red)) {
      vl_df = summarise(vl_df, median = median(cgtt))
    } else {
      if (red == 'abs') {
        vl_df = summarise(vl_df, median = median(red_cgtt))
      } else {
        vl_df = summarise(vl_df, median = median(red_rel_cgtt))
      }
    }
  }
  return(vl_df)
}

NamePlot = function(dwel, lvl, red) {
  plot_name = paste0(dwel, '_', lvl,
                     ifelse(lvl != 'superior', '_',
                            ifelse(is.null(red), '_',
                                   paste0('_red', ifelse(red == 'abs', '_abs_',
                                                         '_rel_')))))
                     
  return(plot_name)
}

NameTitle = function(dwel, lvl, red) {
  title = paste0(str_to_title(dwel), '. - Nível ', str_to_title(lvl),
                 ifelse(lvl == 'intermediario', '',
                        ifelse(is.null(red), '',
                               paste0(' (Redução',
                                      ifelse(red == 'abs', ' Absoluta)',
                                             ' Relativa)')))))
  
  return(title)
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
  df$red_rel_cgtt = df$red_cgtt/df$cgtt_ref*100
  df$estado = factor(df$estado, levels = c('RS', 'SC', 'PR', 'RJ', 'MG', 'GO', 'TO', 'MA'))
  return(df)
}

# statistics and plot functions ####
CalcStats = function(lvl, df, weather) {
  summ_table = df %>%
    filter(estado == weather) %>%
    group_by(geometria, floor)
  if (lvl == 'intermediario') {
    summ_table = summ_table %>%
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
    summ_table = summ_table %>%
      PickHighPHFT() %>%
      summarize('min_cgtt' = min(cgtt),
                'min_red_abs' = min(red_cgtt),
                'min_red_rel' = min(red_rel_cgtt),
                'mean_cgtt' = mean(cgtt),
                'mean_red_abs' = mean(red_cgtt),
                'mean_red_rel' = mean(red_rel_cgtt),
                'median_cgtt' = median(cgtt),
                'median_red_abs' = median(red_cgtt),
                'median_red_rel' = median(red_rel_cgtt),
                'max_cgtt' = max(cgtt),
                'max_red_abs' = max(red_cgtt),
                'max_red_rel' = max(red_rel_cgtt)) %>%
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
    vl_df = DefVertLinesDF(df, lvl)
    plot = plot + geom_histogram(aes(x = phft, colour = geometria),
                                 fill = 'white') +
      geom_vline(data = vl_df, aes(xintercept = min), linetype = 'dashed') +
      geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
      geom_text(data = vl_df, aes(x = min, y = Inf, vjust = 2,
                                  label = '       M->', fontface = 'bold')) +
      geom_text(data = vl_df, aes(x = median, y = Inf, vjust = 2,
                                  label = '     I->', fontface = 'bold')) +
      labs(x = 'PHFT (%)')
  } else {
    vl_df = DefVertLinesDF(df, lvl, red)
    if (is.null(red)) {
      plot = plot + geom_histogram(aes(x = cgtt, colour = geometria),
                                   alpha = 0.5, fill = 'white') +
        labs(x = 'CgTT (kWh/m²)') +
        geom_text(data = vl_df, aes(x = median, y = Inf, vjust = 2, fontface = 'bold',
                                    label = '       <-S'))
    } else {
      if (red == 'abs') {
        plot = plot + geom_histogram(aes(x = red_cgtt, colour = geometria),
                                     alpha = 0.5, fill = 'white') +
          labs(x = 'Red. Abs. CgTT (kWh/m²)')
      } else {
        plot = plot + geom_histogram(aes(x = red_rel_cgtt, colour = geometria),
                                     alpha = 0.5, fill = 'white') +
          labs(x = 'Red. Rel. CgTT (%)')
      }
      plot = plot +
        geom_text(data = vl_df, aes(x = median, y = Inf, vjust = 2, fontface = 'bold',
                                    label = '       S->'))
    }
    plot = plot + geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed')
  }
  
  title_name = NameTitle(dwel, lvl, red)
  plot = plot +
    labs(title = paste0(title_name, ' - Histograma'),
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
    plot_name = NamePlot(dwel, lvl, red)
    SavePlot(plot, paste0(plot_name, 'hist'), lx, ly, output_dir)
  } else {
    return(plot)
  }
}

PlotBP = function(lvl, df, dwel, red, save_plot, lx, ly, output_dir) {
  if (lvl == 'intermediario') {
    plot = ggplot(data = RmLowPerf(group_by(df, estado, floor)),
                  aes(x = geometria, y = phft, fill = geometria)) +
      labs(y = 'PHFT (%)')
  } else {
    if (is.null(red)) {
      plot = ggplot(data = PickHighPHFT(group_by(df, estado, floor)),
                    aes(x = geometria, y = cgtt, fill = geometria)) +
        labs(y = 'CgTT (kWh/m²)')
    } else {
      if (red == 'abs') {
        plot = ggplot(data = PickHighPHFT(group_by(df, estado, floor)),
                      aes(x = geometria, y = red_cgtt, fill = geometria)) +
          labs(y = 'Red. Abs. CgTT (kWh/m²)')
      } else {
        plot = ggplot(data = PickHighPHFT(group_by(df, estado, floor)),
                      aes(x = geometria, y = red_rel_cgtt, fill = geometria)) +
          labs(y = 'Red. Rel. CgTT (%)')
      }
    }
  }
  
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1)
  } else {
    plot = plot + facet_grid(floor ~ estado)
  }
  
  title_name = NameTitle(dwel, lvl, red)
  plot = plot +
    geom_boxplot() +
    labs(title = paste0(title_name, ' - Box Plot'),
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
    plot_name = NamePlot(dwel, lvl, red)
    SavePlot(plot, paste0(plot_name, 'bp'), lx, ly, output_dir)
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
CreateScales = function(item, df, dwel, red, save_plot,
                        lx, ly, output_dir = '') {
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
scales = mapply(CreateScales, scales, dfs_list, names(scales), SIMPLIFY = F,
                MoreArgs = list(red = NULL, save_plot = F))

# to save the plots aditional arguments should be used in scales(), as follow:
  # plot width (lx) = 33.8 / plot hight (ly) = 19 / output directory (output_dir) must be filled
# scales = mapply(CreateScales, scales, dfs_list, names(scales), SIMPLIFY = F,
#                 MoreArgs = list(red = 'rel, save_plot = T, lx = 33.8, ly = 19,
#                                 output_dir = '~/00.git/01.nbr_15575/00.plots/'))
