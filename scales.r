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
# remove cases whose cgtt is higher than cgtt reference
RmHighCgTT = function(df) filter(RmLowPerf(df), cgtt < cgtt_ref)
# define cases higher than intermediary performance
FiltPHFT = function(df, inc) {
  df = RmHighCgTT(df)
  if (inc == F) {
    df = filter(df, phft > median(phft))
  } else if(inc == 'abs') {
    df = filter(df, inc_phft > median(inc_phft))
  } else {
    df = filter(RmHighCgTT(df), inc_rel_phft > median(inc_rel_phft))
  }
  return(df)
}
# create a data frame with levels vertical lines for histogram plots
DefVertLinesDF = function(df, lvl, inc, red) {
  vl_df = df %>%
    group_by(estado, floor)
  if (lvl == 'intermediario') {
    if (inc == F) {
      vl_df = vl_df %>%
        RmLowPerf() %>%
        summarise(min = min(phft), median = median(phft))
    } else {
      vl_df = RmHighCgTT(vl_df)
      if (inc == 'abs') {
        vl_df = summarise(vl_df, median = median(inc_phft))
      } else {
        vl_df = summarise(vl_df, median = median(inc_rel_phft))
      }
    }
  } else {
    vl_df = FiltPHFT(vl_df, inc)
    if (red == F) {
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
# name plot files
NamePlot = function(dwel, lvl, inc, red) {
  plot_name = paste0(dwel, '_', lvl,
                     ifelse(inc == F, '', paste0('_inc_', inc)),
                     ifelse(lvl == 'intermediario', '', ifelse(red == F, '',
                                                               paste0('_red_', red))))
  return(plot_name)
}
# name plot titles
NameTitle = function(dwel, lvl, inc, red) {
 title = paste0(str_to_title(dwel), '. - Nível ', str_to_title(lvl),
                 ifelse(lvl == 'intermediario',
                        ifelse(inc == F, '', paste0(' (Aumento ', str_to_title(inc), '.)')),
                        paste0(ifelse(red == F, '',
                               paste0(' (Redução ', str_to_title(red), '.)')),
                        ifelse(inc == F,
                               '', paste0(' [PHFT ', str_to_title(inc), '.]')))))
  return(title)
}

# data mining functions ####
ShrinkGeom = function(df) {
  df$geometria = ifelse(str_detect(df$geometria, '0.txt'), 'P',
                        ifelse(str_detect(df$geometria, '1.txt'), 'M', 'G'))
  return(df)
}

FixDF = function(df, dwel, area, unit = 'kwh') {
  unit = ifelse(unit == 'kwh', 3600000, 1000)
  df$phft = df$phft*100
  df$phft_ref = df$phft_ref*100
  df$inc_phft = df$phft - df$phft_ref
  df$inc_rel_phft = df$inc_phft/df$phft_ref*100
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
CalcStats = function(lvl, df, inc, weather) {
  summ_table = df %>%
    filter(estado == weather) %>%
    group_by(geometria, floor)
  if (lvl == 'intermediario') {
    summ_table = summ_table %>%
      RmLowPerf() %>%
      summarize('min_phft' = min(phft),
                'min_inc_phft' = min(inc_phft),
                'min_inc_rel_phft' = min(inc_rel_phft),
                'mean_phft' = mean(phft),
                'mean_inc_phft' = mean(inc_phft),
                'mean_inc_rel_phft' = mean(inc_rel_phft),
                'median_phft' = median(phft),
                'median_inc_phft' = median(inc_phft),
                'median_inc_rel_phft' = median(inc_rel_phft),
                'max_phft' = max(phft),
                'max_inc_phft' = max(inc_phft),
                'max_inc_rel_phft' = max(inc_rel_phft)) %>%
      as.data.frame()
    summ_table[, 3:10] = round(summ_table[, 3:10], 1)
  } else {
    summ_table = summ_table %>%
      FiltPHFT(inc) %>%
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

PlotHist = function(lvl, df, dwel, inc, red, save_plot, lx, ly, output_dir) {
  plot = ggplot(data = df)
  
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1, scales = 'free')
    bw = ifelse(lvl == 'intermediario', 2.5, 20)
  } else {
    plot = plot + facet_grid(floor ~ estado, scales = 'free')
    bw = ifelse(lvl == 'intermediario', 2.5, 15)
  }
  
  if (lvl == 'intermediario') {
    vl_df = DefVertLinesDF(df, lvl, inc)
    if (inc == F) {
      plot = plot + geom_histogram(aes(x = phft, colour = geometria),
                                   fill = 'white') +
        geom_vline(data = vl_df, aes(xintercept = min), linetype = 'dashed') +
        geom_text(data = vl_df, aes(x = min, y = Inf, vjust = 2,
                                    label = '       M->', fontface = 'bold')) +
        labs(x = 'PHFT (%)')
    } else {
      if (inc == 'abs') {
        plot = plot + geom_histogram(aes(x = inc_phft, colour = geometria),
                                     fill = 'white') +
          labs(x = 'Red. Abs. PHFT (%)')
      } else {
        plot = plot + geom_histogram(aes(x = inc_rel_phft, colour = geometria),
                                     fill = 'white') +
          labs(x = 'Red. Rel. PHFT (%)')
      }
      plot = plot +
        geom_vline(xintercept = 0, linetype = 'dashed') +
        geom_text(data = vl_df, aes(x = 0, y = Inf, vjust = 2,
                                    label = '       M->', fontface = 'bold'))
    }
    plot = plot +
      geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
      geom_text(data = vl_df, aes(x = median, y = Inf, vjust = 2,
                                  label = '     I->', fontface = 'bold'))
  } else {
    vl_df = DefVertLinesDF(df, lvl, inc, red)
    if (red == F) {
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
  
  title_name = NameTitle(dwel, lvl, inc, red)
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
    plot_name = NamePlot(dwel, lvl, inc, red)
    SavePlot(plot, paste0(plot_name, '_hist'), lx, ly, output_dir)
  } else {
    return(plot)
  }
}

PlotBP = function(lvl, df, dwel, inc, red, save_plot, lx, ly, output_dir) {
  if (lvl == 'intermediario') {
    plot = ggplot(data = RmLowPerf(group_by(df, estado, floor)))
    if (inc == F) {
      plot = plot +
        geom_boxplot(aes(x = geometria, y = phft, fill = geometria)) +
        labs(y = 'PHFT (%)')
    } else if (inc == 'abs') {
      plot = plot +
        geom_boxplot(aes(x = geometria, y = inc_phft, fill = geometria)) +
        labs(y = 'Aumento Abs. PHFT (%)')
    } else {
      plot = plot +
        geom_boxplot(aes(x = geometria, y = inc_rel_phft, fill = geometria)) +
        labs(y = 'Aumento Rel. PHFT (%)')
    }
  } else {
    plot = ggplot(data = FiltPHFT(group_by(df, estado, floor), inc))
    if (red == F) {
      plot = plot +
        geom_boxplot(aes(x = geometria, y = cgtt, fill = geometria)) +
        labs(y = 'CgTT (kWh/m²)')
    } else {
      if (red == 'abs') {
         plot = plot +
          geom_boxplot(aes(x = geometria, y = red_cgtt, fill = geometria)) +
          labs(y = 'Red. Abs. CgTT (kWh/m²)')
      } else {
        plot = plot +
          geom_boxplot(aes(x = geometria, y = red_rel_cgtt, fill = geometria)) +
          labs(y = 'Red. Rel. CgTT (%)')
      }
    }
  }
  
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1)
  } else {
    plot = plot + facet_grid(floor ~ estado)
  }
  
  title_name = NameTitle(dwel, lvl, inc, red)
  plot = plot +
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
    plot_name = NamePlot(dwel, lvl, inc, red)
    SavePlot(plot, paste0(plot_name, '_bp'), lx, ly, output_dir)
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
CreateScales = function(item, df, dwel, inc, red, save_plot,
                        lx, ly, output_dir = '') {
  names = c(levels(df[, 'estado']), 'PLOTS')
  item = vector('list', length = length(names))
  item = lapply(names, DefPerformance, df, dwel, inc,
                red, save_plot, lx, ly, output_dir)
  names(item) = names
  return(item)
}

DefPerformance = function(weather, df, dwel, inc, red,
                          save_plot, lx, ly, output_dir) {
  item = vector('list', length = 2)
  names = c('intermediario', 'superior')
  if (weather != 'PLOTS') {
    item = lapply(names, CalcStats, df, inc, weather)
  } else {
    item = lapply(names, DefPlots, df, dwel, inc, red,
                  save_plot, lx, ly, output_dir)
  }
  names(item) = names
  return(item)
}

DefPlots = function(lvl, df, dwel, inc, red,
                    save_plot, lx, ly, output_dir) {
  item = vector('list', length = 2)
  names(item) = c('hist', 'bp')
  item$hist = PlotHist(lvl, df, dwel, inc, red,
                       save_plot, lx, ly, output_dir)
  item$bp = PlotBP(lvl, df, dwel, inc, red,
                   save_plot, lx, ly, output_dir)
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
                MoreArgs = list(inc = F, red = F, save_plot = T, lx = 33.8, ly = 19,
                                output_dir = '~/00.git/01.nbr_15575/00.plots/'))
