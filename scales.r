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
# select cases higher than the minimum performance level
RmLowPerf = function(df) df %>%
  # df: data frame with cases
  
  filter(
    phft > phft_ref & # choose only cases with phft higher than the reference
      (t_max < t_max_ref + 0.5) & # choose only cases with highest temperature lower than reference
        !((estado == 'PR' | estado == 'RS' | estado == 'SC') & (t_min < t_min_ref - 0.5))
          # remove cases from zb1 to zb3 whose minimum temperature is lower than reference
  )
# select cases with cgtt lower the reference
RmHighCgTT = function(df) filter(df, cgtt < cgtt_ref)
  # df: data frame with cases

# select cases higher than intermediary performance level
FiltPHFT = function(df, inc) {
  # df: data frame with cases
  # inc: if the intermediary level depends on absolute or relative values
    # possible values: 'FALSE' (intermediary level scale depends on phft's absolute value), 'abs'
      # (the intermediary level scale depends on phft's absolute increase) and 'rel' (the
      # intermediary level depends on phft's relative increase)
  
  df = RmLowPerf(df) # first all it selects only cases higher than minimum performance
  if (inc == F) {
    df = filter(df, phft > median(phft)) # phft's absolute value
  } else if (inc == 'abs') {
    df = filter(df, inc_phft > median(inc_phft)) # phft's absolute increase
  } else {
    df = filter(df, inc_rel_phft > median(inc_rel_phft)) # phft's relative increase
  }
  df = RmHighCgTT(df)
  return(df)
}

# create a data frame with performance level threshold for all weathers
  # this data frame will be used to plot vertical lines on histograms
DefVertLinesDF = function(df, lvl, inc, red) {
  # df: data frame with cases
  # lvl: 'intermediario' or 'superior'
  # inc: if the intermediary level depends on absolute or relative values
  # red: if the superior level depends on absolute or relative values
    # just like the 'inc' variable, the possible values are: 'FALSE', 'abs' and 'rel'
  
  # create a data frame grouped by the variables 'estado' and 'floor'
  vl_df = df %>%
    group_by(estado, floor)
  if (lvl == 'intermediario') {
    # select the cases higher than minimum performance for each 'estado' and 'floor' (like a grid
      # combination) and apply the functions 'min' and 'mean' to data frames
    vl_df = RmLowPerf(vl_df)
    if (inc == F) {
      # select cases considering phft's absolute value
      vl_df = summarise(vl_df, min = min(phft), median = median(phft))
    } else {
      if (inc == 'abs') {
        # select cases considering phft's absolute increse
        vl_df = summarise(vl_df, median = median(inc_phft))
      } else {
        # select cases considering phft's relative increse
        vl_df = summarise(vl_df, median = median(inc_rel_phft))
      }
    }
  } else {
    # select the cases higher than intermediary performance for each 'estado' and 'floor' and apply
      # the functions 'min' and 'mean' to data frames
    vl_df = FiltPHFT(vl_df, inc)
    if (red == F) {
      # select cases considering cgtt's absolute value
      vl_df = summarise(vl_df, median = median(cgtt))
    } else {
      if (red == 'abs') {
        # select cases considering cgtt's absolute reduction
        vl_df = summarise(vl_df, median = median(red_cgtt))
      } else {
        # select cases considering cgtt's relative reduction
        vl_df = summarise(vl_df, median = median(red_rel_cgtt))
      }
    }
  }
  return(vl_df)
}

# name plot files
  # name plot files considering the way the scales were defined
NamePlot = function(dwel, lvl, inc, red) {
  # dwel: 'uni' or 'multi'
  # lvl: 'intermediario' or 'superior'
  # inc: FALSE, 'abs' or 'rel'
  # red: FALSE, 'abs' or 'rel'
  
  plot_name = paste0(dwel, '_', lvl,
                     ifelse(inc == F, '', paste0('_inc_', inc)),
                     ifelse(lvl == 'intermediario', '', ifelse(red == F, '',
                                                               paste0('_red_', red))))
  return(plot_name)
}

# name plot titles
  # name plot titles considering the way the scales were defined
NameTitle = function(dwel, lvl, inc, red) {
  # dwel: 'uni' or 'multi'
  # lvl: 'intermediario' or 'superior'
  # inc: FALSE, 'abs' or 'rel'
  # red: FALSE, 'abs' or 'rel'
  
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
# shrink geometry cases into 'P', 'M' or 'G', depending on the floor's area
ShrinkGeom = function(df) {
  # df: data frame with cases
  
  df$geometria = ifelse(str_detect(df$geometria, '0.txt'), 'P',
                        ifelse(str_detect(df$geometria, '1.txt'), 'M', 'G'))
  return(df)
}

# add increase and reduction values for phft and cgtt, areas and fix some variables of the original
  # data frame
FixDF = function(df, dwel, area, unit = 'kwh') {
  # df: data frame with cases
  # dwel: 'uni' or 'multi'
  # area: list with the areas of 'uni' and 'multi' cases
    # example: list(38.58, c(34.72, 33.81))
  # unit: output unit
    # possible values: kwh' or 'kj'
  
  # define cgtt's unit
  div = ifelse(unit == 'kwh', 3600000, 1000)
  # calculate phft's absolute and relative increase
  df$phft = df$phft*100
  df$phft_ref = df$phft_ref*100
  df$inc_phft = df$phft - df$phft_ref # absolute increase
  df$inc_rel_phft = df$inc_phft/df$phft_ref*100 # relative increase
  # define area each case
  if (dwel == 'uni') {
    df$area = ifelse(df$geometria == 'P', area,
                     ifelse(df$geometria == 'M', 1.5*area, 2*area))
  } else {
    df$area = ifelse(df$geometria == 'P',
                     ifelse(df$uh_expo == 'CANTO', area[1], area[2]),
                     ifelse(df$uh_expo == 'CANTO', 1.5*area[1], 1.5*area[2]))
    # fix labels to plot
    df$floor = ifelse(df$floor == 'CO', 'Cob.',
                      ifelse(df$floor == 'TP0', 'Tipo', 'Térreo'))
    # organize floor's levels
    df$floor = factor(df$floor, levels = c('Cob.', 'Tipo', 'Térreo'))
  }
  # calculate phft's absolute and relative reduction
  df$cgtt = ifelse(grepl('MA|TO', df$estado), df$cgtr_cooling,
                   df$cgtr_cooling + df$cgtr_heating)
  df$cgtt = df$cgtt/(df$area*div)
  df$cgtt_ref = ifelse(grepl('MA|TO', df$estado), df$cgtr_cooling_ref,
                       df$cgtr_cooling_ref + df$cgtr_heating_ref)
  df$cgtt_ref = df$cgtt_ref/(df$area*div)
  df$red_cgtt = df$cgtt_ref - df$cgtt # absolute reduction
  df$red_rel_cgtt = df$red_cgtt/df$cgtt_ref*100 # relative reduction
  return(df)
}

# statistic and plot functions ####
# calculate scales for an specific level and weather (for each geometry and floor)
  # it's not essencial to run the code itself, but it can help somehow
CalcStats = function(lvl, df, inc, weather) {
  # lvl: 'intermediario' or 'superior'
  # df: data frame with cases
  # weather: estado
  
  # create a data frame grouped by 'geometria' and 'floor', for an specific weather
  summ_table = df %>%
    filter(estado == weather) %>%
    group_by(geometria, floor)
  # remove cases lower the minimum performance and calculate:
    # min, mean, median and max values for absolute phft, absolute phft's increase and relative
      # phft's increase
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
    # just round the data frame
    summ_table[, 3:10] = round(summ_table[, 3:10], 1)
  # remove cases lower the intermediary performance and calculate:
    # min, mean, median and max values for absolute cgtt, absolute cgtt's increase and relative
      # cgtt's increase
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
    # just round the data frame
    summ_table[, 3:10] = round(summ_table[, 3:10], 1)
  }
  return(summ_table)
}

# generate a scales table file
GenScales <- function(df, dwel, inc, red, save_table, output_dir) {
  # df: data frame with cases
  # dwel: 'uni' or 'multi'
  # inc: 'FALSE', 'abs' or 'rel'
  # red: 'FALSE', 'abs' or 'rel'
  # save_table: 'TRUE' or 'FALSE' (the results are returned as variables in rstudio)
  # output_dir: output directory
  
  # group the data frame according to the variables 'estado' and 'floor'
  scales_tbl = group_by(df, estado, floor)
  # remove cases with performance lower than reference
  inter_tbl = RmLowPerf(scales_tbl)
  # calculate the intermediary performance threshold
  if (inc == FALSE) {
    inter_tbl = inter_tbl %>%
      summarize('PHFT' = median(phft)) %>% # using absolute value of phft
      as.data.frame()
  } else if (inc == 'abs') {
    inter_tbl = inter_tbl %>%
      summarize('ElevPHFT' = median(inc_phft)) %>% # using absolute value of phft's increase
      as.data.frame()
  } else {
    inter_tbl = inter_tbl %>%
      summarize('%ElevPHFT' = median(inc_rel_phft)) %>% # using relative value of phft's increase
      as.data.frame()
  }
  # calculate the superior performance threshold
  sup_tbl = FiltPHFT(scales_tbl, inc)
  if (red == FALSE) {
    sup_tbl = sup_tbl %>%
      summarize('CgTT' = median(cgtt)) %>% # using absolute value of cgtt
      as.data.frame()
  } else if (red == 'abs') {
    sup_tbl = sup_tbl %>%
      summarize('RedCgTT' = median(red_cgtt)) %>% # using absolute value of cgtt's reduction
      as.data.frame()
  } else {
    sup_tbl = sup_tbl %>%
      summarize('%RedCgTT' = median(red_rel_cgtt)) %>% # using relative value of cgtt's reduction
      as.data.frame()
  }
  # create final data frames with weather zones, floor, intermediary and superior performance
  if (dwel == 'uni') {
    # in this case it's floors are ignored, since it's a single storey building
    scales_tbl = cbind(inter_tbl[1], inter_tbl[3], sup_tbl[3])
    names(scales_tbl)[1] = 'ZB'
  } else {
    # in this case consider the bottom, intermediary and top floors
    scales_tbl = cbind(inter_tbl[1:2], inter_tbl[3], sup_tbl[3])
    names(scales_tbl)[1:2] = c('ZB', 'Pavimento')
  }
  # write or return the result
  if (save_table) { # saves a '.csv' file in the output directory
    write.csv(scales_tbl, file = paste0(output_dir, dwel, '_inc_', inc,
                                        '_red_', red, '_scales.csv'))
  } else { # return result into a variable
    return(scales_tbl)
  }
}

# plot histograms
PlotHist = function(lvl, df, dwel, inc, red, save_plot, lx, ly, output_dir) {
  # lvl: 'intermediario' or 'superior'
  # df: data frame with cases
  # dwel: 'uni' or 'multi'
  # inc: 'FALSE', 'abs' or 'rel'
  # red: 'FALSE', 'abs' or 'rel'
  # save_plot: TRUE or FALSE (the results are returned as variables in rstudio)
  # lx: plot width
  # ly: plot hight
  # output_dir: output directory
  
  # pre-process
    # add a column in the data frame to check if minimum performance is accomplished
  df$min = ifelse(df$phft > df$phft_ref & (df$t_max < df$t_max_ref + 0.5) &
                    !(grepl('PR|RS|SC', df$estado) & (df$t_min < df$t_min_ref - 0.5)),
                  '\nAtende', 'Não\natende')
  df$min = factor(df$min, levels = c('Não\natende', '\nAtende'))
  # define the df input as the plot data
  plot = ggplot(data = df)
  
  # create a facets considering the 'estado' and 'floor' variables
    # 'uni' uses facet wrap, because there is just one floor, while 'multi' uses facet grid
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1, scales = 'free')
    bw = ifelse(lvl == 'intermediario', 2.5, 20) # bw = bin's width
  } else {
    plot = plot + facet_grid(floor ~ estado, scales = 'free')
    bw = ifelse(lvl == 'intermediario', 2.5, 15) # bw = bin's width
  }
  # add histograms, vertical lines and line labels to plots
    # histograms depends on the full cases data frame (for each weather)
    # vertical lines are plotted according to the 'inc' argument (FALSE, 'abs' or 'rel')
  if (lvl == 'intermediario') {
    vl_df = DefVertLinesDF(df, lvl, inc) # call the function and create a vertical line data frame
    if (inc == F) { # here 'inc' == FALSE, thus it considered the absolute value of phft
      # add histogram with all the cases
      plot = plot + geom_histogram(aes(x = phft, colour = geometria, fill = min)) +
        scale_fill_manual(values = c('white', 'grey')) +
        # add minimum performance vertical lines
          # case with the lowest absolute phft, considering only cases higher than the minimum
            # performance
        geom_vline(data = vl_df, aes(xintercept = min), linetype = 'dashed') +
        # add minimum vertical lines' labels
        geom_text(data = vl_df, aes(x = min, y = Inf, vjust = 2,
                                    label = '       M->', fontface = 'bold')) +
        labs(x = 'PHFT (%)')
    } else {
      if (inc == 'abs') { # here 'inc' == 'abs', thus it considered the phft's absolute increase
        # add histogram with all the cases
        plot = plot + geom_histogram(aes(x = inc_phft, colour = geometria, fill = min)) +
          scale_fill_manual(values = c('grey', 'white')) +
          labs(x = 'Red. Abs. PHFT (%)')
      } else { # here 'inc' == 'rel', thus it considered the phft's relative increase
        # add histogram with all the cases
        plot = plot + geom_histogram(aes(x = inc_rel_phft, colour = geometria, fill = min)) +
          scale_fill_manual(values = c('grey', 'white')) +
          labs(x = 'Red. Rel. PHFT (%)')
      }
      # add minimum performance vertical lines
        # case with the highest phft's increase, considering only cases higher than the minimum
          # performance
      plot = plot +
        geom_vline(xintercept = 0, linetype = 'dashed') +
        # add minimum vertical lines' labels
        geom_text(data = vl_df, aes(x = 0, y = Inf, vjust = 2,
                                    label = '       M->', fontface = 'bold'))
    }
    # add intermediary performance vertical lines using the median
      # remember that vl_df depends on 'inc' argument (it has to be defined to run the function)
    plot = plot +
      labs(fill = 'Atende aos\ncritérios de\ndesempenho\nmínimo:') +
      geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed') +
      # add intermediary vertical lines' labels
      geom_text(data = vl_df, aes(x = median, y = Inf, vjust = 2,
                                  label = '     I->', fontface = 'bold'))
  } else {
    vl_df = DefVertLinesDF(df, lvl, inc, red) # call the function and create a vertical line data frame
    if (red == F) { # here 'red' == FALSE, thus it considered the absolute value of cgtt
      # add histogram with all the cases
      plot = plot + geom_histogram(aes(x = cgtt, colour = geometria),
                                   alpha = 0.5, fill = 'white') +
        labs(x = 'CgTT (kWh/m²)') +
        # add superior vertical lines' labels
        geom_text(data = vl_df, aes(x = median, y = Inf, vjust = 2, fontface = 'bold',
                                    label = '       <-S'))
    } else {
      if (red == 'abs') { # here 'red' == 'abs', thus it considered the cgtt's absolute reduction
        # add histogram with all the cases
        plot = plot + geom_histogram(aes(x = red_cgtt, colour = geometria),
                                     alpha = 0.5, fill = 'white') +
          labs(x = 'Red. Abs. CgTT (kWh/m²)')
      } else { # here 'red' == 'abs', thus it considered the cgtt's relative reduction
        # add histogram with all the cases
        plot = plot + geom_histogram(aes(x = red_rel_cgtt, colour = geometria),
                                     alpha = 0.5, fill = 'white') +
          labs(x = 'Red. Rel. CgTT (%)')
      }
      # add superior vertical lines' labels
      plot = plot +
        geom_text(data = vl_df, aes(x = median, y = Inf, vjust = 2, fontface = 'bold',
                                    label = '       S->'))
    }
    # add intermediary performance vertical lines
      # in this case vl_df depends on 'red' argument (it has to be defined to run the function)
    plot = plot + geom_vline(data = vl_df, aes(xintercept = median), linetype = 'dashed')
  }
  # final details
  title_name = NameTitle(dwel, lvl, inc, red) # call the function that create the plot title
  # plot theme and labels
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
  # if it's 'TRUE', it saves the plot, if it's not, it returns it as a variable
  if (save_plot) {
    plot_name = NamePlot(dwel, lvl, inc, red) # call the function that create the plot title
    SavePlot(plot, paste0(plot_name, '_hist'), lx, ly, output_dir) # call the function to save the plot
  } else {
    return(plot)
  }
}

# plot box plots
PlotBP = function(lvl, df, dwel, inc, red, save_plot, lx, ly, output_dir) {
  # lvl: 'intermediario' or 'superior'
  # df: data frame with cases
  # dwel: 'uni' or 'multi'
  # inc: 'FALSE', 'abs' or 'rel'
  # red: 'FALSE', 'abs' or 'rel'
  # save_plot: TRUE or FALSE (the results are returned as variables in rstudio)
  # lx: plot width
  # ly: plot hight
  # output_dir: output directory
  
  # create data frame grouped by 'estado' and 'floor' variables
  if (lvl == 'intermediario') {
    # the minimum performance thresholds are calculate for each 'estado' and 'floor' (like a grid)
      # and the cases with performance lower than minimum are removed from the plot
    plot = ggplot(data = RmLowPerf(group_by(df, estado, floor)))
    if (inc == F) { # here 'inc' == FALSE, thus it considered the absolute value of phft
      # add box plots
      plot = plot +
        geom_boxplot(aes(x = geometria, y = phft, fill = geometria)) +
        labs(y = 'PHFT (%)')
    } else if (inc == 'abs') { # here 'inc' == 'abs', thus it considered the phft's absolute increase
      # add box plots
      plot = plot +
        geom_boxplot(aes(x = geometria, y = inc_phft, fill = geometria)) +
        labs(y = 'Aumento Abs. PHFT (%)')
    } else { # here 'inc' == 'abs', thus it considered the phft's relative increase
      # add box plots
      plot = plot +
        geom_boxplot(aes(x = geometria, y = inc_rel_phft, fill = geometria)) +
        labs(y = 'Aumento Rel. PHFT (%)')
    }
  } else {
    # the intermediary performance thresholds are calculate for each 'estado' and 'floor' (like a
      # grid) and the cases with performance lower than intermediary are removed from the plot
    plot = ggplot(data = FiltPHFT(group_by(df, estado, floor), inc))
    if (red == F) { # here 'red' == FALSE, thus it considered the absolute value of cgtt
      # add box plots
      plot = plot +
        geom_boxplot(aes(x = geometria, y = cgtt, fill = geometria)) +
        labs(y = 'CgTT (kWh/m²)')
    } else {
      if (red == 'abs') { # here 'red' == 'abs', thus it considered the cgtt's absolute reduction
        # add box plots
         plot = plot +
          geom_boxplot(aes(x = geometria, y = red_cgtt, fill = geometria)) +
          labs(y = 'Red. Abs. CgTT (kWh/m²)')
      } else { # here 'red' == 'abs', thus it considered the cgtt's relative reduction
        # add box plots
        plot = plot +
          geom_boxplot(aes(x = geometria, y = red_rel_cgtt, fill = geometria)) +
          labs(y = 'Red. Rel. CgTT (%)')
      }
    }
  }
  # create a facets considering the 'estado' and 'floor' variables
    # 'uni' uses facet wrap, because there is just one floor, while 'multi' uses facet grid
  if (dwel == 'uni') {
    plot = plot + facet_wrap(. ~ estado, nrow = 1)
  } else {
    plot = plot + facet_grid(floor ~ estado)
  }
  # final details
  title_name = NameTitle(dwel, lvl, inc, red) # call the function that create the plot title
  # plot theme and labels
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
  # if it's 'TRUE', it saves the plot, if it's not, it returns it as a variable
  if (save_plot) {
    plot_name = NamePlot(dwel, lvl, inc, red) # call the function that create the plot title
    SavePlot(plot, paste0(plot_name, '_bp'), lx, ly, output_dir) # call the function to save the plot
  } else {
    return(plot)
  }
}

# define characteristics to save the plot
SavePlot = function(plot, plot_name, lx, ly, output_dir) {
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
      width = lx, height = ly, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

# main functions ####
# create minimum, intermediary and superior scales for the simulation's output data frame
CreateScales = function(item, df, dwel, inc, red, save_table,
                        save_plot, lx, ly, output_dir = '') {
  # item: local where the output will be returned
  # df: data frame with cases
  # dwel: 'uni' or 'multi'
  # inc: 'FALSE', 'abs' or 'rel'
  # red: 'FALSE', 'abs' or 'rel'
  # save_plot: TRUE or FALSE (the results are returned as variables in rstudio)
  # lx: plot width
  # ly: plot hight
  # output_dir: output directory
  
  # create a vector with the names of the weathers and an extra ('PLOT')
    # this vector will name the following list
  names = levels(df[, 'estado'])
  # create a list with the length vector defined above
    # which weather item in the list will be filled with its own statistics and the 'PLOTS' item
      # will be filled with scales plots, if 'save_plot' argument is 'FALSE'
  item = vector('list', length = length(names))
  # apply the function 'DefStats' for each weather (see functions below)
    # the 'DefStats' call functions that calculate statistics and create plots
  item = lapply(names, DefStats, df, dwel, inc, red,
                save_table, save_plot, lx, ly, output_dir)
  # name the output list
  names(item) = names
  # for the 'PLOT' item, apply 'DefPlots' function (see function below)
    # the DefPlots function call the functions PlotHist and PlotBP (see functions above)
  lvls = c('intermediario', 'superior')
  item$'PLOTS' = lapply(lvls, DefPlots, df, dwel, inc, red,
                        save_plot, lx, ly, output_dir)
  names(item$'PLOTS') = lvls
  item$'TABLE' = GenScales(df, dwel, inc, red, save_table, output_dir)
  return(item)
}

DefStats = function(weather, df, dwel, inc, red, save_table,
                          save_plot, lx, ly, output_dir) {
  # weather: one of the weathers in the data frame (described in the variable 'estado')
  # df: data frame with cases
  # dwel: 'uni' or 'multi'
  # inc: 'FALSE', 'abs' or 'rel'
  # red: 'FALSE', 'abs' or 'rel'
  # save_plot: TRUE or FALSE (the results are returned as variables in rstudio)
  # lx: plot width
  # ly: plot hight
  # output_dir: output directory
  
  # create an empty list to fill with statistics and plots for intermediary and superior performance
    # levels
  item = vector('list', length = 2)
  names = c('intermediario', 'superior')
  # apply 'CalcStats' function for all the weathers
  item = lapply(names, CalcStats, df, inc, weather)
  names(item) = names
  return(item)
}

# define all the plots
DefPlots = function(lvl, df, dwel, inc, red, save_plot, lx, ly, output_dir) {
  # lvl: 'intermediario' or 'superior'
  # df: data frame with cases
  # dwel: 'uni' or 'multi'
  # inc: 'FALSE', 'abs' or 'rel'
  # red: 'FALSE', 'abs' or 'rel'
  # save_plot: TRUE or FALSE (the results are returned as variables in rstudio)
  # lx: plot width
  # ly: plot hight
  # output_dir: output directory
  
  # create an empty list to fill with histograms and box plots
  item = vector('list', length = 2)
  # name the list
  names(item) = c('hist', 'bp')
  # call 'PlotHist' function (see function above)
  item$hist = PlotHist(lvl, df, dwel, inc, red,
                       save_plot, lx, ly, output_dir)
  # call 'PlotBP' function (see function above)
  item$bp = PlotBP(lvl, df, dwel, inc, red,
                   save_plot, lx, ly, output_dir)
  return(item)
}

# main code ####
# load the files
load('/home/rodox/git/nbr_15575/outputs.RData')
# apply 'ShrinkGeom' function for 'uni' and 'multi' (see function above)
  # shrink geometries into 'P', 'M' and 'G'
dfs_list = lapply(dfs_list, ShrinkGeom)
# apply 'FixDF' function for 'uni' and 'multi' (see function above)
dfs_list = mapply(FixDF, dfs_list, names(dfs_list),
                  list(38.58, c(34.72, 33.81)), SIMPLIFY = F)
# create empty list ('scales') to return values of 'CreateScales' function
scales = vector('list', length = length(dfs_list))
# name list items as 'uni' and 'multi'
names(scales) = names(dfs_list)
# apply 'CreateScales' function for 'uni' and 'multi' (see function above)
scales = mapply(CreateScales, scales, dfs_list, names(scales), SIMPLIFY = F,
                MoreArgs = list(inc = 'abs', red = 'rel', save_table = T,
                                save_plot = T, lx = 33.8, ly = 19,
                                output_dir = '~/git/nbr_15575/plot_table/'))
