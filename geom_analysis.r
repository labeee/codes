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
PickHighARCgTT = function(df) filter(PickHighPHFT(df), red_cgtt > median(red_cgtt))
# define superior performance cases based on cgtt's relative reduction
PickHighRRCgTT = function(df) filter(PickHighPHFT(df), red_rel_cgtt > median(red_rel_cgtt))

# data mining functions ####
FixDF = function(df, unit = 'kwh') {
  unit = ifelse(unit == 'kwh', 3600000, 1000)
  df$geometria = sub('.txt', '', df$geometria)
  df$cgtt = (df$cgtr_cooling + df$cgtr_heating)/3600000
  df$cgtt_ref = (df$cgtr_cooling_ref + df$cgtr_heating_ref)/3600000
  df$red_cgtt = df$cgtt - df$cgtt_ref
  df$red_rel_cgtt = df$red_cgtt/df$cgtt_ref*100
  df$estado = factor(df$estado, levels = c('RS', 'SC', 'PR', 'RJ', 'MG', 'GO', 'TO', 'MA'))
  df$floor = ifelse(df$floor == 'CO', 'Cob.',
                    ifelse(df$floor == 'TP0', 'Tipo', 'Térreo'))
  df$floor = factor(df$floor, levels = c('Cob.', 'Tipo', 'Térreo'))
  
  return(df)
}

# statistics and plot functions ####
CreateCountDF = function(df, lvl, red = FALSE) {
  df = group_by(df, geometria, estado, floor)
  if (lvl == 'minimo') {
    df = RmLowPerf(df)
  } else if (lvl == 'intermediario') {
    df = PickHighPHFT(df)
  } else {
    if (red == FALSE) {
      df = PickLowCgTT(df)
    } else {
      if (red == 'abs') {
        df = PickHighARCgTT(df)
      } else {
        df = PickHighRRCgTT(df)
      }
    }
  }
  df = summarize(df, 'count' = length(geometria))
  
  return(df)
}

PlotCount = function(df, dwel, lvl, red = FALSE, output_dir) {
  png(filename = paste0(output_dir, dwel, '_', lvl,
                        ifelse(red == FALSE, '', paste0('_red_', red)), '_count.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    ggplot(df, aes(x = geometria, y = count, fill = geometria)) +
      geom_bar(stat = 'identity') +
      facet_grid(floor ~ estado) +
      labs(title = paste0(str_to_title(dwel), '. - Nível ',
                          ifelse(lvl == 'minimo', 'Mínimo',
                                 ifelse(lvl == 'intermediario', 'Intermediário', 'Superior')),
                          ifelse(red == FALSE, '',
                                 paste0(' (Red. ', str_to_title(red), '.)')),
                          ' - Contagem'),
           fill = 'Geometria:') +
      theme(plot.title = element_text(size = 19, face = 'bold', hjust = 0.5),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13),
            legend.position = 'bottom',
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 13),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  
  dev.off()
}

# main code ####
load('/home/rodox/00.git/01.nbr_15575/outputs.RData')
dfs_list = lapply(dfs_list, FixDF)

# application plots ####
for (dwel in c('uni', 'multi')) {
  for (lvl in c('minimo', 'intermediario', 'superior')) {
    if (lvl == 'superior') {
      for (red in c(FALSE, 'abs', 'rel'))  {
        PlotCount(CreateCountDF(dfs_list[[dwel]], lvl, red),
                  dwel, lvl, red, output_dir = '~/00.git/01.nbr_15575/00.plots/')
      }
    } else {
      PlotCount(CreateCountDF(dfs_list[[dwel]], lvl),
                dwel, lvl, output_dir = '~/00.git/01.nbr_15575/00.plots/')
    }
  }
}
