# load libraries ####
pkgs = c('dplyr', 'ggplot2', 'stringr')
lapply(pkgs, library, character.only = T)

# auxiliar functions ####
# remove cases lower than minimum performance
RmLowPerf = function(df) df %>%
  filter(
    phft > phft_ref &
      (t_max < t_max_ref + 1) &
      !((estado == 'PR' | estado == 'RS' | estado == 'SC') & (t_min < t_min_ref - 1))
  )
# remove cases whose cgtt is higher than cgtt reference
RmHighCgTT = function(df) filter(RmLowPerf(df), cgtt < cgtt_ref)
# define cases higher than intermediary performance
FiltPHFT = function(df, inc) {
  df = RmLowPerf(df)
  if (inc == F) {
    df = filter(df, phft > median(phft))
  } else if(inc == 'abs') {
    df = filter(df, inc_phft > median(inc_phft))
  } else {
    df = filter(df, inc_rel_phft > median(inc_rel_phft))
  }
  return(df)
}
# define superior performance cases based on cgtt
FiltCgTT = function(df, inc, red) {
  df = FiltPHFT(df, inc)
  if (red == F) {
    df = filter(df, cgtt < median(cgtt))
  } else if (red == 'abs') {
    df = filter(df, red_cgtt > median(red_cgtt))
  } else {
    df = filter(df, red_rel_cgtt > median(red_rel_cgtt))
  }
  return(df)
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
FixDF = function(df, unit = 'kwh') {
  div = ifelse(unit == 'kwh', 3600000, 1000)
  df$geometria = sub('.txt', '', df$geometria)
  df$cgtt = ifelse(grepl('MA|TO', df$estado), df$cgtr_cooling,
                   df$cgtr_cooling + df$cgtr_heating)
  df$cgtt = df$cgtt/(df$area*div)
  df$cgtt_ref = ifelse(grepl('MA|TO', df$estado), df$cgtr_cooling_ref,
                       df$cgtr_cooling_ref + df$cgtr_heating_ref)
  df$cgtt_ref = df$cgtt_ref/(df$area*div)
  df$red_cgtt = df$cgtt_ref - df$cgtt
  df$red_rel_cgtt = df$red_cgtt/df$cgtt_ref*100
  df$phft = df$phft*100
  df$phft_ref = df$phft_ref*100
  df$inc_phft = df$phft - df$phft_ref
  df$inc_rel_phft = df$inc_phft/df$phft_ref*100
  df$estado = factor(df$estado, levels = c('RS', 'SC', 'PR', 'RJ', 'MG', 'GO', 'TO', 'MA'))
  df$floor = ifelse(df$floor == 'CO', 'Cob.',
                    ifelse(df$floor == 'TP0', 'Tipo', 'Térreo'))
  df$floor = factor(df$floor, levels = c('Cob.', 'Tipo', 'Térreo'))
  
  return(df)
}

# statistics and plot functions ####
CreateCountDF = function(df, lvl, inc = F, red = F) {
  df = group_by(df, geometria, estado, floor)
  if (lvl == 'minimo') {
    df = RmLowPerf(df)
  } else if (lvl == 'intermediario') {
    df = FiltPHFT(df, inc)
  } else {
    df = FiltCgTT(df, inc, red)
  }
  df = summarize(df, 'count' = length(geometria))
  
  return(df)
}

# plot functions ####
PlotCount = function(df, dwel, lvl, inc = F, red = F, output_dir) {
  png(filename = paste0(output_dir, dwel, '_', lvl,
                        ifelse(inc == F, '', paste0('_inc_', inc)),
                        ifelse(red == F, '', paste0('_red_', red)), '_count.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    ggplot(df, aes(x = geometria, y = count, fill = geometria)) +
      geom_bar(stat = 'identity') +
      facet_grid(floor ~ estado) +
      labs(title = paste0(NameTitle(dwel, lvl, inc, red),
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
load('/home/rodox/git/nbr_15575/outputs.rdata')
dfs_list = lapply(dfs_list, FixDF)

# application plots ####
inc = 'abs'
red = 'rel'
for (dwel in c('uni', 'multi')) {
  for (lvl in c('minimo', 'intermediario', 'superior')) {
    if (lvl == 'minimo') {
      PlotCount(CreateCountDF(dfs_list[[dwel]], lvl),
                dwel, lvl, output_dir = '~/0.git/nbr_15575/plots/')
    } else if (lvl == 'intermediario') {
      PlotCount(CreateCountDF(dfs_list[[dwel]], lvl, inc),
                dwel, lvl, inc, output_dir = '~/0.git/nbr_15575/plots/')
    } else {
      PlotCount(CreateCountDF(dfs_list[[dwel]], lvl, inc, red),
                dwel, lvl, inc, red, output_dir = '~/0.git/nbr_15575/plots/')
    }
  }
}
