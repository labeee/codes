# load libraries ####
invisible({
  pkgs = c('dplyr', 'ggplot2', 'reshape2', 'stringr')
  lapply(pkgs, library, character.only = TRUE)
})

# base functions ####
# classify habitation according to simplified method
ClassSimp = function(comp, zbb, abs, area, paf, fv, dwel, props) {
  zbb = zbb %>% str_sub(-1, -1) %>% as.numeric()
  comp = ifelse(comp == 'REF', ifelse(zbb == 8, 'REF8', 'REF17'),
                paste0(comp, '_', str_to_upper(dwel)))
  chars = data.frame('u_par' = props[[comp]][['paredes_externas']][['transmitancia']],
                     'ct_par' = props[[comp]][['paredes_externas']][['capacidade_termica']],
                     'u_cob' = props[[comp]][['cobertura']][['transmitancia']],
                     'ct_cob' = props[[comp]][['cobertura']][['capacidade_termica']])
  # simplified classification
  class = data.frame(class_u_par = NA, class_ct_par = NA, class_fv = NA, class_paf = NA)
  # wall transmitance
  if (zbb %in% c(1, 2)) {
    class$class_u_par = ifelse(chars$u_par <= 2.5, TRUE, FALSE)
  } else {
    if (abs <= 0.6) {
      class$class_u_par = ifelse(chars$u_par <= 3.7, TRUE, FALSE)
    } else {
      class$class_u_par = ifelse(chars$u_par <= 2.5, TRUE, FALSE)
    }
  }
  # wall thermal capacity
  if (zbb == 8) {
    class$class_ct_par = TRUE
  } else {
    class$class_ct_par = ifelse(chars$ct_par >= 130000, TRUE, FALSE)
  }
  # openings
  area = area/3 # considering a 3 rooms dwelling!
  if (area <= 20) {
    class$class_paf = ifelse(paf <= 20, TRUE, FALSE)
  } else {
    class$class_paf = ifelse(area*as.numeric(paf)/100 <= 4, TRUE, FALSE)
  }
  # open factor
  class$class_fv = ifelse(as.numeric(paf)*as.numeric(fv) >= 7*2, TRUE, FALSE)
  class$simp = ifelse(any(class == FALSE), FALSE, TRUE)
  return(cbind(chars, class))
}
# define cases higher than intermediary performance
FiltPHFT = function(df, dwel, inc) {
  df = RmLowPerf(df, dwel)
  if (inc == FALSE) {
    df = filter(df, phft > median(phft))
  } else if (inc == 'abs') {
    df = filter(df, inc_phft > median(inc_phft))
  } else {
    df = filter(df, inc_rel_phft > median(inc_rel_phft))
  }
  return(df)
}
# define superior performance cases based on cgtt
FiltCgTT = function(df, dwel, inc, red) {
  df = FiltPHFT(df, dwel, inc)
  if (red == FALSE) {
    df = filter(df, cgtt < median(cgtt))
  } else if (red == 'abs') {
    df = filter(df, red_cgtt > median(red_cgtt))
  } else {
    df = filter(df, red_rel_cgtt > median(red_rel_cgtt))
  }
  return(df)
}
# select cases higher than the minimum performance level
RmLowPerf = function(df, dwel) {
  lim_sup = ifelse(dwel == 'uni', 2, 1)
  df$t_max_min = df$t_max_ref + lim_sup
  df$t_max_min = ifelse(df$floor == 'Cob.', df$t_max_min + 1, df$t_max_min)
  df = df %>%
    filter(phft > phft_ref & t_max < t_max_min &
             !((estado == 'PR' | estado == 'RS' | estado == 'SC' | estado == 'MG') &
                 (t_min < t_min_ref - 1)))
  return(df)
}
# select cases lower than the minimum performance level
RmHighPerf = function(df, dwel) {
  lim_sup = ifelse(dwel == 'uni', 2, 1)
  df$t_max_min = df$t_max_ref + lim_sup
  df$t_max_min = ifelse(df$floor == 'Cob.', df$t_max_min + 1, df$t_max_min)
  df = df %>%
    filter(phft < phft_ref | t_max > t_max_min |
             ((estado == 'PR' | estado == 'RS' | estado == 'SC' | estado == 'MG') &
                (t_min < t_min_ref - 1)))
  return(df)
}
# remove cases whose cgtt is higher than cgtt reference
RmHighCgTT = function(df, dwel) filter(RmLowPerf(df, dwel), cgtt < cgtt_ref)

# data mining functions ####
# fix data frame
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
  df$floor = factor(df$floor)
  return(df)
}
# relabel data frame
RelabelDF = function(df) {
  df$veneziana = ifelse(grepl('on', df$veneziana), 'Com', 'Sem')
  df$componente = ifelse(grepl('ref', df$componente), 'REF',
                         ifelse(grepl('sfar', df$componente), 'SFAR',
                                ifelse(grepl('sfiso', df$componente), 'SFISO',
                                       ifelse(grepl('tv', df$componente), 'TV',
                                              ifelse(grepl('tijmacico20', df$componente), 'TM20',
                                                     'TM10')))))
  df$absortancia = ifelse(grepl('20', df$absortancia), '0.2',
                          ifelse(grepl('60', df$absortancia), '0.6', '0.8'))
  df$vidro = paste0('Vid. ', ifelse(grepl('double', df$vidro), 'Duplo', 'Simples'),
                    ' (FS .', str_extract(df$vidro, '(?<=fs).*(?=\\.txt)'), ')')
  df$open_fac = ifelse(grepl('045', df$open_fac), '0.45', '1')
  df$sombreamento = str_extract(df$sombreamento, '(?<=shade_).*(?=_geom)')
  df$paf = str_extract(df$paf, '(?<=wwr_).*(?=_geom)')
  return(df)
}
# apply ClassSimp() function
ApplyClassSimp = function(df, dwel, props_path) {
  df = dfs_list[[1]]
  dwel = names(dfs_list)[1]
  props_path = '~/git/nbr/properties.json'
  
  props = read_json(props_path)
  names(props) = c('REF17', 'REF8', 'C10_UNI', 'C10_MULTI', 'SFAR_UNI', 'SFAR_MULTI',
                   'SFISO_UNI', 'SFISO_MULTI', 'TM10_UNI', 'TM10_MULTI', 'TM20_UNI',
                   'TM20_MULTI', 'TV_UNI', 'TV_MULTI')
  df = cbind(df, bind_rows(mapply(ClassSimp, df$componente, df$zbb, df$absortancia,
                                  df$area, df$paf, df$open_fac, SIMPLIFY = FALSE,
                                  MoreArgs = list(dwel, props))))
  return(df)
}

# statistics and plot functions ####
CreateCountDF = function(var, df, dwel, lvl, inc, red) {
  colnames(df)[colnames(df) == var] = 'var'
  df0 = df = group_by(df, estado, floor)
  if (lvl == 'minimo') {
    df = RmHighPerf(df, dwel)
  } else if (lvl == 'intermediario') {
    df = FiltPHFT(df, inc)
  } else {
    df = FiltCgTT(df, dwel, inc, red)
  }
  df0 = group_by(df0, var, estado, floor, .drop = FALSE)
  df0 = summarize(df0, 'count' = length(var))
  df = group_by(df, var, estado, floor, .drop = FALSE)
  df = summarize(df, 'count' = length(var))
  df$count = df$count*100/df0$count
  return(df)
}

# plot functions ####
PlotCount = function(var, df, dwel, lvl, inc = 'abs', red = 'rel', output_dir) {
  plot_name = paste0(output_dir, dwel, '_', lvl, '_', var, '_count.png')
  plot_title = paste0(str_to_title(dwel), '. - Nível ', str_to_title(lvl))
  png(filename = plot_name, width = 33.8, height = 19, units = 'cm', res = 500)
  df = CreateCountDF(var, df, dwel, lvl, inc, red)
  plot(
    ggplot(df, aes(x = var, y = count, fill = var)) +
      geom_bar(stat = 'identity', colour = 'black') +
      facet_grid(floor ~ estado) +
      labs(title = plot_title,
           subtitle = paste0('Em relação à variável "', str_to_title(var), '"'),
           fill = paste0(str_to_title(var), ':')) +
      ylab('Percentual de casos que não passam no mínimo (%)\n') +
      theme(plot.title = element_text(size = 19, face = 'bold', hjust = 0.5),
            plot.subtitle = element_text(size = 17, face = 'bold', hjust = 0.5),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13),
            legend.position = 'bottom',
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 13),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  dev.off()
}
# scatter plot differences on operative temperature
PlotDiffTop = function(df, dwel, lim, output_dir) {
  top = paste0('t_', lim)
  top_diff = paste0(top, '_diff')
  top_ref = paste0(top, '_ref')
  df[top_diff] = df[top] - df[top_ref]
  df = melt(df, id.vars = c(top, 'floor', 'estado', 'componente'), measure.vars = top_diff)
  colnames(df)[1] = 'top'
  df$lim0 = 0
  if (top == 't_min') {
    df$lim1 = -1
    df$lim2 = -2
    df = df[df$estado %in% c('RS', 'SC', 'PR', 'MG'), ]
  } else {
    df$lim1 = 1
    df$lim2 = 2
  }
  plot_name = paste0(output_dir, dwel, '_', top, '_scatter.png')
  png(filename = plot_name, width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    ggplot(df) +
      geom_point(aes(x = top, y = value, colour = componente), size = 0.4, shape = 4) +
      geom_line(aes(x = top, y = lim0), size = 0.2) +
      geom_line(aes(x = top, y = lim1), size = 0.2) +
      geom_line(aes(x = top, y = lim2), size = 0.2) +
      facet_grid(floor ~ estado) +
      labs(title = str_to_title(dwel),
           fill = 'Componentes:') +
      xlab(paste0('Top,', lim, ' Real. (°C)\n')) +
      ylab(paste0('\nDiff. Top,', lim, ' (°C) [Real - Ref.]')) +
      theme(plot.title = element_text(size = 19, face = 'bold', hjust = 0.5),
            plot.subtitle = element_text(size = 17, face = 'bold', hjust = 0.5),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13),
            legend.position = 'bottom',
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 10, angle = 90),
            axis.text.y = element_text(size = 10),
            strip.text.x = element_text(size = 13),
            strip.text.y = element_text(size = 13))
  )
  dev.off()
}

# main code ####
load('~/git/nbr/outputs.rdata')
dfs_list = lapply(dfs_list, FixDF)
dfs_list = lapply(dfs_list, RelabelDF)

# application ####
vars = c('veneziana', 'componente', 'absortancia', 'vidro', 'open_fac', 'sombreamento', 'paf')
lapply(vars, PlotCount, dfs_list$uni, 'uni', 'minimo', 'abs', 'rel', '~/rolante/nbr/new/')
lapply(vars, PlotCount, dfs_list$multi, 'multi', 'minimo', 'abs', 'rel', '~/rolante/nbr/new/')
mapply(PlotDiffTop, dfs_list, names(dfs_list), rep(c('min', 'max'), each = 2), '~/rolante/nbr/new/')
