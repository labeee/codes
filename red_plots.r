library(ggplot2)
library(Hmisc)
library(stringr)

#output directory
csv_dir = '/home/rodox/Dropbox/EQUIPE-R/nbr_15575/rodolfo/results/'
plot_dir = '/home/rodox/Dropbox/EQUIPE-R/nbr_15575/rodolfo/plots/29.10/'
wraps = c('ref', 'tv', 'sf_lv', 'abs', 'ven')
aptos = c('canto_so', 'meio_so', 'meio_se', 'canto_se', 'canto_ne', 'meio_ne', 'meio_no', 'canto_no')

csv_names = dir(path = csv_dir, pattern = '.csv')

red_files = vector(mode = 'list', length = length(aptos))
names(red_files) = str_remove(csv_names, '.csv')

df = data.frame()
for (i in names(red_files)) {
  red_files[[i]] = read.csv(paste0(csv_dir, i, '.csv'))
  red_files[[i]][, 1] = NULL
  red_files[[i]]$case = ifelse(grepl('canto', i) == T,
                               paste0(capitalize(substr(i, 4, 8)), ' ', toupper(substr(i, 10, 11))),
                               paste0(capitalize(substr(i, 4, 7)), ' ', toupper(substr(i, 9, 10))))
  df = rbind(df, red_files[[i]])
}

df$wrap = factor(df$wrap, levels = c('TV','SFLV','ABS','VEN'))
df_pos_tl = df[df$abs_red_thermal_load < 0, ]
# df_pos_tl$abs_red_thermal_load = -1*df_pos_tl$abs_red_thermal_load
# df_pos_tl$rel_red_thermal_load = -1*df_pos_tl$rel_red_thermal_load
df_pos_phft = df[df$red_phft > 0, ]
# df_pos_phft$red_phft = -1*df_pos_phft$red_phft
df_pos_tmax = df[df$red_temp_max < 0, ]
# df_pos_tmax$red_temp_max = -1*df_pos_tmax$red_temp_max
df_pos = df[df$abs_red_thermal_load < 0 | df$red_phft > 0, ]

png(filename = paste0(plot_dir, 'red_cgt.png'), width = 33.8, height = 19, units = 'cm', res = 500)
ggplot(data = subset(df, room == 'Total'), aes(x = wrap, y = abs_red_thermal_load, fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df, room == 'Total'), aes(x = wrap, y = abs_red_thermal_load)) +
  labs(title = 'Multifamiliar - Redução da Carga Térmica Anual',
       x = NULL,
       y = 'Red. CgT Anual (kWh)',
       fill = 'Envoltória') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()

png(filename = paste0(plot_dir, 'red_cgt_pos.png'), width = 33.8, height = 19, units = 'cm',
    res = 500)
ggplot(data = subset(df_pos_tl, room == 'Total'), aes(x = wrap, y = abs_red_thermal_load,
                                                      fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df_pos_tl, room == 'Total'), aes(x = wrap, y = abs_red_thermal_load,
                                                      colour = case)) +
  labs(title = 'Multifamiliar - Redução da Carga Térmica Anual',
       x = NULL,
       y = 'Red. CgT Anual (kWh)',
       fill = 'Envoltória',
       colour = 'Caso') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()

png(filename = paste0(plot_dir, 'red_rel_cgt.png'), width = 33.8, height = 19, units = 'cm',
    res = 500)
ggplot(data = subset(df, room == 'Total'), aes(x = wrap, y = rel_red_thermal_load, fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df, room == 'Total'), aes(x = wrap, y = rel_red_thermal_load)) +
  labs(title = 'Multifamiliar - Redução Relativa da Carga Térmica Anual',
       x = NULL,
       y = 'Red. CgT Anual (%)',
       fill = 'Envoltória') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()

png(filename = paste0(plot_dir, 'red_rel_cgt_pos.png'), width = 33.8, height = 19, units = 'cm',
    res = 500)
ggplot(data = subset(df_pos_tl, room == 'Total'), aes(x = wrap, y = rel_red_thermal_load, fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df_pos_tl, room == 'Total'), aes(x = wrap, y = rel_red_thermal_load,
                                                      colour = case)) +
  labs(title = 'Multifamiliar - Redução Relativa da Carga Térmica Anual',
       x = NULL,
       y = 'Red. CgT Anual (%)',
       fill = 'Envoltória',
       colour = 'Caso') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()

png(filename = paste0(plot_dir, 'red_phft.png'), width = 33.8, height = 19, units = 'cm', res = 500)
ggplot(data = subset(df, room == 'Total'), aes(x = wrap, y = red_phft, fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df, room == 'Total'), aes(x = wrap, y = red_phft)) +
  labs(title = 'Multifamiliar - Redução da Percentual de Horas Ocupadas na Faixa de Temperatura
       Operativa',
       x = NULL,
       y = 'Red. PHFT (%)',
       fill = 'Envoltória') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()

png(filename = paste0(plot_dir, 'red_phft_pos.png'), width = 33.8, height = 19, units = 'cm',
    res = 500)
ggplot(data = subset(df_pos_phft, room == 'Total'), aes(x = wrap, y = red_phft, fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df_pos_phft, room == 'Total'), aes(x = wrap, y = red_phft,
                                                      colour = case)) +
  labs(title = 'Multifamiliar - Redução da Percentual de Horas Ocupadas na Faixa de Temperatura
       Operativa',
       x = NULL,
       y = 'Red. PHFT (%)',
       fill = 'Envoltória',
       case = 'Caso') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()

png(filename = paste0(plot_dir, 'red_temp_max.png'), width = 33.8, height = 19, units = 'cm',
    res = 500)
ggplot(data = subset(df, room == 'Total'), aes(x = wrap, y = red_temp_max, fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df, room == 'Total'), aes(x = wrap, y = red_temp_max)) +
  labs(title = 'Multifamiliar - Redução da Temperatura Máxima',
       x = NULL,
       y = 'Red. Temp. Max. (°C)',
       fill = 'Envoltória') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()

png(filename = paste0(plot_dir, 'red_temp_max_pos.png'), width = 33.8, height = 19, units = 'cm',
    res = 500)
ggplot(data = subset(df_pos_tmax, room == 'Total'), aes(x = wrap, y = red_temp_max, fill = wrap)) +
  facet_grid(floor ~ weather) +
  geom_boxplot() +
  geom_jitter(data = subset(df_pos_tmax, room == 'Total'), aes(x = wrap, y = red_temp_max,
                                                      colour = case)) +
  labs(title = 'Multifamiliar - Redução da Temperatura Máxima',
       x = NULL,
       y = 'Red. Temp. Max. (°C)',
       fill = '',
       colour = 'Caso') +
  theme(legend.text = element_text(size=17),
        plot.title = element_text(size = 20),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
dev.off()


# red phft x red cgt
ggplot(data = subset(df, room == 'Total'), aes(x = abs_red_thermal_load, y = red_phft)) +
  facet_grid(floor ~ weather) +
  geom_point(data = subset(df, room == 'Total'), aes(x = abs_red_thermal_load, y = red_phft, colour = wrap)) +
  geom_smooth(method = 'lm', data = subset(df, room == 'Total'), se = F, colour = 'Black') +
  labs(title = 'Multifamiliar - Redução PHFT x Redução Carga Térmica',
       x = 'Red. CgT (%)',
       y = 'Red. PHFT (%)',
       fill = '',
       colour = 'Caso') +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))

ggplot(data = df_pos, aes(x = abs_red_thermal_load, y = red_phft)) +
  facet_grid(floor ~ weather) +
  geom_point(data = df_pos, aes(x = abs_red_thermal_load, y = red_phft, colour = wrap)) +
  geom_smooth(method = 'lm', data = df_pos, se = F, colour = 'Black') +
  labs(title = 'Multifamiliar - Redução PHFT x Redução Carga Térmica',
       x = 'Red. CgT (%)',
       y = 'Red. PHFT (%)',
       fill = '',
       colour = 'Caso') +
  theme(legend.text = element_text(size=15),
        legend.title = element_text(size=16),
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13),
        strip.text.x = element_text(size = 19),
        strip.text.y = element_text(size = 19))
