# base functions ####
# calculate approximated view factor value
  # only valid for zones with four walls and 90° tilt
CalcApproximatedViewFactorValue = function(surf1, surf2, areas) {
  if (surf1 == surf2) {
    view_factor = 0
  } else {
  view_factor = areas[surf2]/sum(areas[-surf1])
  }
  return(view_factor)
}
# calculate approximated view factor vector
CalcApproximatedViewFactorVector = function(surf2, surfs1, areas) {
  view_factor = sapply(surfs1, CalcApproximatedViewFactorValue, surf2, areas)
  return(view_factor)
}
# calculate approximated view factor matrix
CalcApproximatedViewFactorMatrix = function(areas, surfs1, surfs2) {
  view_factor = sapply(surfs2, CalcApproximatedViewFactorVector, surfs1, areas)
  return(view_factor)
}

# main function ####
# fix view factors
FixViewFactors = function(areas) {
  # calculate approximated view factor matrix
  n = length(areas)
  f = CalcApproximatedViewFactorMatrix(areas, 1:n, 1:n)
  # define convergence values
  primary_conv = 0.001
  diff_conv = 0.00001
  original_check_value = abs(sum(f) - n)
  accelerator = 1
  old_conv = 10
  largest_area = max(areas)
  # check for huge surfaces
  largest_surf = 0
  fixed_af = f
  if (largest_area > (sum(areas) - largest_area)) {
    for (i in 1:n) {
      if (largest_area == areas[i]) {
        next
      } else {
        largest_surf = i
        break
      }
    }
    # add a self view to huge surfaces
    fixed_af[largest_surf, largest_surf] = min(c(0.9, 1.2*largest_area/sum(areas)))
  }
  # define af matrix
    # this matrix is the f matrix multiplied by viewer (1) surface's area
  af = matrix(nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in seq_along(areas)) {
      af[j, i] = fixed_af[j, i]*areas[i]
    }
  }
  # enforce reciprocity by assuming the mean between f[i, j] and f[j, i]
    # try to fix cases when f[i, j] is different than f[j, i]
  fixed_af = (af+t(af))/2
  # generated fixed view factor matrix
  fixed_f = matrix(nrow = n, ncol = n)
  row_coefficient = rep(NA, n)
  converged = FALSE
  while (!converged) {
    for (i in 1:n) {
      sum_fixed_af = sum(fixed_af[, i])
      if (abs(sum_fixed_af > 0)) {
        row_coefficient[i] = areas[i]/sum_fixed_af
      } else {
        row_coefficient = 1
      }
      fixed_af[, i] = fixed_af[, i]*row_coefficient[i]
    }
    # enforce reciprocity
    fixed_af = 0.5*(fixed_af+t(fixed_af))
    # fill fixed view factor matrix
    for (i in 1:n) {
      for (j in 1:n) {
        fixed_f[j, i] = fixed_af[j, i]/areas[i]
        if (abs(fixed_f[j, i]) <= 0) {
          fixed_f[j, i] = 0
          fixed_af[j, i] = 0
        }
      }
    }
    new_conv = abs(sum(fixed_f) - n)
    if (abs(old_conv - new_conv) < diff_conv | new_conv <= primary_conv) {
      converged = TRUE
    }
    old_conv = new_conv
    # skip non-convergence assumptions
  }
  return(fixed_f)
}

# application ####
# areas c('floor', 'roof', 'n', 's', 'e', 'w')
areas = c(2*4, 2*4, 3*3, 3*3, 3*4, 3*4)
vf = FixViewFactors(areas)

# test

# vidro simples (fs = 0.87)
# concreto (abs = 0.7, ref = 0.3)
# lx = 2, ly = 4, lz = 3

df = read.csv('~/Documents/view_factor/test.csv')[-1]/3600000
rad = c('window' = df$AMBIENTE.Zone.Windows.Total.Transmitted.Solar.Radiation.Energy..J..Annual.,
        'piso' = df$PISO.Surface.Inside.Face.Solar.Radiation.Heat.Gain.Energy..J..Annual.,
        'cob' = df$COBERTURA.Surface.Inside.Face.Solar.Radiation.Heat.Gain.Energy..J..Annual.,
        'n' = df$PAREDENORTE.Surface.Inside.Face.Solar.Radiation.Heat.Gain.Energy..J..Annual.,
        's' = df$PAREDESUL.Surface.Inside.Face.Solar.Radiation.Heat.Gain.Energy..J..Annual.,
        'l' = df$PAREDELESTE.Surface.Inside.Face.Solar.Radiation.Heat.Gain.Energy..J..Annual.,
        'o' = df$PAREDEOESTE.Surface.Inside.Face.Solar.Radiation.Heat.Gain.Energy..J..Annual.)

