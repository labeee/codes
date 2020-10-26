# vidro simples (fs = 0.87)
# concreto (abs = 0.7, ref = 0.3)
# lx = 2, ly = 4, lz = 3

test = read.csv('~/Documents/test/test.csv')[-1]
rad = c('n' = test$PAREDENORTE.Surface.Inside.Face.Absorbed.Shortwave.Radiation.Rate..W..Annual.,
        's' = test$PAREDESUL.Surface.Inside.Face.Absorbed.Shortwave.Radiation.Rate..W..Annual.,
        'l' = test$PAREDELESTE.Surface.Inside.Face.Absorbed.Shortwave.Radiation.Rate..W..Annual.,
        'cob' = test$COBERTURA.Surface.Inside.Face.Absorbed.Shortwave.Radiation.Rate..W..Annual.)

area_cob = 2*4
area_l = 4*3
area_n = 2*3
area_o = 4*3
area_s = 2*3
area_piso = 2*4

# 1st try (area2 / seen area1)
vf1_piso_cob = area_cob/(area_n+area_s+area_l+area_o+area_cob)
vf1_piso_n = area_n/(area_n+area_s+area_l+area_o+area_cob)
vf1_piso_l = area_l/(area_n+area_s+area_l+area_o+area_cob)
# test a -> ok
vf1_piso_cob/vf1_piso_l
rad['cob']/rad['l']
# test b -> bad
vf1_piso_n/vf1_piso_l
rad['n']/rad['l']
# test b -> bad
vf1_piso_n/vf1_piso_cob
rad['n']/rad['cob']

# 2nd try (area1 / seen area2)
vf2_piso_cob = area_piso/(area_n+area_s+area_l+area_o+area_piso)
vf2_piso_n = area_piso/(area_s+area_l+area_o+area_cob+area_piso)
vf2_piso_l = area_piso/(area_n+area_s+area_o+area_cob+area_piso)
# test a -> bad
vf2_piso_n/vf2_piso_l
rad['n']/rad['l']
# test b -> bad
vf2_piso_n/vf2_piso_cob
rad['n']/rad['cob']

# i think this is the right one!
# 3rd try (area2 / seen area2)
vf3_piso_cob = area_cob/(area_n+area_s+area_l+area_o+area_piso)
vf3_piso_n = area_n/(area_s+area_l+area_o+area_cob+area_piso)
vf3_piso_l = area_l/(area_n+area_s+area_o+area_cob+area_piso)
# test a -> bad
vf3_piso_n/vf3_piso_l
rad['n']/rad['l']
# test b -> bad
vf3_piso_n/vf3_piso_cob
rad['n']/rad['cob']

# 4th try (area2 / seen total)
vf4_piso_cob = area_cob/(area_n+area_s+area_l+area_o+area_cob+area_piso)
vf4_piso_n = area_n/(area_n+area_s+area_l+area_o+area_cob+area_piso)
vf4_piso_l = area_l/(area_n+area_s+area_l+area_o+area_cob+area_piso)
# test a -> bad
vf4_piso_n/vf4_piso_l
rad['n']/rad['l']
# test b -> bad
vf4_piso_n/vf4_piso_cob
rad['n']/rad['cob']

