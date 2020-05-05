from scalingGeometry import scaling

# i=0

# files_list = open('files_list.csv','w')
# files_list = open('files_list_multi.csv','w')
# files_list.write('case,z,x,y,area_fac,file\n')

def gen():
    for z in z_scales:
        for xy in xy_scales:
            for a in area_fac:

                a_sqrt = a**(1/2)
                sqrt_scalex = xy[0]**(1/2)
                sqrt_scaley = xy[1]**(1/2)

                scalex = (xy[0]*a_sqrt)/(sqrt_scalex*sqrt_scaley)
                scaley = (xy[1]*a_sqrt)/(sqrt_scalex*sqrt_scaley)

                for file in files:

                    if 'shading' in file:

                        scaling(scales={'scalex':scalex, 'scaley':scaley,'scalez':z}, ref=False, refscale=.17, window_scale=False,
                        shading_scale=False, input_file=file,output_name=file.split('.')[0]+'_'+str(xy_scales.index(xy))+'_'+str(z_scales.index(z))+str(area_fac.index(a)))  # +'_'+'{:03.0f}'.format(i))

                    else:
                        scaling(scales={'scalex':scalex, 'scaley':scaley,'scalez':z}, ref=True, refscale=.17, window_scale=False,
                        shading_scale=False, input_file=file,output_name=file.split('.')[0]+'_'+str(xy_scales.index(xy))+'_'+str(z_scales.index(z))+str(area_fac.index(a)))  # +'_'+'{:03.0f}'.format(i))

                # files_list.write('{:03.0f}'.format(i)+','+str(z)+','+str(xy[0])+','+str(xy[1])+','+str(a)+','+file+'\n')

            # i += 1

# files_list.close()

#########################################

## UNI

# files = ['ac_Ref_ZB1a6.idf','ac_Ref_ZB7e8.idf','vn_Ref_ZB1a6.idf','vn_Ref_ZB7e8.idf']
# z_scales = [1,2]
# xy_scales = [(1,1), (1.2,1), (1.6,1),(2.5,1),(1,1.25),(1,2)]
# area_fac = [1,1.1,1.2,1.35,1.5]

files = ['ac_Ref_ZB1a6.idf','M001_Caso02_vn_shading_150.idf','M001_Caso02_vn_shading_50.idf']
z_scales = [1,2]
xy_scales = [(1,1), (2.5,1), (1,2)]
area_fac = [1,1.5,2]

gen()

## MULTI

# files = ['M001_Caso1_vn.idf','M001_Caso1_ac.idf']
# z_scales = [1]
# xy_scales = [(1,1), (1.2,1), (1.5,1),(2,1),(1,1.5),(1,2)]
# area_fac = [1,1.25,1.5]

files = ['M002_Caso1_vn.idf','M001_C_Caso13_vn_shading_120.idf','M001_I_Caso13_vn_shading_120.idf','M001_T_Caso13_vn_shading_120.idf']
z_scales = [1]
xy_scales = [(1,1), (2,1),(1,2)]
area_fac = [1,1.5]

gen()
