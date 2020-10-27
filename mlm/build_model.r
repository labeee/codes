library(dplyr)
library(jsonlite)
library(purrr)
library(stringr)

# base functions ####
# airflow network zones
AddAFNZone = function(tag, fill) AddFields(fill$item, 'zone_name', tag)
# airflow network surfaces
AddAFNSurf = function(object, tag, storey, index, fill, cond) {
  fields = c('surface_name', 'leakage_component_name',
             'idf_max_fields', 'ventilation_control_mode')
  # surface_name = surface name with storey prefix
  values = RnmStorey(tag, storey, index)
  if (cond == 'afn') { # if it's a natural ventilation model
    if (object$surface_type == 'Window') { # if fenestration is a window
      if (grepl('bwc', tag)) { # if it's a bwc window
        # leakage_component_name = bwc_opening
        # vetilation_control_mode = Constant
        values = append(values, list('bwc_opening', 7, 'Constant'))
      } else { # if it's a living room or dormitory
        fields = append(fields, c('venting_availability_schedule_name',
                                  'ventilation_control_zone_temperature_setpoint_schedule_name'))
        # leakage_component_name = window_opening
        sch = ifelse(grepl('liv', tag), 'sch_afn_liv', 'sch_afn_dorm')
        # venting_availability_schedule_name = Temperature
        # ventilation_control_zone_temperature_setpoint_schedule_name = sch_window_behav
        values = append(values, list('window_opening', 9, 'Temperature', sch, 'sch_window_behav'))
      }
    } else if (object$surface_type == 'Door') { # if fenestration is a door
      # leakage_component_name = door_opening
      values = append(values, list('door_opening', 7))
      if (grepl('bwc|core', tag)) { # if it's a bwc door
        # vetilation_control_mode = NoVent
        values = append(values, 'NoVent')
      } else if (grepl('dorm', tag) &  # if it's a door between living room and dormitory
                 !grepl('bwc|core', object$outside_boundary_condition_object)) {
        # vetilation_control_mode = Constant
        values = append(values, 'Constant')
      } else { # if the door is repeated
        # the airflow network surface is not considered twice, thus the value is removed (NULL)
        values = NULL
      }
    } else {  # if fenestration is not a door or a window
      stop('Not recognized fenestration type!')
    }
  } else if (cond == 'hvac') { # if it's an air conditioned model
    # avoid doubled afn doors
    lkg = paste0(ifelse(object$surface_type == 'Door', 'door',
                        ifelse(grepl('bwc', tag), 'bwc', 'window')), '_opening')
    values = append(values, list(lkg, 7, 'NoVent'))
    if (object$surface_type == 'Door') {
      if (!(grepl('dorm|bwc|core', tag) &
            !grepl('bwc|core', object$outside_boundary_condition_object))) {
        values = NULL
      }
    }
  } else {
    stop('Not recognized conditioning type!')
  }
  # add fields
  if (!is.null(values)) { # if it's not a repeated door
    # fill the fields with the values
    surf = AddFields(fill$item, fields, values)
    return(surf)
  }
}
# add blind control
AddBlindControl = function(tag, fill) {
  zone = str_remove(tag, '_window.*')
  blind = AddFields(fill$item, 'zone_name', zone)
  blind$fenestration_surfaces = list(list('fenestration_surface_name' = tag))
  return(blind)
}
# add fields and its values to an object
AddFields = function(object, fields, values) {
  tags = names(object)
  object = append(object, mapply(function(x, y) x = y, fields, values, SIMPLIFY = FALSE))
  names(object) = c(tags, fields)
  return(object)
}
# add ideal loads (hvac system)
AddHVAC = function(tag, type, fill) {
  if (type == 'hvac') {
    fields = c('availability_schedule_name', 'zone_supply_air_node_name')
    room = str_extract(tag, 'liv|dorm')
    values = c(paste0('sch_hvac_', room), paste0('hvac_supply_inlet_', tag))
    object = AddFields(fill$item, fields, values)
  } else if (type == 'equip') {
    object = fill$item
    object$equipment[[1]] = AddFields(object$equipment[[1]],
                                      'zone_equipment_name', paste0('hvac_', tag))
  } else if (type == 'equip_conn') {
    fields = c('zone_air_inlet_node_or_nodelist_name', 'zone_air_node_name',
               'zone_conditioning_equipment_list_name', 'zone_name',
               'zone_return_air_node_or_nodelist_name')
    values = paste0(c('hvac_supply_inlet_', 'air_node_', 'equip_', '', 'return_outlet_'), tag)
    object = AddFields(fill$item, fields, values)
  } else {
    stop('Not recognized equipment type!')
  }
  return(object)
}
# add internal loads
AddIntLoad = function(tag, fill, type) {
  fields = 'zone_or_zonelist_name'
  values = tag
  liv = grepl('liv', tag)
  if (type == 'light') {
    fields = append(fields, 'schedule_name')
    sch = ifelse(liv, 'sch_ilum_liv', 'sch_ilum_dorm')
    values = append(values, sch)
  } else if (type == 'people') {
    fields = append(fields, c('activity_level_schedule_name',
                              'number_of_people_schedule_name', 'number_of_people'))
    sch_act = ifelse(liv, 'sch_activ_liv', 'sch_activ_dorm')
    sch_np = sub('activ', 'occup', sch_act)
    np = ifelse(liv, 4, 2)
    values = list(values, sch_act, sch_np, np)
  }
  int_load = AddFields(fill$item, fields, values)
  return(int_load)
}
# add output variables
AddOutputs = function(outputs, fill) {
  field = 'variable_name'
  options = list('mean_temp' = 'Zone Mean Air Temperature',
                 'op_temp' = 'Zone Operative Temperature',
                 'ideal_loads' = c('Zone Ideal Loads Zone Total Cooling Energy',
                                   'Zone Ideal Loads Zone Total Heating Energy'),
                 'air_change' = 'AFN Zone Infiltration Air Change Rate',
                 'therm_bal' = c('Surface Inside Face Convection Heat Gain Energy',
                                 'AFN Zone Infiltration Sensible Heat Gain Energy',
                                 'AFN Zone Infiltration Sensible Heat Loss Energy',
                                 'Zone Total Internal Convective Heating Energy'),
                 'surf_temp' = 'Surface Inside Face Temperature')
  values = options[outputs] %>% flatten_chr()
  outputs = mapply(AddFields, list(fill$item), field, values, SIMPLIFY = FALSE)
  names(outputs) = paste0('output', 1:length(outputs))
  return(outputs)
}
# add thermostats
AddThermostats = function(tags, fill) {
  objects = lapply(tags, function(x, y) append(y, c(zone_or_zonelist_name = x)), fill$item)
  names(objects) = paste0('thermostat_', tags)
  return(objects)
}
# create zone lists for living rooms and dormitories
AddZoneList = function(room, tags, fill) {
  tags = tags[grep(room, tags)]
  fields = c('idf_max_extensible_fields')
  values = c(length(tags))
  list = AddFields(fill$item, fields, values)
  list$zones = lapply(tags, function(x, y) append(y, list('zone_name' = x)), list$zones)
  return(list)
}
# apply AddAFNSurf() for all fenestrations
ApplyAFNSurfs = function(storey, index, objects, tags, cond) {
  surfs = mapply(AddAFNSurf, objects, tags, storey, index, SIMPLIFY = FALSE,
                 MoreArgs = list(fill$'AirflowNetwork:MultiZone:Surface', cond))
  surfs[sapply(surfs, is.null)] = NULL
  tags = RnmStorey(names(surfs), storey, index)
  tags = paste0('afn_', tags)
  names(surfs) = tags
  return(surfs)
}
# apply DefBuildSurf() for all surfaces
ApplyBounds = function(storey, index, objects, tags, boundary) {
  surfs = mapply(DefBuildSurf, objects, tags, storey, index, boundary, SIMPLIFY = FALSE)
  tags = RnmStorey(names(surfs), storey, index)
  names(surfs) = tags
  return(surfs)
}
# apply DefFen() for all fenestrations
ApplyFens = function(storey, index, objects, tags) {
  surfs = mapply(DefFen, objects, storey, index, SIMPLIFY = FALSE)
  tags = RnmStorey(tags, storey, index)
  names(surfs) = tags
  return(surfs)
}
# apply AddHVAC() for all types of objects
ApplyHVAC = function(type, fill, tags) {
  rooms = tags[grepl('liv|dorm', tags)]
  objects = lapply(rooms, AddHVAC, type, fill)
  names(objects) = paste0(type, '_', rooms)
  return(objects)
}
# apply AddIntLoad() for all lists of zones
ApplyIntLoads = function(type, fill, tags) {
  if (type == 'equip') {
    tags = tags[grep('liv', tags)]
  } else if (!type %in% c('light', 'people')) {
    stop('Not recognized load source!')
  }
  int_loads = lapply(tags, AddIntLoad, fill, type)
  names(int_loads) = paste0(type, '_', tags)
  return(int_loads)
}
# change an specific coordinate value
ChangeCoordValue = function(vertex, coord, value) {
  vertex[[paste0('vertex_', coord, '_coordinate')]] = value
  return(vertex)
}
# copy zones
CpZones = function(storey, index, height, objects, tags) {
  zones = lapply(objects, AddFields, 'z_origin', height)
  names(zones) = RnmStorey(tags, storey, index)
  return(zones)
}
# define building surface boundaries for floors and roofs
DefBuildSurf = function(object, tag, storey, index, boundary) {
  type = object$surface_type
  object$zone_name = RnmStorey(object$zone_name, storey, index)
  if (type == 'Floor' | type == 'Roof') {
    fields = c('construction_name')
    if (type == 'Floor') {
      fields = append(fields, 'outside_boundary_condition')
      if (storey %in% c('single', 'bot')) {
        fields = append(fields, 'outside_boundary_condition_object')
        values = c('slab', 'OtherSideConditionsModel', 'ground_coupled_oscm')
      } else if (storey %in% c('mid', 'top')) {
        if (boundary == 'surface') {
          fields = append(fields, 'outside_boundary_condition_object')
          values = c('floor', 'Surface', paste0('f', index - 1, '_', TagBoundSurf(tag)))
        } else if (boundary == 'adiabatic') {
          values = c('floor', 'Adiabatic')
        } else {
          stop('Not recognized boundary condition!')
        }
      } else {
        stop('Not recognized storey!')
      }
    } else {
      fields = append(fields, c('sun_exposure', 'wind_exposure', 'outside_boundary_condition'))
      if (storey %in% c('single', 'top')) {
        values = c('roof', 'SunExposed', 'WindExposed', 'Outdoors')
      } else if (storey %in% c('bot', 'mid')) {
        values = c('ceiling', 'NoSun', 'NoWind')
        if (boundary == 'surface') {
          fields = append(fields, 'outside_boundary_condition_object')
          values = append(values, c('Surface', paste0('f', index + 1, '_', TagBoundSurf(tag))))
        } else if (boundary == 'adiabatic') {
          values = append(values, 'Adiabatic')
        } else {
          stop('Not recognized boundary condition!')
        }
      } else {
        stop('Not recognized storey!')
      }
    }
    surf = AddFields(object, fields, values)
    return(surf)
  } else if (type == 'Wall') {
    bound = object$outside_boundary_condition_object
    if (!is.null(bound)) {
      object$outside_boundary_condition_object = RnmStorey(bound, storey, index)
    }
    return(object)
  } else {
    stop('Not recognized surface type!')
  }
}
# define fenestrations
DefFen = function(object, storey, index) {
  object$building_surface_name = RnmStorey(object$building_surface_name, storey, index)
  if (object$surface_type == 'Door') {
    bound = object$outside_boundary_condition_object
    object$outside_boundary_condition_object = RnmStorey(bound, storey, index)
  }
  return(object)
}
# generate balcony
GenBalcony = function(surf, tag, depth, fill) {
  orient = tag %>% str_extract('(?<=wall)[1-4]') %>% as.numeric()
  faces = c(0, ifelse(orient == 1, 4, orient - 1), orient, ifelse(orient == 4, 1, orient + 1), 5)
  balcony = lapply(faces, GenBalcSurf, depth, orient, surf$vertices)
  add = rep(tag, 5)
  names(add) = paste0('shading_', tag, '_', c('floor', paste0('wall', faces[2:4]), 'roof'))
  balcony = mapply(function(x, y, z) c(z, list('vertices' = y, 'base_surface_name' = x)),
                   add, balcony, MoreArgs = list(fill), SIMPLIFY = FALSE)
  return(balcony)
}
# generate balcony surface
GenBalcSurf = function(face, depth, orient, vertices) {
  if (orient %in% c(1, 3)) {
    c = ifelse(orient == 1, -1, 1)
    coord = 'y'
  } else {
    c = ifelse(orient == 2, 1, -1)
    coord = 'x'
  }
  vertex = paste0('vertex_', coord, '_coordinate')
  depth = vertices[[1]][[vertex]] + c*depth
  if (face %in% c(0, 5)) {
    vertices[c(2, 3)] = lapply(vertices[c(2, 3)], ChangeCoordValue, coord, depth)
    if (face == 0) {
      vertices[c(1, 4)] = lapply(vertices[c(1, 4)], ChangeCoordValue, 'z',
                                 vertices[[2]]$vertex_z_coordinate)
      vertices[c(1, 4)] = vertices[c(4, 1)]
      vertices[c(2, 3)] = vertices[c(3, 2)]
    } else {
      vertices[c(2, 3)] = lapply(vertices[c(2, 3)], ChangeCoordValue, 'z',
                                 vertices[[1]]$vertex_z_coordinate)
    }
  } else {
    if (face == orient) {
      vertices = lapply(vertices, ChangeCoordValue, coord, depth)
    } else if ((orient == 1 & face == 4) | face == orient - 1) {
      vertices[c(3, 4)] = lapply(vertices[c(2, 1)], ChangeCoordValue, coord, depth)
    } else {
      vertices[c(1, 2)] = lapply(vertices[c(4, 3)], ChangeCoordValue, coord, depth)
    }
    sill = vertices[[2]]$vertex_z_coordinate + 1.1
    vertices[c(1, 4)] = lapply(vertices[c(1, 4)], ChangeCoordValue, 'z', sill)
  }
  return(vertices)
}
# generate zones and surfaces for all storeys
GenStoreys = function(type, group, n, height, boundary, cond) {
  indexes = 1:n
  storeys = LabelStoreys(n)
  if (type == 'zone') {
    heights = 0:(n -1)*height
    group = mapply(CpZones, storeys, indexes, heights, SIMPLIFY = FALSE,
                   USE.NAMES = FALSE, MoreArgs = list(group, names(group)))
  } else if (type == 'surf') {
    group = mapply(ApplyBounds, storeys, indexes, SIMPLIFY = FALSE, USE.NAMES = FALSE,
                   MoreArgs = list(group, names(group), boundary))
  } else if (type == 'afn') {
    group = mapply(ApplyAFNSurfs, storeys, indexes, SIMPLIFY = FALSE,
                   USE.NAMES = FALSE, MoreArgs = list(group, names(group), cond))
  } else if (type == 'fen') {
    group = mapply(ApplyFens, storeys, indexes, SIMPLIFY = FALSE,
                   USE.NAMES = FALSE, MoreArgs = list(group, names(group)))
  } else {
    stop('Not recognized group of objects!')
  }
  group = flatten(group)
  return(group)
}
# create a string with storeys' label according to the number of floors
  # bot - ground / mid - intermediary / top - roof storey
LabelStoreys = function(n) {
  # n: number of floors
  if (n == 1) { # if the building has only one storey
    storeys = 'single'
  } else { # if the building has multiple storeys
    storeys = c('bot', rep('mid', n - 2), 'top')
  }
  return(storeys)
}
# move fenestration
MoveFenestration = function(vertices, ratio, axis, is_bwc = FALSE) {
  v1 = vertices[[1]]
  v3 = vertices[[3]]
  l = (v3 - v1)/2
  center = (v1 + l)*ratio
  c = ifelse(v3 > v1, 1, -1)
  if (axis != 'z') {
    loop = c(-1, -1, 1, 1)
    vertices = sapply(loop, function(x, y, z) y + x*z, center, l)
  } else {
    if (is_bwc) {
      loop = c(-1, 1, 1, -1)
      vertices = sapply(loop, function(x, y, z) y + x*z, center, l)
    }
  }
  return(vertices)
}
# resize window
ResizeWindow = function(vertices, ratio, wwr, axis) {
  v1 = vertices[[1]]
  v3 = vertices[[3]]
  l = (v3 - v1)/2
  center = (v1 + l)*ratio
  l = l*ratio*wwr
  c = ifelse(v3 > v1, 1, -1)
  if (axis != 'z') {
    loop = c(-1, -1, 1, 1)
  } else {
    loop = c(-1, 1, 1, -1)
  }
  vertices = sapply(loop, function(x, y, z) y + x*z, center, l)
  return(vertices)
}
# add a prefix to a string indicating the storey
  # output: f + index of storey + _ + string
  # example: f1_south_apartment_living / f5_north_apartment_dormitory_1_windows_west
RnmStorey = function(tags, storey, index) {
  if (storey != 'single') {
    tags = paste0('f', index, '_', tags)
  }
  return(tags)
}
# scale door
ScaleDoor = function(axis, ratio, door) {
  coords = paste0('vertex_', 1:4, '_', axis, '_coordinate')
  if (abs(ratio) > 1) {
    coords = SortCoords(ratio, coords)
    vertices = MoveFenestration(door[coords], ratio, axis)
  } else {
    vertices = sapply(door[coords], ScaleVertex, ratio)
  }
  return(vertices)
}
# scale group of objects (apply ScaleObject)
ScaleGroup = function(group, type, ratio, wwr)
  mapply(ScaleObject, group, names(group), MoreArgs = list(type, ratio, wwr), SIMPLIFY = FALSE)
# scale an object
ScaleObject = function(object, tag, type, ratio, wwr) {
  axis = names(ratio)
  if (type == 'zone') {
    coords = paste0(axis[1:2], '_origin')
    object[coords] = mapply(ScaleVertex, object[coords], ratio[1:2])
  } else if (type == 'surf') {
    object$vertices = lapply(object$vertices, ScaleSurf, ratio, axis)
  } else {
    coords = paste0('vertex_', 1:4, '_', rep(axis[1:2], each = 4), '_coordinate')
    if (grepl('door', tag) | grepl('balc', tag)) {
      object[coords] = mapply(ScaleDoor, axis[1:2], ratio[1:2], SIMPLIFY = TRUE,
                              MoreArgs = list(object[coords]))
    } else {
      coords = c(coords, paste0('vertex_', 1:4, rep('_z', 4), '_coordinate'))
      wwr = ifelse(grepl('liv', tag), wwr[['liv']], wwr[['dorm']])
      object[coords] = mapply(ScaleWindow, axis, ratio, grepl('bwc', tag), wwr,
                              SIMPLIFY = TRUE, MoreArgs = list(object[coords]))
    }
  }
  return(object)
}
# scale surface
ScaleSurf = function(vertex, ratio, axis) {
  coords = paste0('vertex_', axis, '_coordinate')
  vertex[coords] = mapply(ScaleVertex, vertex[coords], ratio)
  return(vertex)
}
# scale one vertex of a surface
ScaleVertex = function(coord, ratio) coord*ratio
# scale window
ScaleWindow = function(axis, ratio, is_bwc, wwr, window) {
  coords = paste0('vertex_', 1:4, '_', axis, '_coordinate')
  window = keep(window, names(window) %in% coords)
  if (is_bwc) {
    coords = SortCoords(ratio, coords)
    vertices = MoveFenestration(window[coords], ratio, axis, is_bwc)
  } else {
    vertices = ResizeWindow(window[coords], ratio, wwr, axis)
  }
  return(vertices)
}
# sort coordinates
SortCoords = function(ratio, coords) {
  if (ratio < 0) {
    coords = sort(coords, decreasing = TRUE)
  }
  return(coords)
}
# tag boundary surface for floor and roof
TagBoundSurf = function(tag) {
  opp = ifelse(grepl('floor', tag), str_replace(tag, 'floor', 'roof'),
               str_replace(tag, 'roof', 'floor'))
  return(opp)
}

# main function ####
BuildModel = function(seed_path, area, ratio, height, azimuth, shell_wall, abs_wall,
                      shell_roof, abs_roof, wwr_liv, wwr_dorm, u_window, shgc, open_factor,
                      blind, balcony, mirror, cond, model_path, outputs, construction, fill,
                      setup, geometry, nstrs = 3, boundary = 'surface', scale = TRUE) {
  # seed_path: seed file path
  # area: sum of the long occupancy rooms (living rooms and dormitories) [30 ~ 150]
  # ratio: ratio between the 'y' and the 'x' axis [0.25 ~ 4]
  # height: right foot [2.5 ~ 3.5]
  # azimuth: azimuth angle related to south orientation [0 ~ 360]
  # shell_wall: wall construction materials composition [1 ~ 7]
  # abs_wall: solar absorptance of the walls [0.2 ~ 0.9]
  # shell_roof: roof construction materials composition [1 ~ 4]
  # abs_roof: solar absorptance of the roof [0.2 ~ 0.9]
  # wwr_liv: window to wall ratio in the living room (weighted average) [0.1 ~ 0.8]
  # wwr_dorm: window to wall ratio in the dormitories (weighted average) [0.1 ~ 0.8]
  # u_window: solar transmitance of the windows (mean) [2.8 ~ 5.7]
  # shgc: solar heat gain coefficient of the windows (mean) [0.22 ~ 0.87]
  # open_factor: open factor (weighted average) [0.4 ~ 1]
  # blind: if there is a blind on windows [TRUE or FALSE]
  # balcony: balcony's depth [0 ~ 2]
  # mirror: reflection around y-axis [TRUE or FALSE]
  # model_path: output model path
  # outputs: energyplus simulation outputs
    # possible values: 'mean_temp', 'op_temp', 'air_change', 'therm_bal', 'surf_temp'
  # nstrs: number of storeys
  # boundary: boundaries conditions
    # possible values: 'surface' or 'adiabatic'
  # construction, fill, setup and geometry: auxiliar files
  seed = read_json(seed_path)
  # scale seed
  if (scale) {
    # define seed characteristics
    index = seed_path %>% str_extract('(?<=seed)[0-9]') %>% as.numeric()
    adjust = 1/geometry[[index]]$ratio
    ratio = ratio*adjust
    area_seed = geometry[[index]]$area %>% flatten_dbl() %>% sum()
    multiplier = sqrt(area/(area_seed*ratio))
    inv = ifelse(mirror, -1, 1)
    ratio = c('x' = multiplier*inv, 'y' = ratio*multiplier, 'z' = height/2.6)
    wwr = sqrt(c('liv' = wwr_liv, 'dorm' = wwr_dorm)/0.2)
    seed = mapply(ScaleGroup, seed, c('zone', 'surf', 'fen'),
                  SIMPLIFY = FALSE, MoreArgs = list(ratio, wwr))
  }
  # define the model as the standard setup
  model = setup
  # change rules when mirroring
  if (mirror) {
    model$'GlobalGeometryRules'$geom_rules$starting_vertex_position = 'UpperRightCorner'
    model$'GlobalGeometryRules'$geom_rules$vertex_entry_direction = 'Clockwise'
  }
  # zones
  # determine storey height
  index = grep('roof', names(seed$'BuildingSurface:Detailed'))[1]
  height = seed$'BuildingSurface:Detailed'[[index]]$vertices[[1]]$vertex_z_coordinate
  # generate zones, surfaces, fenestrations and airflow network surfaces
  groups = c('Zone', 'BuildingSurface:Detailed', rep('FenestrationSurface:Detailed', 2))
  items = groups
  items[4] = 'AirflowNetwork:MultiZone:Surface'
  types = c('zone', 'surf', 'fen', 'afn')
  model[items] = mapply(GenStoreys, types, seed[groups], nstrs, height, boundary, cond)
  # add airflow network zones
  model$'AirflowNetwork:MultiZone:Zone' = lapply(names(model$'Zone'), AddAFNZone,
                                                 fill$'AirflowNetwork:MultiZone:Zone')
  names(model$'AirflowNetwork:MultiZone:Zone') = paste0('afn_', names(model$'Zone'))
  # add blind controls to windows in occupied rooms
  if (blind) {
    index = grepl('(liv|dorm.)_window', names(model$'FenestrationSurface:Detailed'))
    tags = names(model$'FenestrationSurface:Detailed')[index]
    model$'WindowShadingControl' = lapply(tags, AddBlindControl, fill$'WindowShadingControl')
    names(model$'WindowShadingControl') = paste0('blind_', paste0(tags))
  }
  # define construction shell
  constructions = append(construction$wall[[shell_wall]], construction$roof[[shell_roof]])
  model$'Construction' = append(model$'Construction', constructions)
  # add zone lists for occupied rooms
  occup_rooms = c('liv', 'dorm')
  model$'ZoneList' = lapply(occup_rooms, AddZoneList, names(model$'Zone'), fill$'ZoneList')
  names(model$'ZoneList') = paste0(occup_rooms, 's')
  # add internal loads from equipments, lights and people
  groups = c('ElectricEquipment', 'Lights', 'People')
  types = c('equip', 'light', 'people')
  model[groups] = mapply(ApplyIntLoads, types, fill[groups],
                         MoreArgs = list(names(model$'ZoneList')))
  # build balcony
  if (balcony > 0) {
    group = model$'BuildingSurface:Detailed'
    tags = names(group)
    index = grep('balc', tags)
    model$'Shading:Zone:Detailed' = GenBalcony %>%
      mapply(group[index], tags[index], balcony, SIMPLIFY = FALSE,
             MoreArgs = list(fill$'Shading:Zone:Detailed'$item)) %>%
      flatten()
  }
  # define open factor for occupied rooms
  pos = 'width_factor_for_opening_factor_2'
  model$'AirflowNetwork:MultiZone:Component:DetailedOpening'$window_opening[[pos]] = open_factor
  # define thermal properties
  model$Building[[1]]$north_axis = azimuth
  model$'WindowMaterial:SimpleGlazingSystem'$vidro$u_factor = u_window
  model$'WindowMaterial:SimpleGlazingSystem'$vidro$solar_heat_gain_coefficient = shgc
  outside_layer_wall = model$'Construction'$'ext_wall'$'outside_layer'
  model$'Material'[[outside_layer_wall]]$'solar_absorptance' = abs_wall
  outside_layer_roof = model$'Construction'$'roof'$'outside_layer'
  model$'Material'[[outside_layer_roof]]$'solar_absorptance' = abs_roof
  # add air conditioning objects
  if (cond == 'hvac') {
    model$'ZoneControl:Thermostat' = AddThermostats(names(model$'ZoneList'),
                                                    fill$'ZoneControl:Thermostat')
    groups = c('ZoneHVAC:IdealLoadsAirSystem', 'ZoneHVAC:EquipmentList',
               'ZoneHVAC:EquipmentConnections')
    model[groups] = mapply(ApplyHVAC, c('hvac', 'equip', 'equip_conn'), fill[groups],
                           MoreArgs = list(names(model$Zone)), SIMPLIFY = FALSE)
  }
  # add output variables
  model$'Output:Variable' = AddOutputs(outputs, fill$'Output:Variable')
  write_json(model, model_path, pretty = TRUE, auto_unbox = TRUE)
}