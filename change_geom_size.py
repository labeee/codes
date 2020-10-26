from eppy.modeleditor import IDF

def scaling(scalex=1.55, scaley=1.55,scalez=1.55,
    ratio_of_building_afn=0.85, window_scale=True, shading_scale=False,
    input_file='model.idf',output_name='scaled_model.idf'):

    # This function multiplies the vertices of the IDF model by a
    # determined value.

    # scalex - Scale factor to be multiplied by the x vertices.
    # scaley - Scale factor to be multiplied by the y vertices.
    # scalez - Scale factor to be multiplied by the z vertices.
    # ratio_of_building_afn - New ratio of building value for AFN.
    # window_scale - Condition to change geometry of windows too.
    # shading_scale - Condition to change geometry of shading too.
    # input_file - The IDF file to be edited.
    # output_name - The name of the output file to be created.

    print(output_name)

    # reading IDF file
    idf = IDF(input_file)

    # changing IDF fields
    ## AirflowNetwork:SimulationControl
    for i,_ in enumerate(idf.idfobjects["AirflowNetwork:SimulationControl"]):
        idf.idfobjects["AirflowNetwork:SimulationControl"][i]['Ratio_of_Building_Width_Along_Short_Axis_to_Width_Along_Long_Axis'] = ratio_of_building_afn

    ## BuildingSurface:Detailed
    for i,_ in enumerate(idf.idfobjects["BuildingSurface:Detailed"]):
        number = 1
        while True:
            try:
                idf.idfobjects["BuildingSurface:Detailed"][i][f'Vertex_{number}_Xcoordinate'] = idf.idfobjects["BuildingSurface:Detailed"][i][f'Vertex_{number}_Xcoordinate']*scalex
                idf.idfobjects["BuildingSurface:Detailed"][i][f'Vertex_{number}_Ycoordinate'] = idf.idfobjects["BuildingSurface:Detailed"][i][f'Vertex_{number}_Ycoordinate']*scaley
                idf.idfobjects["BuildingSurface:Detailed"][i][f'Vertex_{number}_Zcoordinate'] = idf.idfobjects["BuildingSurface:Detailed"][i][f'Vertex_{number}_Zcoordinate']*scalez
                number += 1
            except:
                break

    ## FenestrationSurface:Detailed
    if window_scale:
        for i,_ in enumerate(idf.idfobjects["FenestrationSurface:Detailed"]):
            for number in range(1,5):
                idf.idfobjects["FenestrationSurface:Detailed"][i][f"Vertex_{number}_Xcoordinate"] = idf.idfobjects["FenestrationSurface:Detailed"][i][f"Vertex_{number}_Xcoordinate"]*scalex
                idf.idfobjects["FenestrationSurface:Detailed"][i][f"Vertex_{number}_Ycoordinate"] = idf.idfobjects["FenestrationSurface:Detailed"][i][f"Vertex_{number}_Ycoordinate"]*scaley
                idf.idfobjects["FenestrationSurface:Detailed"][i][f"Vertex_{number}_Zcoordinate"] = idf.idfobjects["FenestrationSurface:Detailed"][i][f"Vertex_{number}_Zcoordinate"]*scalez

    ## Shading:Building:Detailed
    if shading_scale:
        for i,_ in enumerate(idf.idfobjects["Shading:Building:Detailed"]):
            for number in range(1,5):
                idf.idfobjects["Shading:Building:Detailed"][i][f'Vertex_{number}_Xcoordinate'] = idf.idfobjects["Shading:Building:Detailed"][i][f'Vertex_{number}_Xcoordinate']*scalex
                idf.idfobjects["Shading:Building:Detailed"][i][f'Vertex_{number}_Ycoordinate'] = idf.idfobjects["Shading:Building:Detailed"][i][f'Vertex_{number}_Ycoordinate']*scaley
                idf.idfobjects["Shading:Building:Detailed"][i][f'Vertex_{number}_Zcoordinate'] = idf.idfobjects["Shading:Building:Detailed"][i][f'Vertex_{number}_Zcoordinate']*scalez

    ## Zone
    ## Shading:Building:Detailed
    for i,_ in enumerate(idf.idfobjects["Zone"]):
        idf.idfobjects["Zone"][i]["X_Origin"] = idf.idfobjects["Zone"][i]["X_Origin"]*scalex
        idf.idfobjects["Zone"][i]["Y_Origin"] = idf.idfobjects["Zone"][i]["Y_Origin"]*scaley
        idf.idfobjects["Zone"][i]["Z_Origin"] = idf.idfobjects["Zone"][i]["Z_Origin"]*scalez

    # writing  IDF file
    idf.saveas(output_name)

## Test function changing values on the following lines: ---------------

# Define the name of the input file here
IDF.setiddname('C:/EnergyPlusV9-0-1/Energy+.idd')
IDF.setiddname('C:/EnergyPlusV9-0-1/Energy+.idd')
FILE_NAME = '../idfs/uni/ac1_Caso2.idf'

# Define the input parameters here
scaling(scalex=2, scaley=.5,scalez=1, ratio_of_building_afn=0.85,
    window_scale=True, shading_scale=True,
    input_file= FILE_NAME, output_name= FILE_NAME[:-4] + '_changed.idf')
