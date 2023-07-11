# import sys
# import os
# import pandas as pd
# from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from Utils import dsld_Rpy2_IsRDataframe
# from rpy2.robjects import r

dsld = importr("dsld")
qeML = importr("qeML")
# ggplot2 = importr('ggplot2')  # May remove this line
# grdevices = importr('grDevices')

rplotly = importr("plotly")
from IPython.display import display
R_NULL = robjects.NULL

#TODO:
#Implement the yNames to take in both strings and numbers in the vector input

def dsldPyScatterPlot3D(data, sName = R_NULL, yNames = R_NULL, sGroups = R_NULL):
    r_data = dsld_Rpy2_IsRDataframe(data)

    if type(sName) == str:
        sName_r = robjects.StrVector([sName])    # Convert variable name to R character vector
    elif type(sName) == int:
        sName_r = robjects.IntVector([sName])    # Convert number to R integer vector
    else:
        if sName != R_NULL:
            print("ERROR: sName needs to be a string or integer. You Entered: ", sName)
            exit(1)
        else:
            sName_r = sName

    if yNames == R_NULL:
        yNames_r = yNames
    elif all(y.isdigit() for y in yNames):
        yNames_r = robjects.IntVector([int(x) for x in yNames])  # Convert 'yNames' to an R integer vector
    else:
        yNames_r = robjects.StrVector(yNames)

    if sGroups == R_NULL:
        sGroups_r = sGroups
    else:
        sGroups_r = robjects.StrVector(sGroups)    # Convert variable name to R character vector

    scatter_plot = dsld.dsldScatterPlot3D(r_data, sName_r, yNames_r, sGroups_r)

    # Convert the plot to a Plotly widget
    #plot_widget = rplotly.toWidget(scatter_plot)
    plot_widget = rplotly.as_widget(scatter_plot)

    # Display the interactive Plotly graph in Python
    display(plot_widget)

'''
Need to install.packages("plotly") in R
pip install IPython
    python
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D
    import rpy2.robjects as robjects
    robjects.r['data']('pef')
    data = robjects.r('pef')
    dsldPyScatterPlot3D(data, "sex")
    dsldPyScatterPlot3D(data, 4)

Other Examples:
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, "sex", ['occ', 'wageinc', 'wkswrkd'])
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, "sex", ['3', '5', '6'], ['1', '2'])
'''