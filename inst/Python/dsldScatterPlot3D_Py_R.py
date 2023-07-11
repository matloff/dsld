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

def dsldPyScatterPlot3D(data, sName = R_NULL, yNames = R_NULL, sGroups = R_NULL, sortedBy = "Name", numGroups = 8, maxPoints = R_NULL, xlim = R_NULL, ylim = R_NULL, zlim = R_NULL, main = R_NULL, colors = ["Paired"], opacity = "1", pointSize = "8"):
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

    sortedBy_r = robjects.StrVector([sortedBy])

    numGroups_r = robjects.IntVector([numGroups])

    if maxPoints == R_NULL:
        maxPoints_r = maxPoints
    else:
        maxPoints_r = robjects.IntVector([maxPoints])

    if xlim == R_NULL:
        xlim_r = xlim
    else:
        xlim_r = robjects.IntVector([int(x) for x in xlim])

    if ylim == R_NULL:
        ylim_r = ylim
    else:
        ylim_r = robjects.IntVector([int(x) for x in ylim])

    if zlim == R_NULL:
        zlim_r = zlim
    else:
        zlim_r = robjects.IntVector([int(x) for x in zlim])

    if main == R_NULL:
        main_r = main
    else:
        main_r = robjects.StrVector([main])    # Convert variable name to R character vector

    colors_r = robjects.StrVector(colors)

    opacity_r = robjects.StrVector([opacity])

    pointSize_r = robjects.StrVector([pointSize])

    scatter_plot = dsld.dsldScatterPlot3D(r_data, sName_r, yNames_r, sGroups_r, sortedBy_r, numGroups_r, maxPoints_r, xlim_r, ylim_r, zlim_r, main_r, colors_r, opacity_r, pointSize_r)

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
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, "sex", ['3', '5', '6'], ['1', '2'], "Frequency", "2", "10000", ['80', '90'])
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, xlim = ['80', '90'])
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, ylim = ['0', '50000'])
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, zlim = ['0', '10'])
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, colors = ["salmon", "seagreen"])
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, colors = ["salmon"])
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, pointSize = "20")
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D;import rpy2.robjects as robjects;robjects.r['data']('pef');data = robjects.r('pef');dsldPyScatterPlot3D(data, colors = ["yellow", "orange"], maxPoints = "20", ylim = ["50000", "100000"], xlim = ["20", "80"], pointSize = "5")
'''