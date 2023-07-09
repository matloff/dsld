import sys
import os
import pandas as pd
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from Utils import dsld_Rpy2_IsRDataframe
from rpy2.robjects import r

dsld = importr("dsld")
qeML = importr("qeML")
ggplot2 = importr('ggplot2')  # May remove this line
grdevices = importr('grDevices')

rplotly = importr("plotly")
from IPython.display import display

def dsldPyScatterPlot3D(data, sName = None, yName = None):
    r_data = dsld_Rpy2_IsRDataframe(data)

    if type(sName) == str:
        sName_r = robjects.StrVector([sName])    # Convert variable name to R character vector
    elif type(sName) == int:
        sName_r = robjects.IntVector([sName])    # Convert number to R integer vector
    else:
        if sName != None:
            print("ERROR: sName needs to be a string or integer. You Entered: ", sName)
            exit(1)
    
    # Figure out if we can do this without so many if statements
    if sName == None & yName == None:
        scatter_plot = dsld.dsldScatterPlot3D(r_data)
    else:
        scatter_plot = dsld.dsldScatterPlot3D(r_data, sName_r)

    # Convert the plot to a Plotly widget
    plot_widget = rplotly.toWidget(scatter_plot)

    # Display the interactive Plotly graph in Python
    display(plot_widget)

'''
Need to install.packages("plotly") in R
pip install IPython
    python
    from dsldRpy2Scatterplot import dsldPyScatterPlot3D
    import rpy2.robjects as robjects
    robjects.r['data']('pef')
    data = robjects.r('pef')
    dsldPyScatterPlot3D(data, "sex")
    dsldPyScatterPlot3D(data, 4)
'''