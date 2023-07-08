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

def dsldPyScatterPlot3D(data):
    r_data = dsld_Rpy2_IsRDataframe(data)
    
    scatter_plot = dsld.dsldScatterPlot3D(r_data)

    # Convert the plot to a Plotly widget
    plot_widget = rplotly.toWidget(scatter_plot)

    # Display the interactive Plotly graph in Python
    display(plot_widget)

'''
    python
    from dsldRpy2Scatterplot import dsldPyScatterPlot3D
    import rpy2.robjects as robjects
    robjects.r['data']('pef')
    data = robjects.r('pef')
    dsldPyScatterPlot3D(data)
'''