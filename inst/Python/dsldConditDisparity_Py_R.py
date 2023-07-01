'''

'''

import sys
import time
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from Utils import dsld_Rpy2_IsRDataframe


dsld = importr("dsld")
qeML = importr("qeML")
ggplot2 = importr('ggplot2')


def dsldPyConditDisparity(data, yName, sName, xName, condits, qeFtn=qeML.qeKNN, minS=50, yLim=None, useLoess=True):
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName_r = robjects.StrVector([yName])  # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])  # Convert variable name to R character vector
    xName_r = robjects.StrVector([xName])  # Convert variable name to R character vector
    condits_r = robjects.StrVector([cond for cond in condits])  # Convert variable name to R character vector
    minS_r = robjects.IntVector([minS])

    yLim_r = robjects.NULL

    if yLim is not None:
        yLim_r = robjects.IntVector([int(x) for x in yLim])
        print(yLim_r)

    useLoess_r = robjects.BoolVector([useLoess])

    dsld.dsldConditDisparity(r_data, yName_r, sName_r, xName_r, condits_r, qeFtn, minS_r, yLim_r, useLoess_r)
    # time.sleep(True)

    # # Save the plot as an image file in R
    # robjects.r('ggsave("fig.png")')
    # #robjects.r('png(file="plot.png")')
    # # Load the image file in Python
    # image = Image.open("fig.png")
    # image.show()  # Display the plot using the default image viewer

    return


robjects.r['data']('pef')
pef = robjects.r['pef']

#print(pef)

dsldPyConditDisparity(pef, "age", "sex", "wageinc", ['age <= 60', 'wkswrkd>=25'])

'''
    For testing: 
'''


'''if __name__ == "__main__":
    args = sys.argv'''