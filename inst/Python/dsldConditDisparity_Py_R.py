'''

'''
import pandas as pd
import sys
import time
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from Utils import dsld_Rpy2_IsRDataframe
from rpy2.robjects import r


dsld = importr("dsld")
qeML = importr("qeML")
ggplot2 = importr('ggplot2')
grdevices = importr('grDevices')


def dsldPyConditDisparity(data, yName, sName, xName, condits, qeFtn=qeML.qeKNN, minS=50, yLim=None, useLoess=True):
    r_data = dsld_Rpy2_IsRDataframe(data)

    yName_r = robjects.StrVector([yName])  # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])  # Convert variable name to R character vector
    xName_r = robjects.StrVector([xName])  # Convert variable name to R character vector
    condits_r = robjects.StrVector([cond for cond in condits])  # Convert variable name to R character vector
    minS_r = robjects.IntVector([minS])

    yLim_r = robjects.NULL
    print(sName)
    print(sName_r)
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

    grdevices.dev_copy(device=r.png, filename="plot.png")
    grdevices.dev_off()
    # Load the image file in Python
    image = Image.open("plot.png")
    image.show()  # Display the plot using the default image viewer

    return


# robjects.r['data']('pef')
# pef = robjects.r['pef']

# dsldPyConditDisparity(pef, "age", "sex", "wageinc", ['age<=60', 'wkswrkd>=25'])

# grdevices.dev_copy(device=r.png, filename="plot.png", bg=)
# grdevices.dev_off()

'''
    For testing: 
'''


if __name__ == "__main__":
    args = sys.argv
    file_path = args[1]

    data = pd.read_csv(file_path)

    dsldPyConditDisparity(data, args[2], args[3], args[4], sys.argv[5].split(','))


# python dsldConditDisparity_Py_R.py ../../data/pefFixed.csv age gender wageinc age=60,wkswrkd=25