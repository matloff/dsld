'''
    This file contains the interface code for calling the dsldConditDisparity from dsld R package.
    The code uses rpy2 to handle dsld functions call from R and pandas library to check if
    users data input is in pandas data frame before doing any computation
'''
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


# When saving our plot from R, it comes up with a transparent background.
# This function is designed to set the background of the saved image to
# a white background. 
def changeBg(path):
    # Open the image
    image_path = path
    image = Image.open(image_path)
    image = image.convert("RGBA")  # Convert to RGBA mode

    # Create a new image with a white background
    new_image = Image.new("RGB", image.size, "white")

    # Paste the original image onto the new image with alpha blending
    new_image.paste(image, (0, 0), image)

    # Save the modified image
    output_path = path
    new_image.save(output_path)

# This is the interface function for R's dsldConditDisparity function
# The arguments are converted into R data type before calling dsldConditDisparity function
# This function uses qeML's qeKNN function as default argument for qeFtn
def dsldPyConditDisparity(data, yName, sName, xName, condits, qeFtn="qeKNN", minS=50, yLim=None, useLoess=True):
    r_data = dsld_Rpy2_IsRDataframe(data)

    robjects.r.assign("r_data", r_data)  # Assign the 'r_data' variable to R
    # robjects.r('r_data$race <- as.factor(r_data$race)')  # Call as.factor() on the 'race' column
    robjects.r(f"r_data${sName} <- as.factor(r_data${sName})")  # Call as.factor() on the 'race' column
    r_data = robjects.r("r_data")  # Assign the modified R dataframe back to Python

    yName_r = robjects.StrVector([yName])  # Convert variable name to R character vector
    sName_r = robjects.StrVector([sName])  # Convert variable name to R character vector
    xName_r = robjects.StrVector([xName])  # Convert variable name to R character vector
    condits_r = robjects.StrVector([cond for cond in condits])  # Convert variable name to R character vector
    minS_r = robjects.IntVector([minS])    # Convert variable name to R;s number type

    if hasattr(qeML, qeFtn) and callable(getattr(qeML, qeFtn)):
        # Call the function
       qeFtn_r = getattr(qeML, qeFtn)
    else:
        print(f"ERROR: qeML do not have function name: '{qeFtn}'\n")
        return

    yLim_r = robjects.NULL

    if yLim is not None:
        yLim_r = robjects.IntVector([int(x) for x in yLim])
        print(yLim_r)

    useLoess_r = robjects.BoolVector([useLoess])

    dsld.dsldConditDisparity(r_data, yName_r, sName_r, xName_r, condits_r, qeFtn_r, minS_r, yLim_r, useLoess_r)

    # Copy and saves the image as plot.png
    plot_filename = "condits_disparity_plot.png"
    grdevices.dev_copy(device=r.png, filename=plot_filename)
    grdevices.dev_off()

    # Set background of image saved to white
    changeBg(plot_filename)

    # Load the image file in Python
    image = Image.open(plot_filename)
    image.show()  # Display the plot using the default image viewer

    # Close the displayed image
    image.close()
    # Delete the image file
    os.remove(plot_filename)


# The code below is for testing purposes. We'll remove it 
# once we're done working on the shell command inputs
# The code uses pef data for testing
# robjects.r['data']('pef')
# pef = robjects.r['pef']
# print(robjects.r['head'](pef))

# dsldPyConditDisparity(pef, "age", "sex", "wageinc", ['age<=60', 'wkswrkd>=25'])
'''
# Load the dataset 'compas' into R
robjects.r['data']('compas')
compas = robjects.r['compas']
# print(robjects.r['head'](compas))
# Convert the 'two_year_recid' variable to numeric
robjects.r('compas$two_year_recid <- as.numeric(as.character(compas$two_year_recid) == "Yes")')
compas = robjects.r['compas']
# print(robjects.r['head'](compas))

dsldPyConditDisparity(compas,'two_year_recid', 'race', 'age', ['priors_count <= 4','decile_score>=6'], qeML.qeKNN)
'''



'''
    For testing: 
'''


# We'll do some additional work on the shell command
# The code below is not currently working properly

if __name__ == "__main__":
    args = sys.argv
    file_path = args[1]

    data = pd.read_csv(file_path)

    #dsldPyConditDisparity(data, args[2], args[3], args[4], sys.argv[5].split(','))

    dsldPyConditDisparity(data, args[2], args[3], args[4], sys.argv[5].split(','), qeFtn = args[6])

'''
    # Test cases: Before running, go to /dsld/inst/Python

    # Running from the OS Shell
    python dsldConditDisparity_Py_R.py ../../data/compasNumericFixed.csv two_year_recid race age 'priors_count<=4','decile_score>=6' qeGBoost

    # Running from the Python Shell Prompt
    python # Open Python shell prompt
    from dsldConditDisparity_Py_R import dsldPyConditDisparity
    import pandas as pd
    data = pd.read_csv('../../data/compasNumericFixed.csv')
    dsldPyConditDisparity(data, 'two_year_recid', 'race', 'age', ['priors_count <= 4','decile_score>=6'], 'qeGBoost')
'''