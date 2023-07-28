'''
    dsldConfounders_Py_R.py is the python interface for dsldConfounders in the dsld R package.
    Utils contains helper functions which are used in multiple files
    rpy2 is used handle dsld function calls from R
    pandas processes dataset into a panda's data frame before performing any computation
    Ipython.display is used to display the interactive scatter plot
    sys is used for OS shell
'''

from Utils import dsld_Rpy2_IsRDataframe, ERROR
from rpy2.robjects.packages import importr
import rpy2.robjects as robjects
from IPython.display import display
import pandas as pd
import sys

'''
    Importing r packages {dsld, plotly} into Python through rpy2
    dsld contains this file's main R function, dsldConfounders and the dataset svcensus.rData
    rplotly is used to create a plotly widget to aid in displaying graph
'''
dsld    = importr("dsld")
rplotly = importr("plotly")

def dsldPyConfounders(data, sName):
    # ************************** ARGUMENTS *******************************************
    
    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data)
    robjects.r.assign("r_data", r_data)  

    # Identify columns with string data
    robjects.r('string_cols <- sapply(r_data, is.character)')
    # Convert selected columns to factors
    robjects.r('r_data[string_cols] <- lapply(r_data[string_cols], as.factor)')

    r_data = robjects.r("r_data")    

    if type(sName) == str:
        sName_r = robjects.StrVector([sName])    # Convert variable name to R character vector
    elif type(sName) == int:
        sName_r = robjects.IntVector([sName])    # Convert number to R integer vector
    else:
        if sName != None:
            print("ERROR: sName needs to be a string or integer. You Entered: ", sName)
            exit(ERROR)
        else:
            print('Error: sName needs to be a string or integer. It cannot be null')
            exit(ERROR)

    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************
    
    # ************************** RETURN VALUE *******************************************

    result = dsld.dsldConfounders(r_data, sName_r)

    # Convert the plot to a Plotly widget and display it
    plot_widget = rplotly.as_widget(result)
    display(plot_widget)

    # ************************** END FUNCTION *******************************************

# ************************** OS SHELL FUNCTIONALITY *************************************
# TODO: OS Shell functionality is INCOMPLETE
if __name__ == "__main__":
    cmd, data, sName = sys.argv
    data = pd.read_csv(data)
    dsldPyConfounders(data, sName)
# ************************** END OS SHELL *******************************************

'''
# Need to install.packages("plotly") in R
# Need to install Python's IPython
# through the following command (enter in terminal): pip install IPython
    python
    from dsldConfounders_Py_R import dsldPyConfounders
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyConfounders(data, "educ")

# Other Examples:
    from dsldConfounders_Py_R import dsldPyConfounders;import rpy2.robjects as robjects;robjects.r['data']('svcensus');data = robjects.r('svcensus');dsldPyConfounders(data, "educ")
'''