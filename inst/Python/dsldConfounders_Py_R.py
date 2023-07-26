'''
dsldConfounders_Py_R.py is the python interface for dsldConfounders in the dsld R package.
Utils contains helper functions which are used in multiple files
rpy2 is used handle dsld function calls from R
pandas processes dataset into a panda's data frame before performing any computation
Ipython.display is used to display the interactive scatter plot
sys is used for OS shell
'''

from Utils import dsld_Rpy2_IsRDataframe, R_NULL, ERROR
from rpy2.robjects.packages import importr
from IPython.display import display
import rpy2.robjects as robjects
import pandas as pd
import sys #remove this if we aren't doing shell?

'''
Importing r packages {dsld, qeML, forcats, plotly} into Python through rpy2
dsld contains this file's main R function, dsldConfounders and the dataset svcensus.rData
qeML contains the dataset mlb, mlb1, and more
forcats is used for reordering factor levels
rplotly is used to create a plotly widget to aid in displaying graph
'''
dsld    = importr("dsld")
rplotly = importr("plotly")

def dsldPyConfounders(data, sName = R_NULL, fill = False):
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
        if sName != R_NULL:
            print("ERROR: sName needs to be a string or integer. You Entered: ", sName)
            exit(1)
        else:
            sName_r = sName

    fill_r = robjects.BoolVector([fill])

    result = dsld.dsldConfounders(r_data, sName_r, fill_r)
    
    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************
    
    # ************************** RETURN VALUE *******************************************
    # Convert the plot to a Plotly widget and display it
    plot_widget = rplotly.as_widget(result)
    display(plot_widget)
    # ************************** END FUNCTION *******************************************

#************************** OS SHELL FUNCTIONALITY *************************************
# TODO: OS Shell functionality is INCOMPLETE
if __name__ == "__main__":
    cmd, data, sName, fill = sys.argv
    data = pd.read_csv()
    dsldPyConfounders(data, sName, bool(fill))
#************************** END OS SHELL *******************************************

'''
Need to install.packages("plotly") in R
pip install IPython
    python
    from dsldConfounders_Py_R import dsldPyConfounders
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyConfounders(data)

Other Examples:
    from dsldConfounders_Py_R import dsldPyConfounders;import rpy2.robjects as robjects;robjects.r['data']('svcensus');data = robjects.r('svcensus');dsldPyConfounders(data)
    from dsldConfounders_Py_R import dsldPyConfounders;import pandas as pd;data = pd.read_csv('../../data/svcensusFixed.csv');dsldPyConfounders(data)
'''