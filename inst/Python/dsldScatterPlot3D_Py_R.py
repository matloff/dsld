#TODO:
#Implement the yNames to take in both strings and numbers in the vector input
#Test validateInputSP3D, testing, documentation
#Suggestion: In error messages, give users examples of input and if the argument can be left empty



'''
dsldScatterPlot3D_Py_R.py is the python interface for dsldScatterPlot3D in the dsld R package.
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
dsld contains this file's main R function, dsldScatterPlot3D and the dataset svcensus.rData
qeML contains the dataset mlb, mlb1, and more
forcats is used for reordering factor levels
rplotly is used to create a plotly widget to aid in displaying graph
'''
dsld    = importr("dsld")
qeML    = importr("qeML")
fairml = importr("fairml")
forcats = importr('forcats')
rplotly = importr("plotly")

# TODO: Fix error: function displays error msg even when input is fine
def validateInputSP3D(sName = R_NULL, yNames = R_NULL, sGroups = R_NULL,
                      sortedBy = "Name", numGroups = 8, maxPoints = R_NULL,
                      xlim = R_NULL, ylim = R_NULL, zlim = R_NULL, main = R_NULL,
                      colors = ["Paired"], opacity = "1", pointSize = "8"):
    if type(sName) != str and (type(sName) == list and not all(isinstance(s, str) for s in sName)):
        print('Error: sName must be a list of string. Entered type: ', type(sName))
        exit(ERROR)
    if type(yNames) != str and (type(yNames) == list and not all(isinstance(y, str) for y in yNames)):
        print('Error: yNames must be a list of string. Entered type: ', type(yNames))
        exit(ERROR)
    if type(sGroups) != str and (type(sGroups) == list and not all(isinstance(s,str) for s in sGroups)):
        print('Error: sGroups must be a list of strings. Entered type: ', type(sGroups))
    if type(sortedBy) != str and sortedBy != "Name" or sortedBy != "Frequency" or sortedBy != "Frequency-Descending":
        print('ERROR: sortedBy must be "Name", "Frequency", or "Frequency-Descending"')
        exit(ERROR)
    if type(numGroups) != int:
        print('Error: numGroups must be an int type. Entered type:', type(numGroups))
        exit(ERROR)
    if type(maxPoints) != int:
        print('Error: maxPoints must be an int type. Entered type:', type(maxPoints))
        exit(ERROR)
    if type(xlim) != str and (type(xlim) == list and not all (isinstance(x, str) for x in xlim)):
        print('ERROR: xlim must be a list of strings. Entered type: ', type(xlim))
        exit(ERROR)
    if type(ylim) != str and (type(ylim) == list and not all (isinstance(x, str) for x in ylim)):
        print('ERROR: ylim must be a list of strings. Entered type: ', type(ylim))
        exit(ERROR)
    if type(zlim) != str and (type(zlim) == list and not all (isinstance(x, str) for x in zlim)):
        print('ERROR: zlim must be a list of strings. Entered type: ', type(zlim))
        exit(ERROR)
    if type(main) != str and (type(main) == list and not all(isinstance(y, str) for y in main)):
        print('Error: main must be a list of string. Entered type:', type(main))
        exit(ERROR)
    if type(colors) != str and (type(colors) == list and not all(isinstance(y, str) for y in colors)):
        print('Error: colors must be a list of string. Entered type: ', type(colors))
        exit(ERROR)
    if type(opacity) != int:
        print('Error: opacity must be an int type. Entered type:', type(opacity))
        exit(ERROR)
    if type(pointSize) != int:
        print('Error: opacity must be an int type. Entered type:', type(opacity))
        exit(ERROR)
    
def dsldPyScatterPlot3D(data, yNames = R_NULL, sName = R_NULL, sGroups = R_NULL, sortedBy = "Name", numGroups = 8, maxPoints = R_NULL, xlim = R_NULL, ylim = R_NULL, zlim = R_NULL, main = R_NULL, colors = ["Paired"], opacity = "1", pointSize = "8"):
    # ************************** ARGUMENTS *******************************************

    # Type validation of everything except for data
    # TODO: Fix error: see above todo
    # validateInputSP3D(sName,yNames,sGroups,sortedBy,numGroups,maxPoints,xlim,ylim,zlim,main,colors,opacity,pointSize)

    # Data conversion handled by Utils function
    r_data = dsld_Rpy2_IsRDataframe(data)
    robjects.r.assign("r_data", r_data)  

    # Identify columns with string data
    robjects.r('string_cols <- sapply(r_data, is.character)')
    # Convert selected columns to factors
    robjects.r('r_data[string_cols] <- lapply(r_data[string_cols], as.factor)')

    r_data = robjects.r("r_data")    

    # R function requires factorized sName, so we will do that for the user
    robjects.r.assign("r_data", r_data)                      
    robjects.r(f"r_data${sName} <- as.factor(r_data${sName})")  
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
    maxPoints_r = maxPoints if (maxPoints == R_NULL) else robjects.IntVector([maxPoints])
    xlim_r = xlim if (xlim == R_NULL) else r.objects.IntVector([int(x) for x in xlim])
    ylim_r = ylim if (ylim == R_NULL) else r.objects.IntVector([int(x) for x in ylim])
    zlim_r = zlim if (zlim == R_NULL) else r.objects.IntVector([int(x) for x in zlim])
    main_r = main if (main == R_NULL) else r.objects.strVector([main])
    colors_r = robjects.StrVector(colors)
    opacity_r = robjects.StrVector([opacity])
    pointSize_r = robjects.StrVector([pointSize])
    
    # All necessary arguments are in R format at this point
    # ************************** END ARGUMENTS *******************************************

    # ************************** RETURN VALUE *******************************************
    scatter_plot = dsld.dsldScatterPlot3D(r_data, yNames_r, sName_r, sGroups_r, sortedBy_r, numGroups_r, maxPoints_r, xlim_r, ylim_r, zlim_r, main_r, colors_r, opacity_r, pointSize_r)
    # Convyrt the plot to a Plotly widget and display it
    plot_widget = rplotly.as_widget(scatter_plot)
    display(plot_widget)
    # ************************** END FUNCTION *******************************************

#************************** OS SHELL FUNCTIONALITY *************************************
# TODO: OS Shell functionality is INCOMPLETE
# if __name__ == "__main__":
#     #args = sys.argv
#     cmd, data, sName, yNames, sGroups, sortedBy, numGroups, maxPoints, xlim, ylim, zlim, main, colors, opacity, pointSize = sys.argv
#     # cmd, data = sys.argv
#     data = pd.read_csv()
#     # print(data)
#     # dsldPyScatterPlot3D(data)
#     dsldPyScatterPlot3D(data, sName, yNames.split(','), sGroups.split(','), sortedBy, int(numGroups), maxPoints, xlim.split(','), ylim.split(','), zlim.split(','), main, colors.split(','), opacity, pointSize)
#************************** END OS SHELL *******************************************


'''
# Need to install.packages("plotly") in R
# Need to install Python's IPython
# through the following command (enter in terminal): pip install IPython
    python

    # Example 1
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D
    import rpy2.robjects as robjects
    robjects.r['data']('svcensus')
    data = robjects.r('svcensus')
    dsldPyScatterPlot3D(data, ['occ', 'wageinc', 'wkswrkd'], "gender")
    # can also reenter yNames as string numbers

    # Example 2
    from dsldScatterPlot3D_Py_R import dsldPyScatterPlot3D
    import rpy2.robjects as robjects
    robjects.r['data']('law.school.admissions')
    data = robjects.r('law.school.admissions')
    dsldPyScatterPlot3D(data, yNames=['ugpa', 'lsat', 'age'], sName='race1')