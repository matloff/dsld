import pandas as pd
import numpy as np
from PIL import Image
import rpy2.robjects as robjects
from rpy2.robjects import StrVector, FloatVector, ListVector
from rpy2.robjects import pandas2ri

# For handling null arguments
R_NULL = robjects.NULL

# Use as exit status for errors
ERROR = 1

""" ***************************DSLD Class section*************************** """
# Python dsldDiffModel Class to hold R equivalent class
class DsldDiffModel:
    def __init__(self, listVector):
        self.yName = ""
        self.sName = "" 
        self.model = None
        self.newData = None
        self.summary = None
        self.coef = None
        self.data = None

        index = 0
        for key in self.__dict__:
            if isinstance(listVector[index], StrVector):
                self.__dict__[key] = listVector[index][0]
            elif isinstance(listVector[index], FloatVector):
                self.__dict__[key] = list(listVector[index])
            elif isinstance(listVector[index], ListVector):
                self.__dict__[key] = {nested_key: np.asarray(nested_value, dtype='object') for nested_key, nested_value in listVector[index].items()}
            index = index + 1

# Python dsldLinear Class to hold R equivalent class
class DsldLinear:
    def __init__(self, r_object):
        self.dsldModel = {}

        index = 0
        for key in r_object.names:
            diffModelObj = DsldDiffModel(r_object[index])
            self.dsldModel[key] = diffModelObj
            index = index + 1

""" *********************** END OF DSLD Class section*********************** """

# This function converts a pandas data frame into an R data frame
def dsld_Rpy2_PandasToRDataframe(pandas_df):
    pandas2ri.activate()
    r_dataframe = pandas2ri.py2rpy(pandas_df)
    return r_dataframe

# This function checks if the data input from the user is in
# R data frame, pandas' data frame or a different type of data frame.
# The function converts the data into r's data frame or
# return -1 which represent an error.
def dsld_Rpy2_IsRDataframe(data):
    if isinstance(data, robjects.vectors.DataFrame):
        return data
    elif isinstance(data, pd.DataFrame):
        return dsld_Rpy2_PandasToRDataframe(data)
    else: # Error case
        print("Error: not Rdata or Pandas Dataframe")
        return -1

# This function prints the usage for executing dsldTakeALookAround_Py_R.py
# from the shell command.
def print_takeALookAround_usage():
    msg = """
        Usage: python [dsldTakeALookAround_Py_R.py_path] [data_path] [yName] [sName] [maxFeatureSize]

        Parameters:
            [dsldTakeALookAround_Py_R.py_path] (string): Path to dsldTakeALookAround_Py_R.py file.
            [data_path] (string): Path to your data file.
            [yName] (string): Name of variable to be predicted.
            [sName] (string): Name of your sensitive variable.
            [maxFeatureSize] (int): Maximum number of combinations of features to be included in the dataframe; 
                                    default argument set as: maxFeatureSetSize = (ncol(data) - 2)

        Returns:
            pandas.DataFrame: The function returns a pandas data frame from the R data frame from the R environment. 
        """
    print(msg)

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
