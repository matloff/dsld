import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri

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
