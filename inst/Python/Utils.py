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