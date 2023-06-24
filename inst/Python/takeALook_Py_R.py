from rpy2.robjects.packages import importr

# Parsing CSV file and storing into a Python dataframe
import pandas as pd
from rpy2.robjects import pandas2ri
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

devtools = importr("devtools")

# This below line may need to be commented
#devtools.install_local("/Users/tahaabdullah/Documents/GitHub/dsld")

dsld = importr("dsld")

# Setting Python args
yName = 'col1'
sName = 'col2'
#maxFeatureSetSize = 1

# Parsing CSV file and storing into a Python dataframe
def pandas_to_r_dataframe(pandas_df):
    pandas2ri.activate()
    r_dataframe = pandas2ri.py2rpy(pandas_df)
    return r_dataframe

pandas_df = pd.DataFrame({'col1': [1, 2, 3], 'col2': [1, 2, 3]})
# Convert pandas DataFrame to R DataFrame
data_r = pandas_to_r_dataframe(pandas_df)


# Assuming you have the required arguments in Python variables

yName_r = robjects.StrVector([yName])  # Convert variable name to R character vector
sName_r = robjects.StrVector([sName])  # Convert variable name to R character vector
#maxFeatureSetSize_r = robjects.IntVector([maxFeatureSetSize])  # Convert number to R integer vector

dsldTakeALookAround = dsld.dsldTakeALookAround
df_r = dsldTakeALookAround(data_r, yName_r, sName_r)#, maxFeatureSetSize_r)

#Result stored in a python dataframe
df_py = pd.DataFrame(df_r)
