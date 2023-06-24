import pandas as pd
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

devtools = importr("devtools")

# This below line may need to be commented
devtools.install_local("/Users/tahaabdullah/Documents/GitHub/dsld")

dsld = importr("dsld")

# Setting Python args
yName = 'Electric Range'
sName = 'Base MSRP'
maxFeatureSetSize = 2

# Parsing CSV file and storing into a Python dataframe
data_r = robjects.r("read.csv('Electric_Vehicle_Population_Data.csv')")  # Read CSV file in R

# Assuming you have the required arguments in Python variables

yName_r = robjects.StrVector([yName])  # Convert variable name to R character vector
sName_r = robjects.StrVector([sName])  # Convert variable name to R character vector
maxFeatureSetSize_r = robjects.IntVector([maxFeatureSetSize])  # Convert number to R integer vector

dsldTakeALookAround = dsld.dsldTakeALookAround
df_r = dsldTakeALookAround(data_r, yName_r, sName_r, maxFeatureSetSize_r)

#Result stored in a python dataframe
df_py = pd.DataFrame(df_r)