import sys
import pyRserve
import pandas as pd

port = 6311

# Connect to R server on default port 6311
conn = pyRserve.connect(host='localhost', port=port)

# This one might work with some more modification
def convert_to_r_dataframe(pandas_df):
    # Create an R DataFrame with the correct number of rows
    num_rows = len(pandas_df)
    conn.r(f'r_df <- data.frame(matrix(nrow={num_rows}))')

    # Iterate over the columns of the Pandas DataFrame
    for col_name in pandas_df.columns:
        # Convert the column values to an R vector
        col_values = pandas_df[col_name].values.tolist()
        r_vector = 'c(' + ', '.join(str(val) for val in col_values) + ')'

        # Add the column to the R DataFrame
        r_code = f'r_df${col_name} <- {r_vector}'
        conn.r(r_code)
    return


def dsldPyTakeALookAround(data, yName, sName, maxFeatureSize=None):
    conn.r('library(dsld)')

    convert_to_r_dataframe(data)

    # Used iris data for testing and it worked
    # When testing with iris data replace data argument inside dsldTakeALookAround to iris
    # ex: dsldTakeALookAround(iris, yName, sName)
    # Can test it with the follwoing shell command:
    # python dsldTakeLookAround_PyRServe.py iris Sepal.Length Petal.Length
    # uncomment code below before running it
    #conn.r('data(iris)')

    conn.r.assign("yName", yName)
    conn.r.assign("sName", sName)

    if maxFeatureSize is None:
        conn.r('dsld_result <- dsldTakeALookAround(r_df, yName, sName)')
        df_r = conn.r('dsld_result')
    else:
        conn.r.assign("maxFeatureSize", maxFeatureSize)
        conn.r('dsld_result <- dsldTakeALookAround(r_df, yName, sName, maxFeatureSize)')
        df_r = conn.r('dsld_result')

    df_py = pd.DataFrame(df_r)

    conn.close()

    return df_py

if __name__ == "__main__":
    args = sys.argv

    file_path = args[1]

    data = pd.read_csv(file_path)

    if len(args) != 5:
        dsldPyTakeALookAround(data, args[2], args[3])
    else:
        dsldPyTakeALookAround(data, args[2], args[3], int(args[4]))
