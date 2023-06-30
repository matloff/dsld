import sys
import pyRserve
import pandas as pd

port = 6311

conn = pyRserve.connect(host='localhost', port=port)


def convert_to_r_dataframe(pandas_df):
    col_names = pandas_df.columns.tolist()
    columns = []
    r_data_frame_args = col_names[0] + ","

    for i in range(0, len(col_names)):
        columns.append(pandas_df[col_names[i]].values.tolist())

        print(col_names[i], len(columns[i]))

        if i != len(col_names) - 1:
            r_data_frame_args += (col_names[i] + ",")
        else:
            r_data_frame_args += col_names[i]

        conn.r.assign(f'{col_names[i]}', columns[i])

    conn.r(f'r_df <- data.frame({r_data_frame_args})')
    conn.r('col <- r_df$age')
    print(conn.r('r_df'))

    return


def dsldPyTakeALookAround(data, yName, sName, maxFeatureSize=None):
    conn.r('library(dsld)')

    convert_to_r_dataframe(data)

    #conn.r('data(iris)')

    conn.r.assign("yName", yName)
    conn.r.assign("sName", sName)

    if maxFeatureSize is None:
        conn.r('dsld_result <- dsldTakeALookAround(data, yName, sName)')
        df_r = conn.r('dsld_result')
    else:
        conn.r.assign("maxFeatureSize", maxFeatureSize)
        conn.r('dsld_result <- dsldTakeALookAround(data, yName, sName, maxFeatureSize)')
        df_r = conn.r('dsld_result')

    print(df_r)

    df_py = pd.DataFrame(df_r)

    print(df_py)

    conn.close()


if __name__ == "__main__":
    args = sys.argv

    file_path = args[1]

    data = pd.read_csv(file_path)

    if len(args) != 5:
        dsldPyTakeALookAround(data, args[2], args[3])
    else:
        dsldPyTakeALookAround(data, args[2], args[3], int(args[4]))
