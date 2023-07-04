#OVERVIEW:  No need to access dsldScatterPlot3D from dsld-package.
#           The function uses the package plotly in R, which is 
#           also available in Python

#TODO:  Make dsldScatterPlot3D work csv, pandas rdataframe, and actual rdata file
#       Change name to dsldPlotly?
#       Incorporate more arguments from dsldScatterPlot3D R function to Python's equivalent function

#Do:  Install pandas, plotly





import pandas as pd #Convert csv to panda's dataframe
import plotly.express as px #Create interactive graphs


def csvToPandasDF(data):
    #Input location of csv to create panda's dataframe
    return pd.read_csv(data)
def dsldScatterPlot(data):
    #at the moment, assuming data is csv and is using pef dataframe
    df = csvToPandasDF(data)
    fig = px.scatter_3d(df, x='age', y='wageinc', z='wkswrkd') #z,y,x values must be column names of dataframe
    fig.show()


'''
#EXAMPLE 1: Use this example to test if plotly worked.
#           Run the lines (34-37) (by uncommenting this example), and it should open interactive graph in browser. 
#           In terminal, I did: python3 dsldScatterPlot3D.py
  
df1 = px.data.iris() #data type of df: pandas.core.frame.DataFrame (i.e: pandas dataframe)
fig = px.scatter_3d(df1, x='sepal_length', y='sepal_width', z='petal_width',
              color='species')
fig.show()
'''





'''
#EXAMPLE 2: Use this example to test dsldScatterPlot
dsldScatterPlot("/Users/bdzarate98/Documents/GitHub/dsld/data/pefFixed.csv")
'''

