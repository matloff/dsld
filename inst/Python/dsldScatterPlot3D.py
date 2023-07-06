#OVERVIEW:  No need to access dsldScatterPlot3D from dsld-package.
#           The function uses the package plotly in R, which is 
#           also available in Python

#TODO:
#   Remove hardcoded example (pef) in dsldScatterPlot to accept any dataset  
#   Incorporate more arguments from dsldScatterPlot3D R function to Python's equivalent function
#   Possibly add more datatypes accepted through universalConverterToPdf function
#   Change name to dsldPlotly?

#Do:  Install pandas, plotly, pyreadr




import pyreadr #convert .Rdata to panda's dataframe
import pandas as pd #Convert csv to panda's dataframe
import plotly.express as px #Create interactive graphs

#at the moment, assuming data is csv and is using pef dataframe
def dsldScatterPlot(data):
    df = universalConverterToPdf(data)
    fig = px.scatter_3d(df, x='age', y='wageinc', z='wkswrkd') #z,y,x values must be column names of dataframe
    fig.show()
def csvToPdf(data):
    #Converts any .csv file to a panda's dataframe
    pdf = pd.read_csv(data)
    return pdf
def rDataToPdf(data):
    #Converts any .rData file to a panda's dataframe
    dictResult = pyreadr.read_r(data)
    pdfName = list(dictResult.keys())[0]
    pdf = dictResult[pdfName]
    return pdf
def universalConverterToPdf(data):
    fileExt = data[len(data) - 4:] #temporary way to get fileExt
    if fileExt == ".csv": return csvToPdf(data)
    if fileExt == "Data": return rDataToPdf(data)
    else: 
        print("File extension must be .csv or .rData")
        exit(1)
'''
__________________________________________________________________________________________________________________________
#EXAMPLE 1: Use this example to test if plotly worked.

#           Run the next few lines (by uncommenting this example), and it should open interactive graph in browser. 
#           In terminal, I did: python3 dsldScatterPlot3D.py
  
df1 = px.data.iris() #data type of df: pandas.core.frame.DataFrame (i.e: pandas dataframe)
fig = px.scatter_3d(df1, x='sepal_length', y='sepal_width', z='petal_width',
              color='species')
fig.show()

___________________________________________________________________________________________________________________________
#EXAMPLE 2: Use either example to test dsldScatterPlot

dsldScatterPlot("/Users/bdzarate98/Documents/GitHub/dsld/data/pefFixed.csv")
dsldScatterPlot("C:/Users/Brandon/Documents/dsld/data/pef.RData")

___________________________________________________________________________________________________________________________
'''
