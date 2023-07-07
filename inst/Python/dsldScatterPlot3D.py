#OVERVIEW:  
#   No need to access dsldScatterPlot3D from dsld-package.
#   The function uses the package plotly in R, which is also available in Python.
#   This file requires packages: pandas, plotly, pyreadr
#           

#TODO:
#   Remove hardcoded example (pef) in dsldScatterPlot to accept any dataset  
#   Incorporate more arguments from dsldScatterPlot3D R function to Python's equivalent function
#   Possibly add more datatypes accepted through universalConverterToPdf function
#   Make functions accessible from console
#       I believe this part should be done near the end when dsldScatterPlot is fully/nearly functioning
#   Change name to dsldPlotly?

import pyreadr #convert .rData to panda's data frame
import pandas as pd #Convert .csv to panda's data frame
import plotly.express as px #Create interactive graphs

def dsldScatterPlot(data):
    df = universalConverterToPdf(data)
    fig = px.scatter_3d(df, x='age', y='wageinc', z='wkswrkd') #z,y,x values must be column names of dataframe
    fig.show()
def getFileExt(data):
    return data.split(".",1)[1].lower()
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
    fileExt = getFileExt(data)
    if fileExt == "csv": return csvToPdf(data)
    if fileExt == "rda" or fileExt == "rdata": return rDataToPdf(data)
    else: 
        print("File extension must be .csv or .rData")
        exit(1)

'''
__________________________________________________________________________________________________________________________
#EXAMPLE 1: Use this example to test if plotly worked.

#           Run the next few lines (by uncommenting this example), and it should open interactive graph in browser. 
#           In terminal, I did: python3 dsldScatterPlot3D.py
  
df1 = px.data.iris() #data type of df: pandas.core.frame.DataFrame (i.e: pandas data frame)
fig = px.scatter_3d(df1, x='sepal_length', y='sepal_width', z='petal_width',
              color='species')
fig.show()

___________________________________________________________________________________________________________________________
#EXAMPLE 2: Use either example to test dsldScatterPlot

dsldScatterPlot("C:/Users/Brandon/Documents/dsld/data/pefFixed.csv")
dsldScatterPlot("C:/Users/Brandon/Documents/dsld/data/pef.rData")

___________________________________________________________________________________________________________________________
'''