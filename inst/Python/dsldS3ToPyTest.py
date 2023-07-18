import rpy2.robjects as robjects
import pandas as pd


# This function converts python data type into R data type
def convertToR(objType, data):
    if objType == int:
        return robjects.IntVector([data])
    elif objType == str:
        return robjects.StrVector([data])
    elif objType == bool:
        return robjects.BoolVector([data])
    elif objType == float:
        return robjects.FloatVector([data])


# This function converts a python list into an R vector
def convertToRVector(objType, list):
    if objType == int:
        return robjects.IntVector(list)
    elif objType == str:
        return robjects.StrVector(list)
    elif objType == bool:
        return robjects.BoolVector(list)
    elif objType == float:
        return robjects.FloatVector(list)


def py2riObj(object):
    attributes = object.__dict__
    rFieldList = ""
    objArgs = ""

    for key in attributes:
        if type(attributes[key]) == list:
            robjects.r.assign(key, convertToRVector(type(attributes[key][0]), attributes[key]))
        else:
            robjects.r.assign(key, convertToR(type(attributes[key]), attributes[key]))

        rFieldList = rFieldList + f'{key} = "{robjects.r["class"](robjects.r[key])[0]}",'
        objArgs = objArgs + f'{key} = {key},'

    rFieldList = rFieldList[: -1]
    objArgs = objArgs[:-1]


    # The two commented section below returns the object as a data frame
    # robjects.r(f'myDf <- data.frame({objArgs})')
    # return robjects.r['myDf']

    rCode = f'MyObject <- setRefClass("MyObject", fields = list({rFieldList}))'
    robjects.r(rCode)
    robjects.r(f'obj <- MyObject$new({objArgs})')

    return robjects.r['obj']


# For testing in python prompt please enter the following:
"""
python3 # To start python prompt

from py2RObjConversion import py2riObj

class Dog:
    def __init__(self):
        self.name = "Kratos"
        self.color = "black"
        self.age = 5
        self.puppies = [1, 2, 3]
        self.letters = ['k', 'r', 'a', 't', 'o', 's']


dog = Dog()
r_class = py2riObj(dog)
print(r_class)
"""