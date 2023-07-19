import rpy2.robjects as robjects
from rpy2.robjects import StrVector, FloatVector, ListVector
import pandas as pd
import numpy as np

class DsldDiffModel:
    def __init__(self, listVector):
        self.yName = ""
        self.sName = "" 
        self.model = None
        self.newData = None
        self.summary = None
        self.coef = None
        self.data = None

        index = 0

        for key in self.__dict__:
            if isinstance(listVector[index], StrVector):
                self.__dict__[key] = listVector[index][0]
            elif isinstance(listVector[index], FloatVector):
                self.__dict__[key] = list(listVector[index])
            elif isinstance(listVector[index], ListVector):
                self.__dict__[key] = {nested_key: np.asarray(nested_value, dtype='object') for nested_key, nested_value in listVector[index].items()}
            index = index + 1

        for key in self.__dict__:
            if (type(self.__dict__[key])) == dict:
                print(self.__dict__[key])

class DsldLinear:
    def __init__(self, r_object):
        self.dsldModel = {}

        index = 0
        for key in r_object.names:
            diffModelObj = DsldDiffModel(r_object[index])
            self.dsldModel[key] = diffModelObj
            index = index + 1