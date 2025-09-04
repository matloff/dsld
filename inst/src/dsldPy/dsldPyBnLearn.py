from rpy2 import robjects
from rpy2.robjects.packages import importr
from IPython.display import Image, display
from .Utils import get_dsld

def dsldPyIamb(data, file="iamb.png", width=1200, height=900, res=150):
    dsld = get_dsld()
    a = dsld.dsldIamb(data)
    grdevices = importr("grDevices")        
    grdevices.png(file=file, width=width, height=height, res=res)
    robjects.r["plot"](a)                   
    grdevices.dev_off()                     
    display(Image(filename=file))           
    return file
