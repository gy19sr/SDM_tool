# -*- coding: utf-8 -*-
"""
Created on Tue Apr 28 14:51:56 2020

@author: stuar
"""

"""
original
sp <- gbif("Varecia","rubra",download = F)
"""
import scipy as sc
from rpy2.robjects.packages import importr
import rpy2.robjects as ro
import rpy2.interactive as r

utils = importr('utils')
utils.install_packages('rgbif')