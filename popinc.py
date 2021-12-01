# Projections of water use

import pandas as pd

# --------------------
# Read in base data:
# --------------------

# Load the Population and Income projections from Wear and Prestemon
# for Pam:
# popinc=pd.read_csv(r'E:\\GIS_Backup\\WaterDemand\\DataWaterDemand\\popinc_proj.csv')
# for Travis
popinc = pd.read_csv(r'D:\Demand model\BaseData\popinc_proj.csv')

# Load the water withdrawal data
# for Pam
# wd = pd.read_csv(r'E:\\GIS_Backup\\WaterDemand\\DataWaterDemand\\wd2015.csv')
# for Travis
wd = pd.read_csv(r'D:\Demand model\BaseData\wd2015.csv')

# subset population and income data for year = 2015
pop2015 = popinc.copy()
pop2015 = pop2015[pop2015.year == 2015]

wpu_0 = wd["domestic"] / pop2015["pop"]

# next: need to take wpu_0 and apply formula to calculate wpu for future years. See paper
