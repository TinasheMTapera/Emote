import os
import shutil
import sys
import pandas as pd
from zipfile import ZipFile
sys.path.append("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/eda-explorer")
import load_files
import EDA_Artifact_Detection_Script
import EDA_Peak_Detection_Script
import AccelerometerFeatureExtractionScript

filepath = "/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/Data"
eda = load_files._loadSingleFile_E4(os.path.join(filepath,'EDA.csv'),["EDA"],4,"250L")
