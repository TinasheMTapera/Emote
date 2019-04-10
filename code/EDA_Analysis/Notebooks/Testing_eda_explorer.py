import os
import shutil
import sys
import pandas as pd
from zipfile import ZipFile
sys.path.append("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/eda-explorer")
from load_files import *
from EDA_Artifact_Detection_Script import *
from EDA_Peak_Detection_Script import *
from AccelerometerFeatureExtractionScript import *

# Loading the eda data

pp200_path = "/Volumes/psychology/Shared/WEACT/EMOTE/Watch Data/200/"
assert os.path.exists(pp200_path)
zips = [x for x in os.listdir(pp200_path) if ".zip" in x]

def UnzipAndLoad(f, path):

    #Unzip a file and load it to memory with load_files

    temp = path + "/temp"
    if not os.path.exists(temp):
        os.mkdir(temp)
    zf = ZipFile(path + f, 'r')
    zf.extractall(temp)
    zf.close()
    data = loadData_E4(temp)
    shutil.rmtree(temp)
    return data

pp200 = pd.DataFrame([])

for x in zips:
    print(x)
    tmpDF = UnzipAndLoad(x, pp200_path)
    pp200 = pp200.append(tmpDF)

pp200.to_csv("p200_out.csv")


#----------------------#
pp200 = pd.read_csv("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/outputs/p200_out.csv",
                    index_col = 'Unnamed: 0')
pp200["datetime"] = pp200.index
episodes = pd.read_csv("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/DataOutputs/UpdatedSurveys_180713.csv")
episodes = episodes[['ID', 'Event', 'EmotionalEatingEpisodes', 'Stress']]
#---------------------#

p200_eps = episodes[episodes.ID == 200]
p200_eps = p200_eps[p200_eps.EmotionalEatingEpisodes  == 1]
p200_eps.Event = pd.to_datetime(p200_eps.Event)
p200_eps.set_index('Event', inplace = True)
p200_eps["Start"] = p200_eps.index + pd.DateOffset(minutes = -30)
p200_eps["End"] = p200_eps.index

features_df = pd.DataFrame(columns = ["Step count", "Mean step time during movement","Percent stillness"])
dfs = dict()
i=6
for i in range(p200_eps.shape[0]):

    mask = (pp200['datetime'] > str(p200_eps.Start[i])) & (pp200['datetime'] < str(p200_eps.End[i]))
    df = pp200.loc[mask]
    if df.shape[0] > 0:
        # Accelerometer Features
        time_frames = [[0, -1]]
        features, steps, motion = computeAllAccelerometerFeatures(df, time_frames)
        df["steps"] = steps
        df["motion"] = motion
        dfs[p200_eps.index[i]] = df
        features_df.loc[p200_eps.index[i]] = features[0]

        # Peak features
        thresh = 0.02
        offset = 1
        start_WT = 4
        end_WT = 4
        SAMPLE_RATE = 8
        returnedPeakData = findPeaks(df,offset*SAMPLE_RATE,start_WT,end_WT, thresh, SAMPLE_RATE)
        df['peaks'] = returnedPeakData[0]
        df['peak_start'] = returnedPeakData[1]
        df['peak_end'] = returnedPeakData[3]
        df['peak_start_times'] = returnedPeakData[2]
        df['peak_end_times'] = returnedPeakData[4]
        df['half_rise'] = returnedPeakData[10]
        # Note: If an SCR doesn't decrease to 50% of amplitude, then the peak_end = min(the next peak's start, 15 seconds after peak)
        df['amp'] = returnedPeakData[5]
        df['max_deriv'] = returnedPeakData[6]
        df['rise_time'] = returnedPeakData[7]
        df['decay_time'] = returnedPeakData[8]
        df['SCR_width'] = returnedPeakData[9]

        dfs[p200_eps.index[i]] = df


features_df.to_csv("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/outputs/200/p200_features.csv",
    index_label = "Event")

os.makedirs("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/outputs/200/")

for key, val in dfs.iteritems():
    name = str(key).replace(" ", "_")+".csv"
    val.to_csv("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/outputs/200/"+name, index_label = "Event")

# EDA Peaks
thresh = 0.02
offset = 1
start_WT = 4
end_WT = 4
SAMPLE_RATE = 8
returnedPeakData = findPeaks(df,offset*SAMPLE_RATE,start_WT,end_WT, thresh, SAMPLE_RATE)



df.loc[df.peaks != 0]
