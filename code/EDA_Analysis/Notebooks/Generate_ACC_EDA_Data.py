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

events = pd.read_csv("/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/DataOutputs/UpdatedSurveys_180713.csv")
events = events[['ID', 'Event', 'EmotionalEatingEpisodes', 'Stress']]

participant_dir = "/Volumes/psychology/Shared/WEACT/EMOTE/Watch Data/"
assert os.path.exists(participant_dir)
participants = [str(participant_dir+x) for x in os.listdir(participant_dir) if os.path.isdir(str(participant_dir+x))]

# loop through our participants

for part_dir in participants:
    print "Now processing " + part_dir
    # for each participant collect their zip files
    zips = [x for x in os.listdir(part_dir) if ".zip" in x]

    # participant's dataframe
    participant_df = pd.DataFrame()

    print "Unzipping files..."
    for x in zips:
        print "Unzipping " + x
        # for each zip file, unzip and load into session and append to their dataframe
        try:
            tmpDF = UnzipAndLoad(x, str(part_dir + "/"))
            participant_df = participant_df.append(tmpDF)
        except:
            print "Error in zip {0}".format(x)
            continue

    print "Files unzipped"
    print "Collating events..."
    participant_df['datetime'] = participant_df.index
    # now that their data is in session, select the episodes and controls
    participant_events = events[events.ID == int(part_dir[-3:])]
    participant_events.Event = pd.to_datetime(participant_events.Event)
    participant_events.set_index('Event', inplace = True)
    participant_events["Start"] = participant_events.index + pd.DateOffset(minutes = -30)
    participant_events["End"] = participant_events.index

    # for each episode, calculate features if data exists
    event_features = pd.DataFrame(columns = ["Step count", "Mean step time during movement","Percent stillness"])
    event_dfs = dict()

    print "Events preprocessed"
    print "Assessing events for watch data..."
    for i in range(participant_events.shape[0]):

        #subset the eda data
        mask = (participant_df['datetime'] > str(participant_events.Start[i])) & (participant_df['datetime'] < str(participant_events.End[i]))
        df = participant_df.loc[mask]

        # if there's any data, calculate features
        if df.shape[0] > 0:
            print "Found usable event data"
            print "Calculating features..."
            # Accelerometer Features
            time_frames = [[0, -1]]
            features, steps, motion = computeAllAccelerometerFeatures(df, time_frames)
            df["steps"] = steps
            df["motion"] = motion
            event_dfs[participant_events.index[i]] = df
            event_features.loc[participant_events.index[i]] = features[0]

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

            event_dfs[participant_events.index[i]] = df

    # finally, write the event features out
    print "Finished calculating features"
    print "Writing out results..."
    output_dir = "/Users/mac/GDriveDocs/Quantitative Psychology & Statistics Lab/Emote/EDA_Analysis/outputs/"
    participantID = part_dir[-3:]
    if not os.path.exists(str(output_dir+participantID)):
        os.makedirs(str(output_dir+participantID))
    csv_name = "/p%s_features.csv" % (participantID)

    event_features.to_csv(str(output_dir+participantID+csv_name),
        index_label = "Event")

    for key, val in event_dfs.iteritems():
        name = str(key).replace(" ", "_")+".csv"
        val.to_csv(str(output_dir+participantID+"/")+name, index_label = "Event")
    print "Completed participant " + part_dir
