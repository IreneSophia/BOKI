#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

This is a preprocessing pipeline for psychophysiological data collected with 
Empatica wristbands, either E4 or E+. It uses EDA Explorer to detect 
artefacts in the data based on EDA, temperature and acceleration data using
a binary classifier (noise versus okay). For the preprocessing, it uses 
functions from the NeuroKit2 package. Specifically for the EDA preprocessing, 
it also draws inspiration from Ledalab. 

Arguments: 
    empatica   : either 'e4' or 'e+' or 'cut' (4Hz EDA and 64Hz BVP only)
    winwidth   : width of the window for smoothing of EDA with Gaussian kernel (int)
    lowpass    : lowpass filter frequency for EDA - has to be no larger than half the sample rate
    dir_out    : output directory for all the results
    dir_path   : input directory
    exclude    : list of patterns to be excluded from preprocessing

The function creates preprocessed data files for EDA and BVP as well as plots 
to check the data quality. 

(c) Irene Sophia Plank, 10planki@gmail.com

"""

# load all modules
import neurokit2 as nk
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import scipy
import simple_colors
import math
import glob
import os
import warnings

#warnings.simplefilter('ignore', UserWarning)
warnings.filterwarnings('ignore')

from avro.datafile import DataFileReader
from avro.io import DatumReader
from datetime import datetime

from EDA_artifactdetection_short import EDA_artifact_detection

###### Helper functions

def gauss_smoothing(data, winwidth):
    # Gaussian smoothing of EDA data
    
    # pad to remove border errors
    fdata = pd.concat([pd.Series(data.iloc[0]), data, pd.Series(data.iloc[-1])])
    
    # ensure an even window width
    winwidth = math.floor(winwidth/2)*2
    
    # extend data to reduce convolution error at beginning and end
    data_ext = pd.concat([pd.Series([fdata.iloc[0]]*int(winwidth/2)), fdata, pd.Series([fdata.iloc[-1]]*int(winwidth/2))])
    
    # apply normpdf (?)
    x = np.array(range(1, winwidth+2))
    mu = winwidth/2+1
    sigma = winwidth/8
    window = np.exp(-0.5 * ((x - mu)/sigma)**2) / (math.sqrt(2*math.pi) * sigma)
    window = window / sum(window)
    
    # perform convolution for smoothing
    sdata_ext = np.convolve(data_ext, window)
    
    # cut to length of data
    return sdata_ext[(1+winwidth):(len(sdata_ext)-winwidth-1)]

def int_missing(df_eda, df_bvp, df_temp, df_acc, f):
    # performing linear interpolation for bvp, temp and acc as well as cubic
    # spline interpolation for eda
    
    # only process temp if it is not empty
    if len(df_temp) > 0:
        df_temp['temp'] = df_temp['temp'].interpolate()
        df_temp['temp'] = df_temp['temp'].bfill()
    
    # only process acc if it is not empty
    if len(df_acc) > 0:
        df_acc['accx'] = df_acc['accx'].interpolate()
        df_acc['accx'] = df_acc['accx'].bfill()
        df_acc['accy'] = df_acc['accy'].interpolate()
        df_acc['accy'] = df_acc['accy'].bfill()
        df_acc['accz'] = df_acc['accz'].interpolate()
        df_acc['accz'] = df_acc['accz'].bfill()
        
    # EDA
    # first, if there is no raw column yet, then save the EDA as raw
    if 'raw' not in df_eda.columns:
        df_eda['raw'] = df_eda['eda']
    # then, check if this data has been interpolated, if not, create a column 
    # called interpolated to track whether a value has been interpolated
    if 'interpolated' not in df_eda.columns:
        df_eda['interpolated'] = df_eda['eda'].isna()
    else:
        # if it already exists, just add it together to track interpolated values
        df_eda['interpolated'] = df_eda['interpolated'] + df_eda['eda'].isna()
    per_eda = np.mean(df_eda['eda'].isna())
    df_eda['eda'] = df_eda['eda'].interpolate(method='spline', order=3)
    df_eda['eda'] = df_eda['eda'].bfill()
    
    # BVP
    # first, if there is no raw column yet, then save the BVP as raw
    if 'raw' not in df_bvp.columns:
        df_bvp['raw'] = df_bvp['bvp']
    # then, check if this data has been interpolated, if not, create a column 
    # called interpolated to track whether a value has been interpolated
    if 'interpolated' not in df_bvp.columns:
        df_bvp['interpolated'] = df_bvp['bvp'].isna()
    else:
        # if it already exists, just add it together to track interpolated values
        df_bvp['interpolated'] = df_bvp['interpolated'] + df_bvp['bvp'].isna()
    per_bvp = np.mean(df_bvp['bvp'].isna())
    df_bvp['bvp'] = df_bvp['bvp'].interpolate()
    df_bvp['bvp'] = df_bvp['bvp'].bfill()
    
    # print how much was interpolated for bvp and eda
    if round(per_eda, 2) == round(per_bvp, 2):
        if per_eda >= 0.2:
            print(simple_colors.red(datetime.now().strftime("%H:%M:%S") + ' - ' + str(round(per_eda*100,2)) + ' percent of data were interpolated', 'bold'))
        elif per_eda >= 0.01:
            print(simple_colors.yellow(datetime.now().strftime("%H:%M:%S") + ' - ' + str(round(per_eda*100,2)) + ' percent of data were interpolated', 'bold'))
    else:
        if per_eda >= 0.2:
            print(simple_colors.red(datetime.now().strftime("%H:%M:%S") + ' - ' + str(round(per_eda*100,2)) + ' percent EDA were interpolated', 'bold'))
        elif per_eda >= 0.01:
            print(simple_colors.yellow(datetime.now().strftime("%H:%M:%S") + ' - ' + str(round(per_eda*100,2)) + ' percent EDA were interpolated', 'bold'))
        if per_bvp >= 0.2:
            print(simple_colors.red(datetime.now().strftime("%H:%M:%S") + ' - ' + str(round(per_bvp*100,2)) + ' percent BVP were interpolated', 'bold'))
        elif per_bvp >= 0.01:
            print(simple_colors.yellow(datetime.now().strftime("%H:%M:%S") + ' - ' + str(round(per_bvp*100,2)) + ' percent BVP were interpolated', 'bold'))
    
    # write to log file
    f.write('\n' + datetime.now().strftime("%H:%M:%S") + ' - ' + str(round(per_bvp*100,2)) + '% BVP and ' + str(round(per_eda*100,2)) + '% EDA were interpolated')
    
    # return the dataframes
    return df_eda, df_bvp, df_temp, df_acc

def na_missing(df_eda, df_bvp, labels):
    # taking the labels from the artefact detection classifier and create column
    # with NaNs where artefacts were detected
    
    # join label dataframes with the dataframes containing EDA and BVP
    labels.index = labels['StartTime']
    df_eda = df_eda.join(labels, how='outer')
    df_bvp = df_bvp.join(labels, how='outer')
    
    # fill up the NaNs with the preceeding label
    df_eda['Binary'] = df_eda['Binary'].fillna(method='ffill')
    df_bvp['Binary'] = df_bvp['Binary'].fillna(method='ffill')
    
    # calculate the new column and drop the unnecessary ones
    df_eda['eda'] = np.where(df_eda['Binary'] == 1, df_eda['eda'], np.nan)
    df_bvp['bvp'] = np.where(df_bvp['Binary'] == 1, df_bvp['bvp'], np.nan)
    df_eda = df_eda.drop(columns=['StartTime', 'EndTime'])
    df_bvp = df_bvp.drop(columns=['StartTime', 'EndTime'])
    
    return df_eda, df_bvp

###### Cut and convert the data

def read_avro(filepath):
    # reading in avro data and converting it into data frames with the correct time stamp
    
    # read in the avro data
    reader = DataFileReader(open(filepath, "rb"), DatumReader())
    for user in reader:
        dict_data = user
    reader.close()
    
    # temperature: 
    startTime = datetime.fromtimestamp((float(dict_data['rawData']['temperature']['timestampStart'])/(10**(len(str(dict_data['rawData']['temperature']['timestampStart']))-10)))) #1000000
    sampRate  = dict_data['rawData']['temperature']['samplingFrequency']
    df_temp   = pd.DataFrame(dict_data['rawData']['temperature']['values'], columns=["temp"])
    if sampRate > 0.0:
        freq      = str(round(1/sampRate)) + 'S'
        time      = pd.date_range(startTime, periods=len(df_temp), freq=freq)
        df_temp.index = time
        df_temp['sampRate'] = round(1/sampRate)
    
    # acceleration
    startTime = datetime.fromtimestamp((float(dict_data['rawData']['accelerometer']['timestampStart'])/(10**(len(str(dict_data['rawData']['accelerometer']['timestampStart']))-10))))
    sampRate  = dict_data['rawData']['accelerometer']['samplingFrequency']
    df_acc    = pd.DataFrame({'accx_raw': dict_data['rawData']['accelerometer']['x'],
                              'accy_raw': dict_data['rawData']['accelerometer']['y'],
                              'accz_raw': dict_data['rawData']['accelerometer']['z']})
    if sampRate > 0.0:
        freq      = str(round(1/sampRate, 6)) + 'S'
        time      = pd.date_range(startTime, periods=len(df_acc), freq=freq)
        df_acc.index = time
        df_acc['sampRate'] = round(1/sampRate, 6)
    
    # bvp
    startTime = datetime.fromtimestamp((float(dict_data['rawData']['bvp']['timestampStart'])/(10**(len(str(dict_data['rawData']['bvp']['timestampStart']))-10))))
    sampRate  = dict_data['rawData']['bvp']['samplingFrequency']
    df_bvp    = pd.DataFrame({'bvp': dict_data['rawData']['bvp']['values']})
    if sampRate > 0.0:
        freq      = str(round(1/sampRate, 6)) + 'S'
        time      = pd.date_range(startTime, periods=len(df_bvp), freq=freq)
        df_bvp.index = time
        df_bvp['sampRate'] = round(1/sampRate, 6)
        
    # eda
    startTime = datetime.fromtimestamp((float(dict_data['rawData']['eda']['timestampStart'])/(10**(len(str(dict_data['rawData']['eda']['timestampStart']))-10))))
    sampRate  = dict_data['rawData']['eda']['samplingFrequency']
    df_eda    = pd.DataFrame({'eda': dict_data['rawData']['eda']['values']})
    if sampRate > 0.0:
        freq      = str(round(1/sampRate, 2)) + 'S'
        time      = pd.date_range(startTime, periods=len(df_eda), freq=freq)
        df_eda.index = time
        df_eda['sampRate'] = round(1/sampRate, 2)
    
    # return all data frames
    return df_temp, df_acc, df_bvp, df_eda

def convert_eplus(dir_path, part, f):
    # reading in and converting data collected with Embrace Plus
    
    # find the path to the participant
    sub_path = dir_path
    for i in range(6):
        # get list of all files of this participant
        fls = glob.glob(os.path.join(sub_path, part, 'raw_data', 'v*', '*.avro'))
        # check if this level contained the data
        if len(fls) > 0:
            break
        else: 
            sub_path = sub_path + '/*'
    
    # check if any data was found
    if len(fls) < 1:
        print(simple_colors.red(datetime.now().strftime("%H:%M:%S") + '- no data was found for participant ' + part, 'bold'))
        f.write('\n' + datetime.now().strftime("%H:%M:%S") + '- no data was found for participant ' + part)
        return {}
    
    # sort by start time in unix
    fls = sorted(fls, key=lambda i: int(os.path.splitext(os.path.basename(i))[0][-9:]))
    
    # initialise empty data frames
    ls_temp = list()
    ls_acc  = list()
    ls_eda  = list()
    ls_bvp  = list()
    
    # loop through files
    for fl in fls:
        # read in the avro data
        df_temp, df_acc, df_bvp, df_eda = read_avro(fl)
        # check timing difference: 
        if len(ls_temp) > 0 & len(df_temp) > 0: 
            # temperature: 1Hz -> should be about 1 per second
            idx_temp = pd.date_range(ls_temp[-1].index[-1]+pd.Timedelta(seconds=df_temp['sampRate'].iloc[0]), 
                                     df_temp.index[0]-pd.Timedelta(seconds=df_temp['sampRate'].iloc[0]), freq=str(df_temp['sampRate'].iloc[0])+'S')
            tdf_temp = pd.DataFrame(np.nan, index=idx_temp, columns=['temp'])
            df_temp  = pd.concat([tdf_temp, df_temp])
        
        if len(ls_acc) > 0 & len(df_acc) > 0: 
            # acceleration: 64Hz -> should be about 1 every 15.625ms
            idx_acc  = pd.date_range(ls_acc[-1].index[-1]+pd.Timedelta(seconds=df_acc['sampRate'].iloc[0]), 
                                     df_acc.index[0]-pd.Timedelta(seconds=df_acc['sampRate'].iloc[0]), freq=str(df_acc['sampRate'].iloc[0])+'S')
            tdf_acc  = pd.DataFrame(np.nan, index=idx_acc, columns=['accx', 'accy', 'accz'])
            df_acc   = pd.concat([tdf_acc, df_acc])
        
        if len(ls_eda) > 0 & len(df_eda) > 0:     
            # eda: 4Hz -> should be about 1 every 250ms
            idx_eda  = pd.date_range(ls_eda[-1].index[-1]+pd.Timedelta(seconds=df_eda['sampRate'].iloc[0]), 
                                     df_eda.index[0]-pd.Timedelta(seconds=df_eda['sampRate'].iloc[0]), freq=str(df_eda['sampRate'].iloc[0])+'S')
            tdf_eda  = pd.DataFrame(np.nan, index=idx_eda, columns=['eda'])
            df_eda   = pd.concat([tdf_eda, df_eda])
        
        if len(ls_bvp) > 0 & len(df_bvp) > 0:     
            # bvp: 1Hz -> 64Hz -> should be about 1 every 15.625ms
            idx_bvp = pd.date_range(ls_bvp[-1].index[-1]+pd.Timedelta(seconds=df_bvp['sampRate'].iloc[0]), 
                                     df_bvp.index[0]-pd.Timedelta(seconds=df_bvp['sampRate'].iloc[0]), freq=str(df_bvp['sampRate'].iloc[0])+'S')
            tdf_bvp  = pd.DataFrame(np.nan, index=idx_bvp, columns=['bvp'])
            df_bvp   = pd.concat([tdf_bvp, df_bvp])
            
        # append to lists
        if len(df_bvp) > 0:
            ls_bvp.append(df_bvp)
        if len(df_temp) > 0:
            ls_temp.append(df_temp)
        if len(df_acc) > 0:
            # scale the accelometer to +-2g: "each ADC count will be = 1/2048g" (email from 12.12.2023)
            df_acc["accx"] = df_acc["accx_raw"]/2048
            df_acc["accy"] = df_acc["accy_raw"]/2048
            df_acc["accz"] = df_acc["accz_raw"]/2048
            
            ls_acc.append(df_acc)
        if len(df_eda) > 0:
            ls_eda.append(df_eda)
    
    # concat list of dataframes to dataframes in dictionary
    return {
        'temp' : pd.concat(ls_temp),
        'acc'  : pd.concat(ls_acc),
        'bvp'  : pd.concat(ls_bvp),
        'eda'  : pd.concat(ls_eda)
    }

def convert_e4(part_path, part, f):
    # reading in all e4 data and adding a time index
    
    # check if all the data was found
    fl_temp = glob.glob(os.path.join(part_path, 'TEMP.csv'))
    fl_acc  = glob.glob(os.path.join(part_path, 'ACC.csv'))
    fl_eda  = glob.glob(os.path.join(part_path, 'EDA.csv'))
    fl_bvp  = glob.glob(os.path.join(part_path, 'BVP.csv'))
    if len(fl_temp) + len(fl_acc) + len(fl_eda) + len(fl_bvp) != 4:
        print(simple_colors.red(datetime.now().strftime("%H:%M:%S") + '- some or all data was missing for participant ' + part, 'bold'))
        f.write('\n' + datetime.now().strftime("%H:%M:%S") + '- some or all data was missing for participant ' + part)
        return {}
    
    # temperature
    df_temp   = pd.read_csv(os.path.join(part_path, 'TEMP.csv'), names=['temp'], header=None)
    startTime = pd.to_datetime(float(df_temp.iloc[0,0]), unit="s")
    sampRate  = float(df_temp.iloc[1,0])
    freq      = str(round(1/sampRate, 3)) + 'S'
    df_temp   = df_temp[df_temp.index>1]
    time      = pd.date_range(startTime, periods=len(df_temp), freq=freq)
    df_temp.index = time
    df_temp['sampRate'] = round(1/sampRate, 3)
    
    # acceleration
    df_acc    = pd.read_csv(os.path.join(part_path, 'ACC.csv'), names=['accx_raw', 'accy_raw', 'accz_raw'], header=None)
    startTime = pd.to_datetime(float(df_acc.iloc[0,0]), unit="s")
    sampRate  = float(df_acc.iloc[1,0])
    freq      = str(round(1/sampRate, 6)) + 'S'
    df_acc    = df_acc[df_acc.index>1]
    time      = pd.date_range(startTime, periods=len(df_acc), freq=freq)
    df_acc.index = time
    df_acc['sampRate'] = round(1/sampRate, 6)
    # scale the accelometer to +-2g: "a value of x = 64 is in practice 1g"
    df_acc["accx"] = df_acc["accx_raw"]/64
    df_acc["accy"] = df_acc["accy_raw"]/64
    df_acc["accz"] = df_acc["accz_raw"]/64
    
    # eda
    df_eda    = pd.read_csv(os.path.join(part_path, 'EDA.csv'), names=['eda'], header=None)
    startTime = pd.to_datetime(float(df_eda.iloc[0,0]), unit="s")
    sampRate  = float(df_eda.iloc[1,0])
    freq      = str(round(1/sampRate, 3)) + 'S'
    df_eda    = df_eda[df_eda.index>1]
    time      = pd.date_range(startTime, periods=len(df_eda), freq=freq)
    df_eda.index = time
    df_eda['sampRate'] = round(1/sampRate, 3)
    
    # bvp
    df_bvp    = pd.read_csv(os.path.join(part_path, 'BVP.csv'), names=['bvp'], header=None)
    startTime = pd.to_datetime(float(df_bvp.iloc[0,0]), unit="s")
    sampRate  = float(df_bvp.iloc[1,0])
    freq      = str(round(1/sampRate, 6)) + 'S'
    df_bvp    = df_bvp[df_bvp.index>1]
    time      = pd.date_range(startTime, periods=len(df_bvp), freq=freq)
    df_bvp.index = time
    df_bvp['sampRate'] = round(1/sampRate, 6)
    
    # put all data frames into one dictionary and return it
    return {       
        'temp' : df_temp,
        'acc'  : df_acc,
        'bvp'  : df_bvp,
        'eda'  : df_eda
    }

def convert_cut(dir_path, part, f):
    # reading in cut EDA and BVP data and adding a time index
    
    # check if all the data was found
    fl_eda  = glob.glob(os.path.join(dir_path, 'EDA_' + part + '.csv'))
    fl_bvp  = glob.glob(os.path.join(dir_path, 'BVP_' + part + '.csv'))
    if len(fl_eda) + len(fl_bvp) != 2:
        print(simple_colors.red(datetime.now().strftime("%H:%M:%S") + '- some or all data was missing for participant ' + part, 'bold'))
        f.write('\n' + datetime.now().strftime("%H:%M:%S") + '- some or all data was missing for participant ' + part)
        return {}
    
    # define the dateparse
    dateparse = lambda x: datetime.strptime(x, '%Y-%m-%d %H:%M:%S.%f')
    
    # eda
    df_eda    = pd.read_csv(fl_eda[0], names=['time', 'eda'], header=None, 
                            delimiter=";", index_col=0, parse_dates=['time'], date_parser=dateparse)
    sampRate  = 4
    df_eda['sampRate'] = round(1/sampRate, 3)
    df_eda['raw'] = df_eda['eda']
    
    # bvp
    df_bvp    = pd.read_csv(fl_bvp[0], names=['time', 'bvp'], header=None, 
                            delimiter=";", index_col=0, parse_dates=['time'], date_parser=dateparse)
    sampRate  = 64
    df_bvp['sampRate'] = round(1/sampRate, 6)
    df_bvp['raw'] = df_bvp['bvp']
    
    # put all data frames into one dictionary and return it
    return {       
        'temp' : [],
        'acc'  : [],
        'bvp'  : df_bvp,
        'eda'  : df_eda
    }

def cut_data(dict_data, tags, dir_out, f):
    # cutting the data into blocks of interest based on the tag files
    
    # if output directory does not exist, create it
    if not os.path.exists(dir_out): os.makedirs(dir_out) 
    
    # create empty dictionary
    dict_df_new = {}
        
    # loop throught the rows of tags
    for index, row in tags.iterrows():
        
        # check if anything is supposed to be cut
        if row['start_unit'] != 'all':
        
            # check if end and start values are provided
            if np.isnan(row['end']) | np.isnan(row['start']):
                print(simple_colors.red('Skipping block ' + row['tag'] + '.', 'bold'), 'Either start or end value is missing.')
                f.write('\n' + 'Skipping block ' + row['tag'] + '. Either start or end value is missing.')
                continue
            
            # determine the start point in seconds from the beginning of the recording
            if row['start_unit'] == 'unix':
                # rounded unix has to have 10 digits for it to be feasible (roughly between 2001 and 2286)
                # however, sometimes saved in different unit, therefore, we adjust it
                adjust = len(str(round(row['start']))) - 10
                start   = datetime.utcfromtimestamp(float(row['start'])/(10**adjust))
            elif row['start_unit'] == 'seconds':
                start   = dict_data['bvp'].index[0] + pd.Timedelta(seconds=row['start'])
            else:
                print(simple_colors.red('Skipping block ' + row['tag'] + '.', 'bold'), 'Start of each tag has to be either seconds or unix.')
                f.write('\n' + 'Skipping block ' + row['tag'] + '. Start of each tag has to be either seconds or unix.')
                continue
            
            # add buffer if necessary
            if row['start_buffer'] > 0:
                start = start + pd.Timedelta(seconds=row['start_buffer'])
            
            # cut out start 
            if len(dict_data['temp']) > 0:
                df_temp = dict_data['temp'][dict_data['temp'].index >= start]
            else:
                df_temp = []
            if len(dict_data['acc']) > 0:
                df_acc  = dict_data['acc'][dict_data['acc'].index   >= start]
            else:
                df_acc = []
            df_bvp  = dict_data['bvp'][dict_data['bvp'].index   >= start]
            df_eda  = dict_data['eda'][dict_data['eda'].index   >= start]
            
            # figure out duration of block if not in seconds based on bvp
            if row['end_unit'] == 'unix':
                # rounded unix has to have 10 digits for it to be feasible (roughly between 2001 and 2286)
                # however, sometimes saved in different unit, therefore, we adjust it
                adjust = len(str(round(row['start']))) - 10
                end   = (datetime.utcfromtimestamp(float(row['end'])/(10**adjust)) - df_bvp.index[0]).total_seconds()
            elif row['end_unit'] == 'duration': 
                end = row['end']
            elif row['end_unit'] == 'seconds':
                x = df_bvp.index[0] - dict_data['bvp'].index[0]
                end = row['end'] - x.total_seconds()
            else:
                print(simple_colors.red('Skipping block ' + row['tag'] + '.', 'bold'), 'End of each tag has to be duration, seconds or unix.')
                f.write('\n' + 'Skipping block ' + row['tag'] + '. End of each tag has to be duration, seconds or unix.')
                continue
            
            # subtract buffer from end and check if enought time left
            end = end - row['end_buffer']
            if end <= 0:
                print(simple_colors.red('Skipping block ' + row['tag'] + '.', 'bold'), 'Duration is 0 seconds or less.')
                f.write('\n' + 'Skipping block ' + row['tag'] + '. Duration is 0 seconds or less.')
                continue
            
            # cut out the end point as well as
            if len(dict_data['temp']) > 0:
                df_temp = df_temp[df_temp.index <
                                        (df_temp.index[0] + pd.Timedelta(seconds=end))]
                df_temp['time'] = df_temp.index
                df_temp.index = pd.timedelta_range(start='0S', periods=len(df_temp), freq=str(df_temp['sampRate'].iloc[0]*1000000)+'U')
            if len(dict_data['acc']) > 0:
                df_acc  = df_acc[df_acc.index <
                                        (df_acc.index[0] + pd.Timedelta(seconds=end))]
                df_acc['time'] = df_acc.index
                df_acc.index  = pd.timedelta_range(start='0S', periods=len(df_acc), freq=str(df_acc['sampRate'].iloc[0]*1000000)+'U')
            df_eda  = df_eda[df_eda.index < 
                                        (df_eda.index[0] + pd.Timedelta(seconds=end))]
            df_eda['time'] = df_eda.index
            df_eda.index  = pd.timedelta_range(start='0S', periods=len(df_eda), freq=str(df_eda['sampRate'].iloc[0]*1000000)+'U')
            df_bvp  = df_bvp[df_bvp.index < 
                                        (df_bvp.index[0] + pd.Timedelta(seconds=end))] 
            df_bvp['time'] = df_bvp.index       
            df_bvp.index  = pd.timedelta_range(start='0S', periods=len(df_bvp), freq=str(df_bvp['sampRate'].iloc[0]*1000000)+'U')
            
            # interpolate missing data and add column with interpolation info
            df_eda, df_bvp, df_temp, df_acc = int_missing(df_eda, df_bvp, df_temp, df_acc, f)
            
            # add cut data frame to the lists
            dict_df_new[row['tag']] = {     
                'temp' : df_temp,
                'acc'  : df_acc,
                'bvp'  : df_bvp,
                'eda'  : df_eda
                }
            
        else:
            # reset the index 
            df_bvp = dict_data['bvp']
            df_bvp['time'] = df_bvp.index
            df_bvp.index  = pd.timedelta_range(start='0S', periods=len(df_bvp), freq=str(df_bvp['sampRate'].iloc[0]*1000000)+'U')
            df_eda = dict_data['eda']
            df_eda['time'] = df_eda.index
            df_eda.index  = pd.timedelta_range(start='0S', periods=len(df_eda), freq=str(df_eda['sampRate'].iloc[0]*1000000)+'U')
            
            # then add all the data under this tag if all is selected
            dict_df_new[row['tag']] = {     
                'temp' : dict_data['temp'],
                'acc'  : dict_data['acc'],
                'bvp'  : df_bvp,
                'eda'  : df_eda
                }
            
    # replace the data frame in the dictionary with the list of data frames
    return dict_df_new

###### EDA

def eda_prepro(dir_out, df_eda, part, key, winwidth, lowpass, f):
    # preprocessing EDA data

    # get sampling rate in Hz
    sr = 1/df_eda['sampRate'].iloc[0]
    
    # if a filter was supposed to be applied
    if isinstance(lowpass, int):
        # check if digital filter critical frequency 0 < lowpass < fs/2
        if (lowpass > 0) & (lowpass < sr/2):
            fil = scipy.signal.butter(1, 5, fs = sr)
            data = scipy.signal.sosfilt(fil, df_eda['eda'])
        else:
            data = df_eda['eda']
            print(simple_colors.red('No filtering applied.', 'bold'), 'Critical frequency must be at least half the sampling rate for smoothing to be performed.')
            f.write('\n' + 'No filtering applied. Critical frequency must be at least half the sampling rate for smoothing to be performed.')
    else:
        data = df_eda['eda']
    
    # smooth the data - same as GAUSS option in ledalab
    df_eda['eda_smooth'] = gauss_smoothing(data, winwidth)
    
    # process the data, including TTG to detect peaks
    signals, info = nk.eda_process(df_eda['eda_smooth'], sampling_rate = sr)
    
    # combine the data frames
    signals.index = df_eda.index
    df_eda = signals.join(df_eda)
    df_eda = df_eda.drop(['EDA_Raw'], axis=1) #
    signals = df_eda.reset_index().rename(columns={"eda": "EDA_Raw"}, errors="raise")
    
    # visualise the signals
    matplotlib.rcParams['figure.figsize'] = (100, 10)
    nk.eda_plot(signals, info)
    plt.savefig(os.path.join(dir_out, part + '_' + key + '_eda_signals.png'), dpi = 300)
    
    # close all figures
    plt.close("all") 
    
    # save data to csv
    df_eda.to_csv(os.path.join(dir_out, part + '_' + key + '_eda_signals.csv'), index = True)
    info.pop('sampling_rate')
    info_df = pd.DataFrame.from_dict(info)
    info_df.to_csv(os.path.join(dir_out, part + '_' + key + '_eda_scr.csv'), index = True)
    
    return 

###### BVP and HR

def bvp_prepro(dir_out, df_bvp, part, key):
    # preprocessing BVP and HR data

    # get sampling rate in Hz
    sr  = 1/df_bvp['sampRate'].iloc[0]
    
    # process the data following Elgendi et al. (2013)
    # settings: 
    #   peakwindow=0.111,
    #   beatwindow=0.667,
    #   beatoffset=0.02,
    #   mindelay=0.3
    #   minimum peak height of 0
    signals, info = nk.ppg_process(df_bvp['bvp'], sampling_rate = sr)
    
    # plot the data
    matplotlib.rcParams['figure.figsize'] = (20, 10)
    nk.ppg_plot(signals, info)
    plt.savefig(os.path.join(dir_out, part + '_' + key + '_bvp_signals.png'), dpi = 300)
    signals_split = np.array_split(signals, 10)
    count = 0
    for s in signals_split:
        count = count + 1
        # graphs can only be created when there are more than three peaks
        if sum(s['PPG_Peaks'] == 1) > 3:
            nk.ppg_plot(s, info)
            plt.savefig(os.path.join(dir_out, part + '_' + key + '_bvp_signals_' + str(count) + '.png'), dpi = 300)
    
    # calculate HRV
    hrv_indices = nk.hrv(signals, sampling_rate = info['sampling_rate'], show = True)
    plt.savefig(os.path.join(dir_out, part + '_' + key + '_bvp_hrv.png'), dpi = 300)
    
    # close all figures
    plt.close("all") 
    
    # save signals and hrv_indices to csv
    signals.index = df_bvp.index
    df_bvp = signals.join(df_bvp)
    df_bvp.drop(['PPG_Raw'], axis=1).to_csv(os.path.join(dir_out, part + '_' + key + '_bvp_signals.csv'), index = True)
    hrv_indices.to_csv(os.path.join(dir_out, part + '_' + key + '_bvp_hrv.csv'), index = True)
    
    return 

###### Run everything

def preproPSYPHY(dir_path, dir_out, tag_file, empatica, exclude = [], winwidth = 8, lowpass = 5, max_art = 100/3, art_cor = True):

    # load the tag file containing participant IDs and block information
    tags = pd.read_csv(tag_file)
    tags['artefact%'] = np.nan
    ls_parts = list(tags['part'].unique())
    
    # start writing a log file
    log_file = tag_file[:-4] + '_log.txt'
    f = open(log_file, "a")

    # remove excluded participants
    for e in exclude:
        for part in ls_parts:
            if e in part: 
                ls_parts.remove(part)

    # loop through the sorted participants
    for part in sorted(ls_parts):
        
        # print a message
        print(simple_colors.blue(datetime.now().strftime("%H:%M:%S") + ' - processing participant ' + part, 'bold'))
        f.write('\n\n' + datetime.now().strftime("%H:%M:%S") + ' - processing participant ' + part)

        # read in and convert the data
        if empatica == 'e+':

            # convert eplus data
            dict_data = convert_eplus(dir_path, part, f)

        elif empatica == 'e4':

            # convert e4 data
            dict_data = convert_e4(os.path.join(dir_path, part), part, f)
            
        elif empatica == 'cut':
            
            # convert the cut data
            dict_data = convert_cut(dir_path, part, f)

        # if no data was found for this participant, continue with the next one
        if len(dict_data) < 1:
            continue

        print(simple_colors.green(datetime.now().strftime("%H:%M:%S") + ' - conversion done', 'bold'))
        f.write('\n' + datetime.now().strftime("%H:%M:%S") + ' - conversion done')

        # cut out the relevant blocks of data and interpolate any missing data
        dict_data = cut_data(dict_data, tags[tags['part'] == part], dir_out, f)
        print(simple_colors.green(datetime.now().strftime("%H:%M:%S") + ' - block separation done', 'bold'))
        f.write('\n' + datetime.now().strftime("%H:%M:%S") + ' - block separation done')

        # loop through the blocks and preprocess the data
        for key, dict_df in dict_data.items():

            # check if artifact detection already exists
            if (os.path.exists(os.path.join(dir_out, part + '_' + key + '_artefacts.csv'))):
                # load it
                labels = pd.read_csv(os.path.join(dir_out, part + '_' + key + '_artefacts.csv'), index_col=0)
                labels['StartTime'] = pd.to_timedelta(labels['StartTime'])
                labels['EndTime']   = pd.to_timedelta(labels['EndTime'])
            else:
                # detect artifacts using the EDA Explorer classifier
                labels  = EDA_artifact_detection(dict_df, dir_out, part, key)
            per_art = sum(labels['Binary'] == -1)*100/len(labels)
            
            # add the percent to the tags object            
            tags['artefact%'].loc[(tags['part'] == part) & (tags['tag'] == key)] = per_art
            
            # only preprocess if less than 20% artefacts
            if per_art < max_art:

                print(simple_colors.green(datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': artifact detection done', 'bold'))
                f.write('\n' + datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': artifact detection done')

                # replacing artefacts with NaNs and then interpolating them
                if art_cor:
                    df_eda, df_bvp = na_missing(dict_df['eda'], dict_df['bvp'], labels)
                    df_eda, df_bvp, [], [] = int_missing(df_eda, df_bvp, [], [], f)
                    print(simple_colors.green(datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': artifact correction done', 'bold'))
                    f.write('\n' + datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': artifact correction done')
                else:
                    df_eda = dict_df['eda']
                    df_bvp = dict_df['bvp']

                # preprocess EDA and BVP data with neurokit
                eda_prepro(dir_out, df_eda, part, key, winwidth, [], f) 
                bvp_prepro(dir_out, df_bvp, part, key)

                # simply save temp and acc, if they exist
                if len(dict_df['temp']) > 0:
                    dict_df['temp'].to_csv(os.path.join(dir_out, part + '_' + key + '_temp.csv'))
                if len(dict_df['acc']) > 0:
                    dict_df['acc'].to_csv(os.path.join(dir_out, part + '_' + key + '_acc.csv'))

                print(simple_colors.green(datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': preprocessing done', 'bold'))
                f.write('\n' + datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': preprocessing done')

            else: 

                print(simple_colors.red(datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': STOPPED due to ' + str(round(per_art,2)) + '% artefacts', 'bold'))
                f.write('\n' + datetime.now().strftime("%H:%M:%S") + ' - block ' + key + ': STOPPED due to ' + str(round(per_art,2)) + '% artefacts')
                
    tags.to_csv(tag_file[:-4] + '_prepro.csv')
    f.close()
