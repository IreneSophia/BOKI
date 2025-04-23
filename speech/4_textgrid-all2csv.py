#!/usr/bin/python
# textgridall2csv.py
# D. Gibbon
# 2016-03-15
# 2016-03-15 (V02, includes filename in CSV records)
# adjusted by I. S. Plank (2022)

#-------------------------------------------------
# Import standard Python modules

import os, re

#-------------------------------------------------
# Text file input / output

def inputtextlines(filename):
    handle = open(filename,'r')
    linelist = handle.readlines()
    handle.close()
    return linelist

def outputtext(filename, text):
    handle = open(filename,'w')
    handle.write(text)
    handle.close()

#-------------------------------------------------
# Conversion routine

def convertgrid2csv(textgridlines,textgridname):

    csvsil = ''
    csvsyl = ''
    for line in textgridlines:
        line = re.sub('\n','',line)
        line = re.sub('^ *','',line)
        linepair = line.split(' = ')
        if len(linepair) == 2:
            if linepair[0] == 'class':
                classname = linepair[1]
            if linepair[0] == 'name':
                tiername = linepair[1]
            if linepair[0] == 'number':
                number = linepair[1]
            if linepair[0] == 'xmin':
                xmin = f'{float(linepair[1]):.3f}'
            if linepair[0] == 'xmax':
                xmax = f'{float(linepair[1]):.3f}'
            if linepair[0] == 'mark':
                mark = linepair[1]
                csvsyl += textgridname + ',' + classname + ',' + tiername + ',' + number + ',' + mark + '\n'
            if linepair[0] == 'text':
                text = linepair[1]
                if (text == '"" '):
                    label = "silent"
                else:
                    label = "sounding"   
                diff = str(float(xmax)-float(xmin))
                csvsil += textgridname + ',' + classname + ',' + tiername + ',' + label + ',' + xmin + ',' + xmax + ',' + diff + '\n'
    return csvsil, csvsyl


#### [!PARAMETERS TO ADJUST!] ####
# directory for input and output
directory = "/media/emba/emba-2/ML_BOKI/AUD_preprocessed/"

#-------------------------------------------------
# Define CSV header for individual files

header_silence = 'Name,TierType,TierName,Label,Start,End,Duration\n'
header_syllable = 'Name,TierType,TierName,Number,Mark\n'

# should the output be saved individually or all together?
sep = 1

#-------------------------------------------------
# Get filenames.
try:
    allfiles = sorted(os.listdir(directory))
except:
    print("Problem with input files.")
    exit()

#-------------------------------------------------
# Check for txt, Intensity and Matrix filenames.
try:
    textgridfiles = [x for x in allfiles if x.endswith('.TextGrid')]
    if textgridfiles == []:
        print("No TextGrid files to process.")
        exit()
except:
    print("File input problem.")
    exit()

#-------------------------------------------------
# Process one TextGrid file at a time.

try:

    allcsv_silence = ''
    allcsv_syllable = ''
    for filename in textgridfiles:

# Conversion of silence and syllable information.
        print('Converting',filename,'to silence and syllable csv')
        try:
            textgrid = inputtextlines(os.path.join(directory,filename))
            csvsil, csvsyl = convertgrid2csv(textgrid,filename[0:-9])
            if csvsil == '':
                print('No silence data in file',filename)
                exit()
            if csvsyl == '':
                print('No syllable data in file',filename)
                exit()
        except:
            print("Problem with TextGrid data.")
            exit()
            
        if sep:
            outputfilename = os.path.splitext(filename)[0]+'_silence.csv'
            outputtext(os.path.join(directory,outputfilename),header_silence + csvsil)
            outputfilename = os.path.splitext(filename)[0]+'_syllable.csv'
            outputtext(os.path.join(directory,outputfilename),header_syllable + csvsyl)
        else:
            allcsv_silence += csvsil
            allcsv_syllable += csvsyl            
    
    if not(sep):
        print('Saving all csvs in one file: all_silence.csv and all_syllable.csv')
        outputtext(os.path.join(directory,'OUT_silence.csv'),header_silence + allcsv_silence)
        outputtext(os.path.join(directory,'OUT_syllable.csv'),header_syllable + allcsv_syllable)

    print("Done.")

except:
    print("Unknown error.")

#--- EOF -----------------------------------------
