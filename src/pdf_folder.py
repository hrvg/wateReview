# coding: utf-8
# author: Noelle Patterson, for Latin American project
# written: November 2018

import glob
from pathlib import Path
import os
import shutil, os


# Change path to specify folder name, either spanish or portuguese
data_folder = Path("/Users/noellepatterson/apps/Other/Lat_am/spanish/")

os.chdir(data_folder)
files = glob.glob("*.pdf")

# make new folder to put all the separate pdf folders into
os.makedirs('pdf_files')
os.chdir(data_folder/'pdf_files')
data_folder2 = data_folder/'pdf_files'

# copy pdf files and paste them into new folders of same name
for filename in files:
    data_folder3 = data_folder2/filename[:-4]
    os.makedirs(filename[:-4])
    shutil.copy(os.path.join(data_folder,filename), os.path.join(data_folder3,filename))
    
