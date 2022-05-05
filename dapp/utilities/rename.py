import os
from shutil import move as mv
from os.path import join as osjoin
print('This script will remove spaces and hashtags from filenames.')
whatdir = input('Enter the full folder path containing ONLY the files to be renamed:')
whatdir = osjoin(whatdir, '')
dirs = os.listdir(whatdir)
for file in dirs:
    remove_file = False
    simplename = file.split('.')[0]
    origname = simplename
    exten = file.split('.')[1]
    if '#' in simplename:
        remove_file = True
        simplename = simplename.replace('#', '')
    if ' ' in simplename:
        remove_file = True
        simplename = simplename.replace(' ', '')
    mv(whatdir + origname + '.' + exten, whatdir + simplename + '.' + exten)
