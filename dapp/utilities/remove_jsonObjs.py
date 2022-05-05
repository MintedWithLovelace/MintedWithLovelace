import json
import os
from os.path import join as osjoin
print('This script will remove "files" sections and "mediaType" entries.\n')
whatdir = input('Enter the full folder path containing ONLY your json files:')
whatdir = osjoin(whatdir, '')
dirs = os.listdir(whatdir)
for file in dirs:
    jsondata = open(whatdir + file, 'r').read().strip()
    jsondata = json.loads(jsondata)
    alterjson = dict(jsondata)
    for topkey in jsondata['721']['POLICY_ID'].keys():
        for key in [key for key in jsondata['721']['POLICY_ID'][topkey] if key == 'files' or key == 'mediaType']: del jsondata['721']['POLICY_ID'][topkey][key]
    with open(whatdir + file, 'w') as jsonout:
        jsonout.write(json.dumps(alterjson, indent=4))
print('Complete! Check files to confirm it worked.')
