import json
import os
import re
from os import remove
from os.path import join as osjoin
print('This script will rename your json files sans any spaces or hashtags;\nIt will also replace your asset-name placeholder with the resulting\nfilename (e.g. Extinction008.json = asset-name of: Extinction008;\ndisplay-name of: Extinction #008). Works with single-word NFT names\nonly at this time.\n')
whatdir = input('Enter the full folder path containing ONLY your json files:')
asset = input('Enter asset name placeholder, disclude quotes (e.g. <asset_name>):')
display = input('Enter display name placeholder, disclude quotes (e.g. <display_name>):')
whatdir = osjoin(whatdir, '')
dirs = os.listdir(whatdir)
for file in dirs:
    remove_file = False
    simplename = file.split('.')[0]
    if '#' in simplename:
        remove_file = True
        simplename = simplename.replace('#', '')
    if ' ' in simplename:
        remove_file = True
        simplename = simplename.replace(' ', '')
    temp = re.compile("([a-zA-Z]+)([0-9]+)")
    tuple_name = temp.match(simplename).groups()
    text = tuple_name[0]
    num = tuple_name[1]
    longname = text + ' #' + num
    jsondata = open(whatdir + file, 'r').read().strip()
    jsondata = json.dumps(jsondata)
    jsondata = jsondata.replace(asset, ''.join(simplename))
    jsondata = jsondata.replace(display, ''.join(longname))
    with open(whatdir + simplename + '.json', 'w') as jsonout:
        jsonout.write(json.loads(jsondata))
    if remove_file is True:
        remove(whatdir + file)
