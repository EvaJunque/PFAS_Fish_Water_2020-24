#!/bin/bash

# The url (https://environment.data.gov.uk/water-quality/view/download/new) gives large files containing additional information, saved with names like 2004.csv
#Postprocessing with

OUTFILE=fluor_combined.csv

rm -f $OUTFILE unknown.$OUTFILE fluor2???.csv

# The -M suffix means build it out of the monitoring csv files only, not compliance or combined
for i in 2???-M.csv; do egrep -i 'fluoro|determinand' $i > "fluor$i"; done
# gives the smaller files checked into this folder. All compounds of current interest have fluor in the name
# and every csv file has the same first line (containing determinand)


# Turns out the different years contain multiple measurements for the same location, e.g. roughly once a month.

# Each file uses the same header / same columns
HEADER=$(head -n1 2024-M.csv)


# Sort by ID at the start of the line is probably not the most useful choice
echo $HEADER > $OUTFILE
cat fluor2???*.csv | grep -v $HEADER | sort >> $OUTFILE

# Discard dry weight measurements (~2k of them, ~250 in the set reduced by location)
mv $OUTFILE $OUTFILE.tmp
egrep -v 'Dry Wt,' $OUTFILE.tmp > $OUTFILE
rm $OUTFILE.tmp

# Checking what the various compounds are (gawk csv hack from https://stackoverflow.com/questions/4205431/parse-a-csv-using-awk-and-ignoring-commas-inside-a-field because some locations have commas in their string name fields)

# Pull out unique determinands (~ list of compounds)
cat $OUTFILE | gawk -vFPAT='[^,]*|"[^"]*"' '{print $6 " - " $7}' | grep -v 'determinand.label determinand.definition' | sort | uniq  > determinands.txt

# Pull out material type (~ a way to distinguish fish from water)
# Commented out for now as edited in the repo
# cat $OUTFILE | gawk -vFPAT='[^,]*|"[^"]*"' '{print $13}' | sort | uniq  > sampled_material_type.txt


# Sadly that's 119mb which will upset github, compress it a bit - the id and sample.samplingPoint have a common
# prefix (the url to get the data from), removing those reduces it to 78mb

sed -i 's_http://environment.data.gov.uk/water-quality/data/measurement/__g' $OUTFILE
sed -i 's_http://environment.data.gov.uk/water-quality/id/sampling-point/__g' $OUTFILE

# Some of the locations do not have associated longitude/latitude at the gov website
# Until a better idea presents itself, removing them from the combined csv file
# and leaving them in the unknown. prefix file

REMOVED=',SO-E0000119,|,TH-PTTG0025,|,TH-PDAR9999,|,TH-PTTR9999,|,TH-PWAE9999,'

echo $HEADER > unknown.$OUTFILE
egrep $REMOVED $OUTFILE >> unknown.$OUTFILE 

egrep -v $REMOVED $OUTFILE > tmp.csv
mv tmp.csv $OUTFILE


# Filtering again based on the version with locations appended instead of $OUTFILE as it's quicker
# Rscript append_locations.R will regenerate fluor_with_locations
Rscript append_locations.R
LOCFILE=fluor_with_locations.csv

# Filter by determinand and keep the column headers

KEEP='$13~/^(CANAL WATER|ESTUARINE WATER|GROUNDWATER|MINEWATER|POND \/ LAKE \/ RESERVOIR WATER|RIVER \/ RUNNING SURFACE WATER|SOIL WATER|SEA WATER|SURFACE DRAINAGE)$/'

# Keep compounds other than those matching this regex. On $6 instead of $7 because the abbreviation
# is easier to write the regex for (shorter, fewer parentheses) and len(uniq($6)) == len(uniq($7))
# Note dropping PFOS, PFOS (B), PFOS (L) and keeping PFOS (T), the total
DROP='$6!~/^(DiClDiFMetha|EtFOSAA-B|EtFOSAA-L|EtFOSE|Fluoroxypyr|HFPO-TA|MeFOSE|MeFOSAA-B|MeFOSAA-L|NFDHA|PFHxDA|PFMOBA|PFMOPrA|PFODA|PFTrDS|PFUnDS|TextVirology|TrCFMethan|TRICLFLMETHN|PFOS|PFOS \(B\)|PFOS \(L\))$/'


gawk -vFPAT='[^,]*|"[^"]*"' "$KEEP" $OUTFILE > tmp
gawk -vFPAT='[^,]*|"[^"]*"' "$DROP" tmp > sampled.$OUTFILE.filt
rm tmp
head -n 1 $OUTFILE > sampled.$OUTFILE
cat sampled.$OUTFILE.filt >> sampled.$OUTFILE
rm sampled.$OUTFILE.filt

gawk -vFPAT='[^,]*|"[^"]*"' "$KEEP" $LOCFILE > tmp
gawk -vFPAT='[^,]*|"[^"]*"' "$DROP" tmp > sampled.$LOCFILE.filt
rm tmp
head -n 1 $LOCFILE > sampled.$LOCFILE
cat sampled.$LOCFILE.filt >> sampled.$LOCFILE
rm sampled.$LOCFILE.filt



# This is fine with or without locations as they're appended, may as well use the sample set
# File has since been edited to mark which compounds are kept/dropped so don't overwrite it
cat sampled.$LOCFILE | gawk -vFPAT='[^,]*|"[^"]*"' '{print $6 " " $7}' | grep -v 'determinand.label determinand.definition' | sort | uniq  > sampled.determinands.txt

