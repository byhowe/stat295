# Download all datasets
jq -r '.distribution[] | (.contentUrl + " " + .name)' < urls.json | parallel -j3 --colsep ' ' curl -sL {1} -o {2}';' echo Finished {2}

# Get column number
head -n1 2008.csv | tr ',' '\n' | grep -n Origin
