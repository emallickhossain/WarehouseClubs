# Gets Nielsen Scanner data from Globus and extracts desired module and store assortment
# In Terminal
#Find file location (change YEAR)
tar ztvf SCANNER_DATA_YEAR.tgz | grep 7260_YEAR.tsv
tar ztvf SCANNER_DATA_YEAR.tgz | grep rms_versions_YEAR.tsv
tar -xvf SCANNER_DATA_YEAR.tgz SCANNER_DATA_YEAR/file/path/here.tsv

# I did this manually by downloading and extracting the following files:
# rms_versions
# stores
# 7260
