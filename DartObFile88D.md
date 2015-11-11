A common task in storm-scale data assimilation is to produce [Data Assimilation Research Testbed (DART) format](http://www.image.ucar.edu/DAReS/DART/DART_Observations.php#obs_seq_overview) observation files for archived WSR-88D data.  Here is a summary of a way to accomplish this task.

# Obtaining WSR-88D Data from NCDC #

Archived WSR-88D datasets can be obtained from the [Hierarchical Data Storage System (HDSS)](http://has.ncdc.noaa.gov/pls/plhas/HAS.FileAppSelect?datasetname=6500) at the National Climatic Data Center (NCDC).  To obtain a dataset from this website, select the radar name of interest, select the dates of interest, enter your e-mail address, and then click on "Continue With Selections".  The website will then ask you to select specific files (dates and times) of interest.  After choosing the desired files, click on "Retrieve Selected Files".  You will be notified with an e-mail containing ftp instructions when your files are ready to be retrieved.

The retrieved datasets are _.tar_ files for each requested radar and hour.  After the files are separated and uncompressed with the _tar_ and _gunzip_ commands, the result is Archive 2 data, one file per volume.

# Converting WSR-88D Data to Dorade / NetCDF Sweep Files with Foray #

The Archive 2 data obtained in the previous step can be converted to either [DORADE](http://www.ral.ucar.edu/projects/titan/docs/radial_formats/DoradeDoc.pdf) or NetCDF-Foray
sweep files with the [Foray data translator](http://www.eol.ucar.edu/Members/dennisf/foray/translator-usage).  See the [instructions for downloading and building](http://www.eol.ucar.edu/Members/dennisf/foray/getting-and-building-foray) Foray.

# Preprocessing Sweep Files with OPAWS #

objective analysis

limited quality control






# Links #

[Data Assimilation Research Testbed (DART)](http://www.image.ucar.edu/DAReS/DART/)

[Foray](http://www.eol.ucar.edu/Members/dennisf/foray)