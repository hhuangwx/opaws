**Observation Processing And Wind Synthesis (OPAWS)**

OPAWS is an open source project which includes software (mostly in Fortran) for objectively analyzing Doppler and dual-polarimetric radar data from meteorological radars.  It is meant to be used to create fields for dual-Doppler analysis of convective storms and/or for thinning radar data for assimilation into atmospheric models using such techniques as the ensemble Kalman filter.

The software is free to anyone with the understanding that while the authors have tried to develop robust and bug-free software, it is provided without warranty and the analysis output generated from running the code could contain errors.  Authors are not full time software developers, and so we likely will not work on feature requests or fix non-lifethreatening bugs found by users.  However, we will be grateful for any bug reports and/or lavish praise for this project.  We welcome submissions of fixed code or code with new features and will attempt to incorporate such code when possible into the distribution.

At present, OPAWS can process NCAR Dorade (sweep) and NCAR EOL Foray (netcdf) radar data.  It analyzes (grids) data in either two-dimensions (on the conical surface of each sweep) or three-dimensions (Cartesian).  Analyses are output in netcdf, Vis5d, and/or DART (Data Assimilation Research Testbed) formats.

Any questions about the Python scripts for processing and plotting should be sent to Louis Wicker.  All other questions should be directed to both David Dowell and Lou Wicker.

The authors would like to acknowledge contributions from:

> Wen-Chau Lee (2001):  dorade sweep-file reader

> Mike Coniglio (2005, 2006, 2012):  velocity unfolding, ground-clutter removal, Mesonet-data input, Lambert conformal map projection

> M. Majcen, P. Markowski, Y. Richardson, J. Wurman (2007):  multi-pass objective analysis

> Glen Romine (2010):  suggestion of ground-clutter mask

> Robin Tanamachi, Dan Dawson (2010):  bug fixes, filtering

> Corey Potvin (2011) implemented full netCDF interface to speedup output I/O


**!! NEW ANALYSIS TOOL Available !!**

Recently Dr. Corey Potvin and collaborators have compared the retrieved vertical velocity fields using a 3DVAR approach versus "traditional" dual-Doppler analyses (using NCAR's Cedric) via an observing systems experiment (OSE).  The results of these experiments strongly suggest the 3DVAR DDA framework is generally preferable to traditional formulations.

The paper can be found at [Potvin et al. (2012)](http://dx.doi.org/10.1175/MWR-D-12-00063.1).  Dr. Potvin and the original author of the method, OU's Dr. Shapiro, have decided to begin sharing the 3DVAR code after summer 2012 to the community.  Interested parties should contact Dr. Potvin at **Corey.Potvin@noaa.gov** to indicate their interest in obtaining this code _(E.g., if you want it, tell him, else he might work on other things)_.  Since support for the code will be minimal, there will be an emphasis to make the code easy to use for researchers.


**Recent Code Changes and Bug Fixes**

**7/2/13**  Recreated the tar ball based on current source files.  _LJW_

**6/28/13**  support for tiled MRMS files (netCDF format) is introduced.  _DCD_

**3/28/13** Important:  default behavior is now changed for super resolution sweep files.  My understanding is that as of 2009, there is a separate super resolution reflectivity file, but that the same data is stored in the regular file (there is no longer any legacy data).  One might want to delete the 0.5, 0.9, and 1.3 files containing the redundant data (need to check using solo).  I have set the default to reading all the sweep files.  For each sweep, the unambiguous range and number of azimuths are printed out.   If super resolution is detected a warning is printed to standard out.  In addition, I believe I have fixed the python plotting scripts for the netCDF output so that one can now more reliably plot the gridded data.     _LJW_

**2/24/12** A Lambert conformal map projection has been added, controlled by the namelist parameters map\_proj, tlat1, tlat2, and clon.  _DCD_

**1/12/12** Added a script (_Scripts/cycle\_oban2.csh_) for running _x.oban_ repeatedly, at regular time intervals, for a dorade sweep radar dataset.  The following output is produced:  NetCDF analysis files, text output, DART obs\_seq.out files, and NCL plots.  _DCD_

**1/11/12** Improved handling of reflectivity and velocity data split between different dorade sweep files for WSR-88D data.  The result is that more data will be retained, and clutter masking (if selected) will be implemented more correctly.  _DCD_

**12/9/11** Added the option of appending 'EVAL' to the names of DART observation types, controlled through the new namelist parameter append\_eval.  This option is useful when multiple DART ob files are to be merged, some containing observations to be assimilated and some containing observations to be evaluated only.  _DCD_

**9/16/11** Added some features which allow one to select or exclude a sector, and uploaded a clean tar source file to the Downloads section.  Contact me for information about the sector stuff.  _LJW_

**08/10/11 and 9/16/11**
A new tool has been added for removing ground clutter from radar data.  The  method is based on the Clutter Residue Editing Map (CREM) by Lakshmanan et al. 2010, which identifies ground clutter at a particular location from the frequency of occurrence of gates with Doppler velocity close to zero and reflectivity above a threshold.  The method for removing ground clutter involves two steps:  (1) running a new program _x.clutter\_stats_ to develop a clutter map from a dataset (typically multiple hours of data) and (2) removing ground clutter when running _x.oban_ by setting relevant parameters in the input namelist.  Currently, ground-clutter mapping and removal has been implemented only for dorade radar data, but it's possible that netcdf radar data could be supported in future code releases.  **Some namelist parameters for oban have changed as a result of these code updates, so users should check their .nml files that are input to oban, to make sure they are consistent with oban.nml.sweep\_all\_parameters and/or oban\_namelist.f90.** _DCD_

**06/27/11** For anyone trying to analyze data either with a single radar variable, or more than two variables, you need to update the file "fileio.f90".  There was a bug in there that will cause netCDF to crash when trying to write the fieldnames.  You can either download the code again from the svn, or you can simply update the file by typing:  "svn update fileio.f90" _LJW_


**References:**

[3DVAR vs. Traditional dual-Doppler Wind Retrievals of a Simulated Supercell Thunderstorm, Potvin et al., Mon. Wea. Rev., accepted 25 May 2012](http://dx.doi.org/10.1175/MWR-D-12-00063.1)

[Multipass Objective Analyses of Doppler Radar Data, Majcen et al., Mon. Wea. Rev., Oct. 2008, p. 1845.](http://journals.ametsoc.org/doi/pdf/10.1175/2008JTECHA1089.1)

[Wind and Temperature Retrievals in the 17 May 1981 Arcadia, Oklahoma, Supercell: Ensemble Kalman Filter Experiments, Dowell et al., Mon. Wea. Rev., Aug. 2004, p. 1982](http://journals.ametsoc.org/doi/pdf/10.1175/1520-0493%282004%29132%3C1982%3AWATRIT%3E2.0.CO%3B2)

[The Simple Rectification to Cartesian Space of Folded Radial Velocities from Doppler Radar Sampling, Miller et al., Mon. Wea. Rev., Mar. 1986, p. 162](http://journals.ametsoc.org/doi/pdf/10.1175/1520-0426%281986%29003%3C0162%3ATSRTCS%3E2.0.CO%3B2)

[A Statistical Approach to Mitigating Persistent Clutter in Radar Reflectivity Data, Lakshmanan et al., 2010](http://www.cimms.ou.edu/~lakshman/Papers/cluttermap.pdf)

**Additional Links:**

[Data Assimilation Research Testbed (DART)](http://www.image.ucar.edu/DAReS/DART/)

[DART observation format](http://www.image.ucar.edu/DAReS/DART/DART_Observations.php#obs_seq_overview)

[DORADE radar-data format](http://www.ral.ucar.edu/projects/titan/docs/radial_formats/DoradeDoc.pdf)

[Foray](http://www.eol.ucar.edu/Members/dennisf/foray)