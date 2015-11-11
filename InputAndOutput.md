# Input Radar Data #

Radar data that are input to OPAWS can be in one of the following two formats:  Dorade sweep files or Foray NetCDF (Foray-1 version).

**Dorade sweep files**

  * Developed by the National Center for Atmospheric Research (NCAR)
  * Used by the NCAR [SOLO editing program](http://www.eol.ucar.edu/rdp/solo/solo_home.html)
  * Designated as the format for VORTEX and VORTEX2 radar data

Detailed information is provided in the [DORADE documentation](http://www.ral.ucar.edu/projects/titan/docs/radial_formats/DoradeDoc.pdf).

**Foray-1 NetCDF**

  * Developed by the National Center for Atmospheric Research (NCAR)
  * Designated as the format for TIMREX radar data
  * Considered a pathway to [Foray2](http://www.eol.ucar.edu/Members/dennisf/foray2)

More information is available on the [Foray web page](http://www.eol.ucar.edu/Members/dennisf/foray).

# Analysis Grid #

Data are analyzed (gridded) in either two dimensions (on the conical surface of each sweep) or three dimensions (on a Cartesian grid).  Three-dimensional Cartesian grids are typical for radar analysis, particular for gridding data in preparation for multiple-Doppler wind synthesis.  Two-dimensional (sweep-by-sweep) analyses can be advantageous for preprocessing radar data for assimilation into a numerical model, as discussed in [Wind and Temperature Retrievals in the 17 May 1981 Arcadia, Oklahoma, Supercell: Ensemble Kalman Filter Experiments, Mon. Wea. Rev., August 2004, p. 1982](http://journals.ametsoc.org/doi/pdf/10.1175/1520-0493%282004%29132%3C1982%3AWATRIT%3E2.0.CO%3B2).

# Output Analysis Data #

Gridded analyses can be output in any of the following formats:

  * [NetCDF](http://www.unidata.ucar.edu/software/netcdf/)
  * [Vis5D](http://www.ssec.wisc.edu/~billh/vis5d.html) format
  * [Data Assimilation Research Testbed (DART) text observation format](http://www.image.ucar.edu/DAReS/DART/DART_Observations.php#obs_seq_overview).

# Namelist Parameters #

Namelist parameters that control how OPAWS processes radar data are described on the "NamelistParameters" wiki.