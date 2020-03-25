# fitdecodeR
An R wrapper for python-fitdecode to read Garmin .fit data files

The python package fitdecode seems to be the best-maintained and reasonably
current tool for reading the .fit files produced by Garmin devices.  Many 
thanks to the developers/maintainers of that package. 

This package was developed because reticulate is ending support for python 2.X,
so an older package to read .fit files needed updating

All data fields that are single-valued and have an intelligible name are
returned.  If your device reports a single-valued field that you want
which fits that pattern, or it reports a multi-valued parameter that that
throws an error, the code is in readfitffile.py, and you don't need 
to know much Python to figure out how to make it suit your needs.

You have to have a conda (either anaconda or miniconda) setup that R can find.
There are lots of resources to help you do that.  Use your search engine.  This
package requires python >= 3.6.  The environment needs a suitable version of pandas  
and numpy.  

The first call to read a fit file triggers a time-consuming check that the
conda environment has the required packages installed, unless a flag to skip it
is TRUE.

