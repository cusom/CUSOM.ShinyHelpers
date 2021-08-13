# CUSOMShinyHelpers 1.5.2
 Adding new functions, updating a few existing functions to support continuous vs continuous variable analysis 
 
## New Functionality 
-- getScatterPlotByGroup - new plot function to display scatter plot colored by group - optionally add linear model fitted value traces per group label. 
-- getLinearModelWithInteraction - new function to return linear model with interaction between independent variable and specificed interaction variable
 
## Major Changes 
-- getLinearModel - logic to format linear model dataframe based on data type of indepdendent variable (factor vs. continuous)
-- formatFoldChangeDataframe - logic to label fold change dataframe correclty based on data type of indepdendent variable (factor vs. continuous)
 
## Minor Changes
-- getStatTestbyKeyGroup - refactoring parameter names to be more descriptive for getLinearModel args 

  
## Bug Fixes 
None in this release
