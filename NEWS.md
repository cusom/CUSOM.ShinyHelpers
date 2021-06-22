# CUSOMShinyHelpers 1.4.2
 Adding new functions to support correlation modules
 
## Major Changes 
- getCorrelationsByKey - function to generate correlation matrix comparing reference analyte to all other analytes. Outputs resulting rho and p.values, adjusted for multiple comparisons
- getDensityColors - function to get density colors for scatter plot
- getScatterPlotWithSmoothing - function to generate ggplot scatter plot with smoothing method (lm, loess, etc)
- Updating getVolcanoPlot to accept optional color and shape parameters - allows for control over colors used in volcano and changing specific shapes used within volcano plot. 
- added helper functions to execute DBI odbc database connections  
 
## Minor Changes
-adding small helper function to parse delimited strings
  
## Bug Fixes 
None in this release
