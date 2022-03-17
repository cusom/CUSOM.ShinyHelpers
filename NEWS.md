# CUSOMShinyHelpers 1.7.2

## New Functionality 
No major new functionality in this release.

***
## Major Changes 
No major changes in this release. 

***
## Minor Changes
- reduced default font size and added new standard legend, title, and axis layouts for the following plotting functions:

    `getBoxPlotWithHighlightGroup`

    `getScatterPlotByGroup`

    `getVolcanoPlot`

- Reduced default font size for the following annotation functions:
  
    `getCorrelationVolcanoAnnotations`

    `getDefaultVolcanoAnnotations`

- Added new logic to handle various annotation labels ("Up" vs "Down", "Increasing" vs "Decreasing", "Greater" vs "Decreased") for the `getDefaultVolcanoAnnotations` function. 


- Minor naming conventions and element positioning changes applied to the `getCorrelationVolcanoAnnotations` function.



***
## Bug Fixes 
- Added missing `text` and `hoverinfo` properties to the `getScatterPlotByGroup` plot function. 
