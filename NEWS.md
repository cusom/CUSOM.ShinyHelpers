# CUSOMShinyHelpers 1.7.1

## New Functionality 
No major new functionality in this release.

***
## Major Changes 
`getVolcanoPlot` - Volcano plot now includes legend output, sorted by expected trace order of "Down", "Not Significant", then "Up". Marker colors match expected values passed via `color` parameter. Minor parameter re-naming. 

`getGroupedStatAnnotations` - Includes significance key annotation. Annotations now include "ns" for insignificant statistical results. Annotations now include formatted statistical value as hovertext. 

`getVolcanoAnnotations` - Naming changes to be more generic (not referencing "p-value" globally). `parameters` output list is not backward-compatible with previous versions. Any references to `parameters` list need to be updated. 

***
## Minor Changes
`addGroupCount` - includes new optional parameter `addLineBreak` to add line breaks between group label and group count 

`addSignificanceGroup` - Outputs more simplified group labels - ex) "up (q < 0.1)". Minor parameter re-naming. 

`formatPValue` - includes optional parameter `formatInsignificantValues` which will output formatted values even if value is not less than significance threshold. 

`getDefaultVolcanoAnnotations` - minor adjustments to top-level and threshold text annotation positions. Minor changes to parameter naming. 

`getSOMStandardFooter` - switched to utilizing `glue` rather than `paste0`. Minor formatting changes.  

`getStatAnnotationAnchorLines` - added `groupIsSignificant` parameter to indicate whether the `significanceVariable` value should be foramtted as "ns" or not. Minor formatting changes.  

added `data.tree-conversion` helper functions to support hierarchical inputs built with the `shinyTree` package. 

***
## Bug Fixes 
None in this release
