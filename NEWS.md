# CUSOMShinyHelpers 1.8.2

## New Functionality 
Adding helper functions to enable Gene Set Enrichment Analysis (GSEA)
Updating Volcano Plots to combine same significance groups / shapes into same legend group

***
## Major Changes 
GSEA functions: 

`runfGSEA` -> Run 2-sided fgseaMultilevel analysis (positive and negative) with adjusted pvals

`plotGSEAEnrichment` -> Generate a plotly-based GSEA Enrichment plot 

`getVolcanoPlot` -> Combine same significance groups / shapes into same legend group


***
## Minor Changes
- Version 1.8.2 - Updating Volcano Plots to combine same significance groups / shapes into same legend group


***
## Bug Fixes 
- Version 1.8.1 Fixed bug in function `getLinearModelWithInteraction`: hard-coded `group_by(Analyte)` clause throwing error "Warning: Error in group_by: Must group by variables found in `.data`. ✖ Column `Analyte` is not found." if different nammed column was passed to function.  
