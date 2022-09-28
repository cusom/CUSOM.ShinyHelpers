# CUSOMShinyHelpers 1.8.1

## New Functionality 
Adding helper functions to enable Gene Set Enrichment Analysis (GSEA):

***
## Major Changes 
GSEA functions: 

`runfGSEA` -> Run 2-sided fgseaMultilevel analysis (positive and negative) with adjusted pvals

`plotGSEAEnrichment` - > Generate a plotly-based GSEA Enrichment plot 


***
## Minor Changes
No minor changes in this release

***
## Bug Fixes 
- Version 1.8.1 Fixed bug in function `getLinearModelWithInteraction`: hard-coded `group_by(Analyte)` clause throwing error "Warning: Error in group_by: Must group by variables found in `.data`. ✖ Column `Analyte` is not found." if different nammed column was passed to function.  
