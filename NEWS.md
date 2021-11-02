# CUSOMShinyHelpers 1.6.1

 
## New Functionality 

## Major Changes 
- GetVolcanoPlot now utilizes `selectedpoints`, `selected` and `unselected` properties to call out / highlight row with `selected point` = 1  
- GetLinearModelWithInteraction dynamically determines whether to include interaction term - accomodates both simple lm or lm with interaction term. 
- getScatterPlotByGroup now accomodates single, double and triple traces. When passing `addFitLines` = TRUE, plot now includes 95% CI ribbons. 

## Minor Changes
- AddSignificanceGroup returns color as a derived column based on groups
- createTooltip function eases creating HTML divs with tooltips
- createTutorialTooltip function eases creating tooltips with tutorial launch bound to onclick event 
- createTooltip and createTutorialTooltip functions now support HTML within hover text

## Bug Fixes 
None in this release
