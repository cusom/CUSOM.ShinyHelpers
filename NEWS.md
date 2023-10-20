# CUSOMShinyHelpers 1.9.4
Bug fix for `downloadFile` module

***
## Major Changes 
- No major functionality changes in this release

***
## Minor Changes
- Updated `prettyRadioButtonsFieldSet` to accommodate separate `names` and `values` passed to function - i.e. if you want to use ID field for `values` but show a user friendly `names` value in input widget. Update required adding a simple `dots` param to `createTooltip` function. 
- Suppressed console output from `geom_smooth()` calls for scatter plots

***
## Bug Fixes 
- Minor bug fix to `getPairwiseStatTestByKeyGroup` to handle factor variables appropriately
- Added `writexl` package dependency for save excel file function in `downloadFile` module 
