# CUSOMShinyHelpers 1.9.2
Minor change to `prettyRadioButtonsFieldSet` and `createTooltip` 

***
## Major Changes 
- No major functionality changes in this release

***
## Minor Changes
- Updated `prettyRadioButtonsFieldSet` to accommodate separate `names` and `values` passed to function - i.e. if you want to use ID field for `values` but show a user friendly `names` value in input widget. Update required adding a simple `dots` param to `createTooltip` function.  

***
## Bug Fixes 
- Minor bug fix to `getPairwiseStatTestByKeyGroup` to handle factor variables appropriately
