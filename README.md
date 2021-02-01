# CUSOM.ShinyHelpers
Common Shiny Helper Functions

## Overview 
This package contains standard helper functions used throughout shiny applications developed by the CU School of Medicine. 

Generally, the functions fall into 4 core areas: 

### 1) Application / Object Creation 
| Function Name    | Description   | 
| :------------- |:-------------| 
| createApplicationLinks | Utility function to create application link buttons
| createInputControl |Utility function to create standard Shiny input widgets
| createMenuItem |Creates Shiny Dashboard Menu Item / Tab
| createServerObject |Utility function to import shiny modules to server
| createSidebarInputs |Utility function to create Shiny UI inputs within the sidebar panel for a sidebar name
| createTabOutput |Utility function to create Tab Items
| moduleServer |utility function to call modules by namespace and module name

--- 
---

### 2) Graphical Output 
| Function Name    | Description   | 
| :------------- |:-------------| 
| getBoxPlot |Plotly function to generate standard box and whisker plot
| getBoxplotForEmptyData |Utility Plotly function to return blank plot when useful
| getBoxPlotWithHighlightGroup |Plotly function to generate standard box and whisker plot with highighted groups
| getBoxBoxwithSelectedRecords |Plotly function to generate standard box and whisker plot with highighted / selected points
| getDefaultVolcanoAnnotations |volcano plot Plotly function to get standard volcano annotations
| getDefaultVolcanoLine |volcano plot Plotly function to get standard volcano line dividing significance cutoff
| getGroupedBoxplot |Plotly function to generate multiple box and whisker plots comparing group across secondary grouping level
| getGroupedStatAnnotations |function to generate stat annotations across multiple groups
| getSelectedRecordsAgePlot |get Plotly box and whisker plot showing age distribution by group
| getSelectedRecordsSexPlot |get Plotly stacked bar chart showing distribution by sex for group variable.
| getSideBySideGroupedBoxPlot |Plotly function to generate side-by-side box and whisker plots comparing groups by highlighted group
| getStatAnnotationAnchorLines |function to generate list of line anchors for stat annotations
| getVolcanoAnnotations |gets all possible volcano plot annotations in a single call. Combines list output from default and arrow annotations.
| getVolcanoArrowAnnotation |volcano plot Plotly function to add volcano plot arrow annotation to specfic point

--- 
---

### Calculation / Data Manipulation  
| Function Name    | Description   | 
| :------------- |:-------------| 
| addGroupCount | adds HTML formatted count to group variable within dataframe
| addSignificanceGroup | Function to add 3 group significance group labels to fold change dataframe
| applyGroupCountThreshold |Utility function to check for minimum group membership in a dataframe
| calculateFoldChangeByKeyGroup |Calculate fold change for each key value between groups
| fillMissingSexObservations |Utility function to fill in missing missing conbinations between sex and another group variable
| getGroupedStatTestByKeyGroup |function to run stat tests for multiple groups
| getMaxAbsValue |gets maximum abs value from dataframe column
| getStatTestByKeyGroup |Calculate statistical test between 2 groups for each key value in a dataframe
| summarizeByGroup |Calculate standard summaries (mean,median,n) across groups for a dataframe

--- 
---

### Standardized Formatting 
| Function Name    | Description   | 
| :------------- |:-------------| 
| formatFoldChangeDataframe | Apply standard formatting to fold change dataframe
| formatPValue | formats p value based on threshold
| getSOMStandardFooter | Utility function to get standard School of Medicine Footer HTML


---
---
---

## Dependencies 
``` DESCRIPTION
Package: CUSOMShinyHelpers
Type: Package
Title: CU School of Medicine Helper Functions for Shiny Applications
Version: 0.1.0
Authors@R: person("Kyle", "Bartsch", email = "kyle.bartsch@cuanschutz.edu",
  role = c("aut", "cre"))
Description: Consists of standard helper functions within shiny application delevelopment. 
Encoding: UTF-8
LazyData: true
RoxygenNote: 7.1.1
Imports: 
    dplyr,
    tidyr (>= 1.1.0),
    purrr (>= 0.2.5),
    shiny (>= 1.5.0),
    shinyWidgets, 
    stringr (>= 1.4.0),
    plotly 

```