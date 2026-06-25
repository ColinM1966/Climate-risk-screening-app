# Week 2 - Day 4

## Main objective

Add a simple comparison graph to help users interpret baseline and future results.

## Starting checks

| Check | Result |
| ----- | ------ |
| Local copy up to date with GitHub | Done |
| App starts from clean session | Done |
| Day 3 result CSV download still works | Done |
| Day 3 comparison CSV download still works | Done |
| Day 3 cropped raster download still works | Done |

## Graph feature added

Describe the graph added to the Results tab.

## Graph tests

| Variable | Graph appears | Units correct | Matches table | Screenshot |
| -------- | ------------- | ------------- | ------------- | ---------- |
| Fire | Done | Done | Done | Done |
| Bio05 | Done | Done | Done | Done |
| PPETmin | Done | Done | Done | Done |

## Layout check

| Check | Result |
| ----- | ------ |
| Results tab readable | Done |
| Graph section clear | Done |
| Downloads still easy to find | Done |

## Issues found

1. The script still the old ggplot2 comparison graph block. The uploaded script still included library(ggplot2) and used ggplot(...) + geom_col() + coord_flip() for the comparison graph. This was not the simple base R version you wanted to test for Day 4.
2. The graph display was still unstable. The error message shows the plot was failing inside output$comparison_plot, es pecially especially when using base R barplot(). The specific error was Error in plot.new: figure margins too large. This means the plot area was too small for the margins, labels, or graph layout.
3. The graph output area needed fixed sizing. The graph should use a wider/fixed plot area, for example: width = "700px" height = "520x". Without this, Shiny may try to draw the graph in a narrow space and trigger the margin error.
4. The base R plot needed snaller margins and text size. The simple barplot() version still needed: par( mar = c(4, 8, 3, 1) + 0.1, cex.main = 0.9, cex.lab = 0.8, cex.axis = 0.75). Without this, the graph labels can take too much space and cause the plot to fail.
5. Graph interpretation note was missing or not yet finalised. The note below the graph should clearly explain, The graph shows mean values only. Minimum and maximum values are shown in the table. Interpretation depends on the selected variable; for some variables, higher values indicate greater concern, while for others, lower values indicate srier conditions, It should also specifically mention that Bio017 and PPETmin are lower-is-drier variables.
6. Graph download should not be added yet. Day 4 should not include graph download. The priority is only, comparison graph appears correctly graph matches table values graph is understandable,
7. The graph must be checked against the comparison table. After fixing the display, the next test is to confirm that every graph bar uses the same Mean values shown in the comparison table.
8. Potential confusion from having both ggplot2 and base R plotting. It is okay to keep library(ggplot2) loaded for later, but the Day 4 comparison graph should use only one plotting method. Mixing instructions between ggplot2 and barplot() caused confusion during testing.

## Next steps

Continue improving comparison visuals, test draw-polygon and point-buffer AOI workflows, and begin code cleanup.

### Day 3 download check

| Check | Expected | Actual | Pass/Fail |
| ----- | -------- | ------ | --------- |
| Result table appears | Yes | Done | Pass |
| Comparison table appears | Yes | Done | Pass |
| Change from baseline appears | Yes | Done | Pass |
| Result CSV downloads | Yes | Done | Pass |
| Comparison CSV downloads | Yes | Done | Pass |
| Cropped raster downloads | Yes | Done | Pass |

### Fire Probability check

| Check | Result |
| ----- | ------ |
| Graph appears | Done |
| Baseline bar appears | Done |
| Future bar appears | Done |
| Units correct | Done |
| Title correct | Done |

### Bio05 check

| Check | Result |
| ----- | ------ |
| Graph appears | Done |
| Units = degrees C | Done |
| Bars match comparison table | Done |
| Change direction is understandable | Done |

### PPETmin check

| Check | Result |
| ----- | ------ |
| Graph appears | Done |
| Units = ratio | Done |
| Dry-condition note appears | Done |
| Bars match comparison table | Done |

### Graph matches comparison table check

| Scenario | Period | Table mean | Graph value matches? |
| -------- | ------ | ---------- | -------------------- |
| Baseline | 1981-2010 | 0.9 | Yes |
| SSP2-4.5 | 2041-2070 | 1.2 | Yes |

### Results tab layout

| Layout check | Result |
| ------------ | ------ |
| Result table visible | Done |
| Comparison table visible | Done |
| Graph visible without confusion | Done |
| Downloads easy to find | Done |
| Notes are not too long | Done |
