
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pmtables

<!-- badges: start -->

<!-- badges: end -->

Tables for pharmacometrics

## Installation

Installation information to be updated when ready.

## Examples

Continuous covariate summary\!

``` r
library(dplyr)
library(pmtables)


data <- pmtables:::data("id") 
data <- mutate(data, STUDY = STUDYf)
```

# Continuous by study

``` r
pt_cont_study(
  data, 
  cols = vars(BMI,ALB,AAG), 
  study_col = vars(Study = STUDY)
) 
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#jdcobcljll .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#jdcobcljll .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jdcobcljll .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#jdcobcljll .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#jdcobcljll .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jdcobcljll .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#jdcobcljll .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#jdcobcljll .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#jdcobcljll .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#jdcobcljll .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#jdcobcljll .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#jdcobcljll .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#jdcobcljll .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#jdcobcljll .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#jdcobcljll .gt_from_md > :first-child {
  margin-top: 0;
}

#jdcobcljll .gt_from_md > :last-child {
  margin-bottom: 0;
}

#jdcobcljll .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#jdcobcljll .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#jdcobcljll .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jdcobcljll .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#jdcobcljll .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#jdcobcljll .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#jdcobcljll .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#jdcobcljll .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jdcobcljll .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#jdcobcljll .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#jdcobcljll .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#jdcobcljll .gt_left {
  text-align: left;
}

#jdcobcljll .gt_center {
  text-align: center;
}

#jdcobcljll .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#jdcobcljll .gt_font_normal {
  font-weight: normal;
}

#jdcobcljll .gt_font_bold {
  font-weight: bold;
}

#jdcobcljll .gt_font_italic {
  font-style: italic;
}

#jdcobcljll .gt_super {
  font-size: 65%;
}

#jdcobcljll .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="jdcobcljll" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Variable

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Mean

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Median

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

SD<sup class="gt_footnote_marks">1</sup>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Min / Max

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Study 12-DEMO-001 (n=30)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

BMI

</td>

<td class="gt_row gt_right">

22.3

</td>

<td class="gt_row gt_right">

22.4

</td>

<td class="gt_row gt_right">

1.66

</td>

<td class="gt_row gt_right">

19.4 / 24.9

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

ALB

</td>

<td class="gt_row gt_right">

4.27

</td>

<td class="gt_row gt_right">

4.08

</td>

<td class="gt_row gt_right">

0.469

</td>

<td class="gt_row gt_right">

3.56 / 5.15

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

AAG

</td>

<td class="gt_row gt_right">

88.4

</td>

<td class="gt_row gt_right">

89.2

</td>

<td class="gt_row gt_right">

19.7

</td>

<td class="gt_row gt_right">

50.1 / 119

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Study 12-DEMO-002 (n=50)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

BMI

</td>

<td class="gt_row gt_right">

21.8

</td>

<td class="gt_row gt_right">

21.8

</td>

<td class="gt_row gt_right">

1.86

</td>

<td class="gt_row gt_right">

18.5 / 24.8

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

ALB

</td>

<td class="gt_row gt_right">

4.47

</td>

<td class="gt_row gt_right">

4.43

</td>

<td class="gt_row gt_right">

0.468

</td>

<td class="gt_row gt_right">

3.65 / 5.39

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

AAG

</td>

<td class="gt_row gt_right">

81.9

</td>

<td class="gt_row gt_right">

87.0

</td>

<td class="gt_row gt_right">

20.7

</td>

<td class="gt_row gt_right">

51.1 / 118

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Study 11-DEMO-005 (n=40)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

BMI

</td>

<td class="gt_row gt_right">

22.6

</td>

<td class="gt_row gt_right">

23.0

</td>

<td class="gt_row gt_right">

1.74

</td>

<td class="gt_row gt_right">

18.6 / 24.9

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

ALB

</td>

<td class="gt_row gt_right">

4.42

</td>

<td class="gt_row gt_right">

4.44

</td>

<td class="gt_row gt_right">

0.536

</td>

<td class="gt_row gt_right">

3.51 / 5.39

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

AAG

</td>

<td class="gt_row gt_right">

84.9

</td>

<td class="gt_row gt_right">

87.8

</td>

<td class="gt_row gt_right">

20.2

</td>

<td class="gt_row gt_right">

50.4 / 118

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Study 13-DEMO-001 (n=40)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

BMI

</td>

<td class="gt_row gt_right">

21.6

</td>

<td class="gt_row gt_right">

21.3

</td>

<td class="gt_row gt_right">

1.96

</td>

<td class="gt_row gt_right">

18.6 / 24.8

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

ALB

</td>

<td class="gt_row gt_right">

3.53

</td>

<td class="gt_row gt_right">

3.46

</td>

<td class="gt_row gt_right">

1.15

</td>

<td class="gt_row gt_right">

1.28 / 5.38

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

AAG

</td>

<td class="gt_row gt_right">

85.1

</td>

<td class="gt_row gt_right">

87.4

</td>

<td class="gt_row gt_right">

21.8

</td>

<td class="gt_row gt_right">

51.1 / 119

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

All data (n=160)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

BMI

</td>

<td class="gt_row gt_right">

22.0

</td>

<td class="gt_row gt_right">

22.2

</td>

<td class="gt_row gt_right">

1.84

</td>

<td class="gt_row gt_right">

18.5 / 24.9

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

ALB

</td>

<td class="gt_row gt_right">

4.19

</td>

<td class="gt_row gt_right">

4.31

</td>

<td class="gt_row gt_right">

0.809

</td>

<td class="gt_row gt_right">

1.28 / 5.39

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

AAG

</td>

<td class="gt_row gt_right">

84.7

</td>

<td class="gt_row gt_right">

87.8

</td>

<td class="gt_row gt_right">

20.6

</td>

<td class="gt_row gt_right">

50.1 / 119

</td>

</tr>

</tbody>

<tfoot>

<tr class="gt_footnotes">

<td colspan="5">

<p class="gt_footnote">

<sup class="gt_footnote_marks"> <em>1</em> </sup>

standard deviation <br />

</p>

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

# Continuous by covariate

``` r
pt_cont_long(
  data, 
  cols = vars(WT,SCR,CRCL), 
  panel = vars("Renal function" = RFf)
) 
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#iboksmeuju .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#iboksmeuju .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#iboksmeuju .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#iboksmeuju .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#iboksmeuju .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iboksmeuju .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#iboksmeuju .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#iboksmeuju .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#iboksmeuju .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#iboksmeuju .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#iboksmeuju .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#iboksmeuju .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#iboksmeuju .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#iboksmeuju .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#iboksmeuju .gt_from_md > :first-child {
  margin-top: 0;
}

#iboksmeuju .gt_from_md > :last-child {
  margin-bottom: 0;
}

#iboksmeuju .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#iboksmeuju .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#iboksmeuju .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iboksmeuju .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#iboksmeuju .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#iboksmeuju .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#iboksmeuju .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#iboksmeuju .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#iboksmeuju .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#iboksmeuju .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#iboksmeuju .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#iboksmeuju .gt_left {
  text-align: left;
}

#iboksmeuju .gt_center {
  text-align: center;
}

#iboksmeuju .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#iboksmeuju .gt_font_normal {
  font-weight: normal;
}

#iboksmeuju .gt_font_bold {
  font-weight: bold;
}

#iboksmeuju .gt_font_italic {
  font-style: italic;
}

#iboksmeuju .gt_super {
  font-size: 65%;
}

#iboksmeuju .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="iboksmeuju" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Variable

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Mean

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Median

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

SD<sup class="gt_footnote_marks">1</sup>

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">

Min / Max

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Renal function normal (n=130)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

WT

</td>

<td class="gt_row gt_right">

71.3

</td>

<td class="gt_row gt_right">

71.5

</td>

<td class="gt_row gt_right">

12.7

</td>

<td class="gt_row gt_right">

50.5 / 97.2

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

SCR

</td>

<td class="gt_row gt_right">

0.975

</td>

<td class="gt_row gt_right">

0.980

</td>

<td class="gt_row gt_right">

0.162

</td>

<td class="gt_row gt_right">

0.710 / 1.30

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

CRCL

</td>

<td class="gt_row gt_right">

103

</td>

<td class="gt_row gt_right">

102

</td>

<td class="gt_row gt_right">

8.54

</td>

<td class="gt_row gt_right">

90.6 / 126

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Renal function mild (n=10)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

WT

</td>

<td class="gt_row gt_right">

71.0

</td>

<td class="gt_row gt_right">

69.0

</td>

<td class="gt_row gt_right">

11.6

</td>

<td class="gt_row gt_right">

53.1 / 87.3

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

SCR

</td>

<td class="gt_row gt_right">

1.74

</td>

<td class="gt_row gt_right">

1.72

</td>

<td class="gt_row gt_right">

0.263

</td>

<td class="gt_row gt_right">

1.32 / 2.20

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

CRCL

</td>

<td class="gt_row gt_right">

75.0

</td>

<td class="gt_row gt_right">

74.9

</td>

<td class="gt_row gt_right">

7.52

</td>

<td class="gt_row gt_right">

65.9 / 85.4

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Renal function moderate (n=10)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

WT

</td>

<td class="gt_row gt_right">

67.8

</td>

<td class="gt_row gt_right">

62.8

</td>

<td class="gt_row gt_right">

16.8

</td>

<td class="gt_row gt_right">

43.6 / 91.0

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

SCR

</td>

<td class="gt_row gt_right">

2.83

</td>

<td class="gt_row gt_right">

2.80

</td>

<td class="gt_row gt_right">

0.282

</td>

<td class="gt_row gt_right">

2.46 / 3.23

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

CRCL

</td>

<td class="gt_row gt_right">

43.2

</td>

<td class="gt_row gt_right">

42.9

</td>

<td class="gt_row gt_right">

8.24

</td>

<td class="gt_row gt_right">

32.3 / 56.2

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

Renal function severe (n=10)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

WT

</td>

<td class="gt_row gt_right">

68.6

</td>

<td class="gt_row gt_right">

65.0

</td>

<td class="gt_row gt_right">

13.7

</td>

<td class="gt_row gt_right">

50.5 / 88.2

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

SCR

</td>

<td class="gt_row gt_right">

4.58

</td>

<td class="gt_row gt_right">

4.16

</td>

<td class="gt_row gt_right">

0.657

</td>

<td class="gt_row gt_right">

3.92 / 5.59

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

CRCL

</td>

<td class="gt_row gt_right">

22.6

</td>

<td class="gt_row gt_right">

22.6

</td>

<td class="gt_row gt_right">

3.77

</td>

<td class="gt_row gt_right">

15.4 / 28.9

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="5" class="gt_group_heading">

All data (n=160)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

WT

</td>

<td class="gt_row gt_right">

70.9

</td>

<td class="gt_row gt_right">

70.2

</td>

<td class="gt_row gt_right">

12.9

</td>

<td class="gt_row gt_right">

43.6 / 97.2

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

SCR

</td>

<td class="gt_row gt_right">

1.36

</td>

<td class="gt_row gt_right">

1.04

</td>

<td class="gt_row gt_right">

0.986

</td>

<td class="gt_row gt_right">

0.710 / 5.59

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

CRCL

</td>

<td class="gt_row gt_right">

92.4

</td>

<td class="gt_row gt_right">

98.8

</td>

<td class="gt_row gt_right">

25.2

</td>

<td class="gt_row gt_right">

15.4 / 126

</td>

</tr>

</tbody>

<tfoot>

<tr class="gt_footnotes">

<td colspan="5">

<p class="gt_footnote">

<sup class="gt_footnote_marks"> <em>1</em> </sup>

standard deviation <br />

</p>

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

# Categorical by study

``` r
pt_cat_study(
  data, 
  cols = vars(Sex = SEXf,"Renal function" = RFf), 
  study_col = vars(Study = STUDY)
) 
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uoeuezqwcn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uoeuezqwcn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uoeuezqwcn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uoeuezqwcn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uoeuezqwcn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uoeuezqwcn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uoeuezqwcn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uoeuezqwcn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uoeuezqwcn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uoeuezqwcn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uoeuezqwcn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uoeuezqwcn .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uoeuezqwcn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uoeuezqwcn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uoeuezqwcn .gt_from_md > :first-child {
  margin-top: 0;
}

#uoeuezqwcn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uoeuezqwcn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uoeuezqwcn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#uoeuezqwcn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uoeuezqwcn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uoeuezqwcn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uoeuezqwcn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uoeuezqwcn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uoeuezqwcn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uoeuezqwcn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uoeuezqwcn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uoeuezqwcn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uoeuezqwcn .gt_left {
  text-align: left;
}

#uoeuezqwcn .gt_center {
  text-align: center;
}

#uoeuezqwcn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uoeuezqwcn .gt_font_normal {
  font-weight: normal;
}

#uoeuezqwcn .gt_font_bold {
  font-weight: bold;
}

#uoeuezqwcn .gt_font_italic {
  font-style: italic;
}

#uoeuezqwcn .gt_super {
  font-size: 65%;
}

#uoeuezqwcn .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="uoeuezqwcn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">

<span class="gt_column_spanner">Study</span>

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

All
studies

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

12-DEMO-001

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

12-DEMO-002

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

11-DEMO-005

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

13-DEMO-001

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="6" class="gt_group_heading">

Sex

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

male

</td>

<td class="gt_row gt_left">

10 (33.3)

</td>

<td class="gt_row gt_left">

18 (36.0)

</td>

<td class="gt_row gt_left">

29 (72.5)

</td>

<td class="gt_row gt_left">

23 (57.5)

</td>

<td class="gt_row gt_left">

80 (50.0)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

female

</td>

<td class="gt_row gt_left">

20 (66.7)

</td>

<td class="gt_row gt_left">

32 (64.0)

</td>

<td class="gt_row gt_left">

11 (27.5)

</td>

<td class="gt_row gt_left">

17 (42.5)

</td>

<td class="gt_row gt_left">

80 (50.0)

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="6" class="gt_group_heading">

Renal function

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

normal

</td>

<td class="gt_row gt_left">

30 (100.0)

</td>

<td class="gt_row gt_left">

50 (100.0)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

<td class="gt_row gt_left">

40 (100.0)

</td>

<td class="gt_row gt_left">

130 (81.2)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

mild

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

10 (6.2)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

moderate

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

10 (6.2)

</td>

</tr>

<tr>

<td class="gt_row gt_left gt_stub">

severe

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

10 (6.2)

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="6">

Summaries are count (percent)

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

## Wide version

  - Both tables look wide; at this point, “wide” means that the
    covariates are going east to west

<!-- end list -->

``` r
pt_cat_study(
  data, 
  cols = vars(Sex = SEXf,"Renal function" = RFf), 
  study_col = vars(Study = STUDY), 
  wide = TRUE
) 
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gwzbnpuzij .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#gwzbnpuzij .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwzbnpuzij .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gwzbnpuzij .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gwzbnpuzij .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwzbnpuzij .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#gwzbnpuzij .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#gwzbnpuzij .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#gwzbnpuzij .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gwzbnpuzij .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gwzbnpuzij .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gwzbnpuzij .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#gwzbnpuzij .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#gwzbnpuzij .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gwzbnpuzij .gt_from_md > :first-child {
  margin-top: 0;
}

#gwzbnpuzij .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gwzbnpuzij .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#gwzbnpuzij .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#gwzbnpuzij .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwzbnpuzij .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gwzbnpuzij .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gwzbnpuzij .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gwzbnpuzij .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gwzbnpuzij .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwzbnpuzij .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gwzbnpuzij .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#gwzbnpuzij .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gwzbnpuzij .gt_left {
  text-align: left;
}

#gwzbnpuzij .gt_center {
  text-align: center;
}

#gwzbnpuzij .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gwzbnpuzij .gt_font_normal {
  font-weight: normal;
}

#gwzbnpuzij .gt_font_bold {
  font-weight: bold;
}

#gwzbnpuzij .gt_font_italic {
  font-style: italic;
}

#gwzbnpuzij .gt_super {
  font-size: 65%;
}

#gwzbnpuzij .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="gwzbnpuzij" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Study

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Sex</span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">

<span class="gt_column_spanner">Renal
function</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

male

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

female

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

normal

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

mild

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

moderate

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

severe

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_empty_group_heading">

</td>

</tr>

<tr>

<td class="gt_row gt_left">

12-DEMO-001

</td>

<td class="gt_row gt_left">

10 (33.3)

</td>

<td class="gt_row gt_left">

20 (66.7)

</td>

<td class="gt_row gt_left">

30 (100.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

12-DEMO-002

</td>

<td class="gt_row gt_left">

18 (36.0)

</td>

<td class="gt_row gt_left">

32 (64.0)

</td>

<td class="gt_row gt_left">

50 (100.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

11-DEMO-005

</td>

<td class="gt_row gt_left">

29 (72.5)

</td>

<td class="gt_row gt_left">

11 (27.5)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

<td class="gt_row gt_left">

10 (25.0)

</td>

</tr>

<tr>

<td class="gt_row gt_left">

13-DEMO-001

</td>

<td class="gt_row gt_left">

23 (57.5)

</td>

<td class="gt_row gt_left">

17 (42.5)

</td>

<td class="gt_row gt_left">

40 (100.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

<td class="gt_row gt_left">

0 (0.0)

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_group_heading">

Total

</td>

</tr>

<tr>

<td class="gt_row gt_left">

All studies

</td>

<td class="gt_row gt_left">

80 (50.0)

</td>

<td class="gt_row gt_left">

80 (50.0)

</td>

<td class="gt_row gt_left">

130 (81.2)

</td>

<td class="gt_row gt_left">

10 (6.2)

</td>

<td class="gt_row gt_left">

10 (6.2)

</td>

<td class="gt_row gt_left">

10 (6.2)

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

Summaries are count (percent)

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

# Data disposition

## By study

``` r
data <- pmtables:::data("id")
pt_data_inventory(
  data, 
  outer = vars(Study = STUDYf)
)
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#hvitxwhogm .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#hvitxwhogm .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hvitxwhogm .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#hvitxwhogm .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#hvitxwhogm .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hvitxwhogm .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#hvitxwhogm .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#hvitxwhogm .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#hvitxwhogm .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#hvitxwhogm .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#hvitxwhogm .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#hvitxwhogm .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#hvitxwhogm .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#hvitxwhogm .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#hvitxwhogm .gt_from_md > :first-child {
  margin-top: 0;
}

#hvitxwhogm .gt_from_md > :last-child {
  margin-bottom: 0;
}

#hvitxwhogm .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#hvitxwhogm .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#hvitxwhogm .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hvitxwhogm .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#hvitxwhogm .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#hvitxwhogm .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#hvitxwhogm .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#hvitxwhogm .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hvitxwhogm .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#hvitxwhogm .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#hvitxwhogm .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#hvitxwhogm .gt_left {
  text-align: left;
}

#hvitxwhogm .gt_center {
  text-align: center;
}

#hvitxwhogm .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#hvitxwhogm .gt_font_normal {
  font-weight: normal;
}

#hvitxwhogm .gt_font_bold {
  font-weight: bold;
}

#hvitxwhogm .gt_font_italic {
  font-style: italic;
}

#hvitxwhogm .gt_super {
  font-size: 65%;
}

#hvitxwhogm .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="hvitxwhogm" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Study

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">

<span class="gt_column_spanner">Number</span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Percent</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

SUBJ

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

MISS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

BQL

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

BQL

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

12-DEMO-001

</td>

<td class="gt_row gt_center">

30

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

30

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

18.8

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

12-DEMO-002

</td>

<td class="gt_row gt_center">

50

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

50

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

31.2

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

11-DEMO-005

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

25.0

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

13-DEMO-001

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

25.0

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Grand Total

</td>

<td class="gt_row gt_center">

160

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

160

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

100.0

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

SUBJ: subjects; OBS: observations; MISS: missing; BQL: below
quantitation limit

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

## By study and covariate

``` r
data <- pmtables:::data("obs") %>% filter(SEQ==1)
pt_data_inventory(
  data, 
  outer = vars(Study = STUDYf), 
  inner = vars(Sex = SEXf)
)
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#dewqxcdztk .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#dewqxcdztk .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dewqxcdztk .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#dewqxcdztk .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#dewqxcdztk .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dewqxcdztk .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#dewqxcdztk .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#dewqxcdztk .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#dewqxcdztk .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#dewqxcdztk .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#dewqxcdztk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#dewqxcdztk .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#dewqxcdztk .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#dewqxcdztk .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#dewqxcdztk .gt_from_md > :first-child {
  margin-top: 0;
}

#dewqxcdztk .gt_from_md > :last-child {
  margin-bottom: 0;
}

#dewqxcdztk .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#dewqxcdztk .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#dewqxcdztk .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dewqxcdztk .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#dewqxcdztk .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#dewqxcdztk .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#dewqxcdztk .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#dewqxcdztk .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dewqxcdztk .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#dewqxcdztk .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#dewqxcdztk .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#dewqxcdztk .gt_left {
  text-align: left;
}

#dewqxcdztk .gt_center {
  text-align: center;
}

#dewqxcdztk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#dewqxcdztk .gt_font_normal {
  font-weight: normal;
}

#dewqxcdztk .gt_font_bold {
  font-weight: bold;
}

#dewqxcdztk .gt_font_italic {
  font-style: italic;
}

#dewqxcdztk .gt_super {
  font-size: 65%;
}

#dewqxcdztk .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="dewqxcdztk" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Sex

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">

<span class="gt_column_spanner">Number</span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Group
percent</span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Overall
percent</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

SUBJ

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

MISS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

BQL

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

BQL

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

BQL

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="9" class="gt_group_heading">

Study: 12-DEMO-001

</td>

</tr>

<tr>

<td class="gt_row gt_left">

male

</td>

<td class="gt_row gt_center">

10

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

150

</td>

<td class="gt_row gt_center">

3

</td>

<td class="gt_row gt_left">

33.3

</td>

<td class="gt_row gt_left">

0.7

</td>

<td class="gt_row gt_left">

4.7

</td>

<td class="gt_row gt_left">

0.1

</td>

</tr>

<tr>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_center">

20

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

300

</td>

<td class="gt_row gt_center">

23

</td>

<td class="gt_row gt_left">

66.7

</td>

<td class="gt_row gt_left">

5.1

</td>

<td class="gt_row gt_left">

9.3

</td>

<td class="gt_row gt_left">

0.7

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="9" class="gt_group_heading">

Study: 12-DEMO-002

</td>

</tr>

<tr>

<td class="gt_row gt_left">

male

</td>

<td class="gt_row gt_center">

18

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

432

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

36.0

</td>

<td class="gt_row gt_left">

0.0

</td>

<td class="gt_row gt_left">

13.5

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_center">

32

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

768

</td>

<td class="gt_row gt_center">

1

</td>

<td class="gt_row gt_left">

64.0

</td>

<td class="gt_row gt_left">

0.1

</td>

<td class="gt_row gt_left">

23.9

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="9" class="gt_group_heading">

Study: 11-DEMO-005

</td>

</tr>

<tr>

<td class="gt_row gt_left">

male

</td>

<td class="gt_row gt_center">

29

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

696

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

72.5

</td>

<td class="gt_row gt_left">

0.0

</td>

<td class="gt_row gt_left">

21.7

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_center">

11

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

264

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

27.5

</td>

<td class="gt_row gt_left">

0.0

</td>

<td class="gt_row gt_left">

8.2

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="9" class="gt_group_heading">

Study: 13-DEMO-001

</td>

</tr>

<tr>

<td class="gt_row gt_left">

male

</td>

<td class="gt_row gt_center">

23

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

345

</td>

<td class="gt_row gt_center">

28

</td>

<td class="gt_row gt_left">

57.5

</td>

<td class="gt_row gt_left">

4.7

</td>

<td class="gt_row gt_left">

10.7

</td>

<td class="gt_row gt_left">

0.9

</td>

</tr>

<tr>

<td class="gt_row gt_left">

female

</td>

<td class="gt_row gt_center">

17

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

255

</td>

<td class="gt_row gt_center">

13

</td>

<td class="gt_row gt_left">

42.5

</td>

<td class="gt_row gt_left">

2.2

</td>

<td class="gt_row gt_left">

7.9

</td>

<td class="gt_row gt_left">

0.4

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Grand Total

</td>

<td class="gt_row gt_center">

160

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

3210

</td>

<td class="gt_row gt_center">

68

</td>

<td class="gt_row gt_left">

100.0

</td>

<td class="gt_row gt_left">

2.1

</td>

<td class="gt_row gt_left">

100.0

</td>

<td class="gt_row gt_left">

2.1

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="9">

SUBJ: subjects; OBS: observations; MISS: missing; BQL: below
quantitation limit

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->

## Stacked by endpoint

``` r
data <- pmtables:::data("obs")
pt_data_inventory(
  data, 
  outer = vars(Endpoint = SEQf), 
  inner = vars(Study = STUDYf), 
  stacked = TRUE
)
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#kfjxwordxn .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#kfjxwordxn .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kfjxwordxn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#kfjxwordxn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#kfjxwordxn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kfjxwordxn .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#kfjxwordxn .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#kfjxwordxn .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#kfjxwordxn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#kfjxwordxn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#kfjxwordxn .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#kfjxwordxn .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#kfjxwordxn .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#kfjxwordxn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#kfjxwordxn .gt_from_md > :first-child {
  margin-top: 0;
}

#kfjxwordxn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#kfjxwordxn .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#kfjxwordxn .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#kfjxwordxn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kfjxwordxn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#kfjxwordxn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#kfjxwordxn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#kfjxwordxn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#kfjxwordxn .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kfjxwordxn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#kfjxwordxn .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#kfjxwordxn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#kfjxwordxn .gt_left {
  text-align: left;
}

#kfjxwordxn .gt_center {
  text-align: center;
}

#kfjxwordxn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#kfjxwordxn .gt_font_normal {
  font-weight: normal;
}

#kfjxwordxn .gt_font_bold {
  font-weight: bold;
}

#kfjxwordxn .gt_font_italic {
  font-style: italic;
}

#kfjxwordxn .gt_super {
  font-size: 65%;
}

#kfjxwordxn .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="kfjxwordxn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Study

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="4">

<span class="gt_column_spanner">Number</span>

</th>

<th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="2">

<span class="gt_column_spanner">Percent</span>

</th>

</tr>

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

SUBJ

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

MISS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

BQL

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

BQL

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_group_heading">

Endpoint: DEMO PK

</td>

</tr>

<tr>

<td class="gt_row gt_left">

12-DEMO-001

</td>

<td class="gt_row gt_center">

30

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

450

</td>

<td class="gt_row gt_center">

26

</td>

<td class="gt_row gt_left">

14.0

</td>

<td class="gt_row gt_left">

0.8

</td>

</tr>

<tr>

<td class="gt_row gt_left">

12-DEMO-002

</td>

<td class="gt_row gt_center">

50

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

1200

</td>

<td class="gt_row gt_center">

1

</td>

<td class="gt_row gt_left">

37.4

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

11-DEMO-005

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

960

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

29.9

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

13-DEMO-001

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

600

</td>

<td class="gt_row gt_center">

41

</td>

<td class="gt_row gt_left">

18.7

</td>

<td class="gt_row gt_left">

1.3

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Group Total

</td>

<td class="gt_row gt_center">

160

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

3210

</td>

<td class="gt_row gt_center">

68

</td>

<td class="gt_row gt_left">

100.0

</td>

<td class="gt_row gt_left">

2.1

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_group_heading">

Endpoint: ESTRDIOL

</td>

</tr>

<tr>

<td class="gt_row gt_left">

11-DEMO-005

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

50.0

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

13-DEMO-001

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

50.0

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Group Total

</td>

<td class="gt_row gt_center">

80

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

80

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

100.0

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr class="gt_group_heading_row">

<td colspan="7" class="gt_group_heading">

Endpoint: BMD

</td>

</tr>

<tr>

<td class="gt_row gt_left">

11-DEMO-005

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

120

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_left">

50.0

</td>

<td class="gt_row gt_left">

0.0

</td>

</tr>

<tr>

<td class="gt_row gt_left">

13-DEMO-001

</td>

<td class="gt_row gt_center">

40

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

120

</td>

<td class="gt_row gt_center">

4

</td>

<td class="gt_row gt_left">

50.0

</td>

<td class="gt_row gt_left">

1.7

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Group Total

</td>

<td class="gt_row gt_center">

80

</td>

<td class="gt_row gt_center">

0

</td>

<td class="gt_row gt_center">

240

</td>

<td class="gt_row gt_center">

4

</td>

<td class="gt_row gt_left">

100.0

</td>

<td class="gt_row gt_left">

1.7

</td>

</tr>

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="7">

SUBJ: subjects; OBS: observations; MISS: missing; BQL: below
quantitation limit

</td>

</tr>

</tfoot>

</table>

</div>

<!--/html_preserve-->
