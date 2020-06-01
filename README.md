
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

#pdmresudik .gt_table {
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

#pdmresudik .gt_heading {
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

#pdmresudik .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#pdmresudik .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#pdmresudik .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdmresudik .gt_col_headings {
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

#pdmresudik .gt_col_heading {
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

#pdmresudik .gt_column_spanner_outer {
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

#pdmresudik .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#pdmresudik .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#pdmresudik .gt_column_spanner {
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

#pdmresudik .gt_group_heading {
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

#pdmresudik .gt_empty_group_heading {
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

#pdmresudik .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#pdmresudik .gt_from_md > :first-child {
  margin-top: 0;
}

#pdmresudik .gt_from_md > :last-child {
  margin-bottom: 0;
}

#pdmresudik .gt_row {
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

#pdmresudik .gt_stub {
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

#pdmresudik .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdmresudik .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#pdmresudik .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#pdmresudik .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#pdmresudik .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#pdmresudik .gt_footnotes {
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

#pdmresudik .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#pdmresudik .gt_sourcenotes {
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

#pdmresudik .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#pdmresudik .gt_left {
  text-align: left;
}

#pdmresudik .gt_center {
  text-align: center;
}

#pdmresudik .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#pdmresudik .gt_font_normal {
  font-weight: normal;
}

#pdmresudik .gt_font_bold {
  font-weight: bold;
}

#pdmresudik .gt_font_italic {
  font-style: italic;
}

#pdmresudik .gt_super {
  font-size: 65%;
}

#pdmresudik .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="pdmresudik" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

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

#wlguynqlmh .gt_table {
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

#wlguynqlmh .gt_heading {
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

#wlguynqlmh .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wlguynqlmh .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wlguynqlmh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wlguynqlmh .gt_col_headings {
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

#wlguynqlmh .gt_col_heading {
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

#wlguynqlmh .gt_column_spanner_outer {
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

#wlguynqlmh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wlguynqlmh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wlguynqlmh .gt_column_spanner {
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

#wlguynqlmh .gt_group_heading {
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

#wlguynqlmh .gt_empty_group_heading {
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

#wlguynqlmh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wlguynqlmh .gt_from_md > :first-child {
  margin-top: 0;
}

#wlguynqlmh .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wlguynqlmh .gt_row {
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

#wlguynqlmh .gt_stub {
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

#wlguynqlmh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wlguynqlmh .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#wlguynqlmh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wlguynqlmh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wlguynqlmh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wlguynqlmh .gt_footnotes {
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

#wlguynqlmh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#wlguynqlmh .gt_sourcenotes {
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

#wlguynqlmh .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#wlguynqlmh .gt_left {
  text-align: left;
}

#wlguynqlmh .gt_center {
  text-align: center;
}

#wlguynqlmh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wlguynqlmh .gt_font_normal {
  font-weight: normal;
}

#wlguynqlmh .gt_font_bold {
  font-weight: bold;
}

#wlguynqlmh .gt_font_italic {
  font-style: italic;
}

#wlguynqlmh .gt_super {
  font-size: 65%;
}

#wlguynqlmh .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="wlguynqlmh" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

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

#qalqvzxcbn .gt_table {
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

#qalqvzxcbn .gt_heading {
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

#qalqvzxcbn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#qalqvzxcbn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#qalqvzxcbn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qalqvzxcbn .gt_col_headings {
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

#qalqvzxcbn .gt_col_heading {
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

#qalqvzxcbn .gt_column_spanner_outer {
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

#qalqvzxcbn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#qalqvzxcbn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#qalqvzxcbn .gt_column_spanner {
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

#qalqvzxcbn .gt_group_heading {
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

#qalqvzxcbn .gt_empty_group_heading {
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

#qalqvzxcbn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#qalqvzxcbn .gt_from_md > :first-child {
  margin-top: 0;
}

#qalqvzxcbn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#qalqvzxcbn .gt_row {
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

#qalqvzxcbn .gt_stub {
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

#qalqvzxcbn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qalqvzxcbn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#qalqvzxcbn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#qalqvzxcbn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#qalqvzxcbn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#qalqvzxcbn .gt_footnotes {
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

#qalqvzxcbn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#qalqvzxcbn .gt_sourcenotes {
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

#qalqvzxcbn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#qalqvzxcbn .gt_left {
  text-align: left;
}

#qalqvzxcbn .gt_center {
  text-align: center;
}

#qalqvzxcbn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#qalqvzxcbn .gt_font_normal {
  font-weight: normal;
}

#qalqvzxcbn .gt_font_bold {
  font-weight: bold;
}

#qalqvzxcbn .gt_font_italic {
  font-style: italic;
}

#qalqvzxcbn .gt_super {
  font-size: 65%;
}

#qalqvzxcbn .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="qalqvzxcbn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

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

#doprduppkd .gt_table {
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

#doprduppkd .gt_heading {
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

#doprduppkd .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#doprduppkd .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#doprduppkd .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#doprduppkd .gt_col_headings {
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

#doprduppkd .gt_col_heading {
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

#doprduppkd .gt_column_spanner_outer {
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

#doprduppkd .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#doprduppkd .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#doprduppkd .gt_column_spanner {
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

#doprduppkd .gt_group_heading {
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

#doprduppkd .gt_empty_group_heading {
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

#doprduppkd .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#doprduppkd .gt_from_md > :first-child {
  margin-top: 0;
}

#doprduppkd .gt_from_md > :last-child {
  margin-bottom: 0;
}

#doprduppkd .gt_row {
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

#doprduppkd .gt_stub {
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

#doprduppkd .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#doprduppkd .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#doprduppkd .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#doprduppkd .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#doprduppkd .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#doprduppkd .gt_footnotes {
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

#doprduppkd .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#doprduppkd .gt_sourcenotes {
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

#doprduppkd .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#doprduppkd .gt_left {
  text-align: left;
}

#doprduppkd .gt_center {
  text-align: center;
}

#doprduppkd .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#doprduppkd .gt_font_normal {
  font-weight: normal;
}

#doprduppkd .gt_font_bold {
  font-weight: bold;
}

#doprduppkd .gt_font_italic {
  font-style: italic;
}

#doprduppkd .gt_super {
  font-size: 65%;
}

#doprduppkd .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="doprduppkd" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

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

#eeynpoobin .gt_table {
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

#eeynpoobin .gt_heading {
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

#eeynpoobin .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#eeynpoobin .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#eeynpoobin .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eeynpoobin .gt_col_headings {
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

#eeynpoobin .gt_col_heading {
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

#eeynpoobin .gt_column_spanner_outer {
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

#eeynpoobin .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#eeynpoobin .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#eeynpoobin .gt_column_spanner {
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

#eeynpoobin .gt_group_heading {
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

#eeynpoobin .gt_empty_group_heading {
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

#eeynpoobin .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#eeynpoobin .gt_from_md > :first-child {
  margin-top: 0;
}

#eeynpoobin .gt_from_md > :last-child {
  margin-bottom: 0;
}

#eeynpoobin .gt_row {
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

#eeynpoobin .gt_stub {
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

#eeynpoobin .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eeynpoobin .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#eeynpoobin .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#eeynpoobin .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#eeynpoobin .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#eeynpoobin .gt_footnotes {
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

#eeynpoobin .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#eeynpoobin .gt_sourcenotes {
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

#eeynpoobin .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#eeynpoobin .gt_left {
  text-align: left;
}

#eeynpoobin .gt_center {
  text-align: center;
}

#eeynpoobin .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#eeynpoobin .gt_font_normal {
  font-weight: normal;
}

#eeynpoobin .gt_font_bold {
  font-weight: bold;
}

#eeynpoobin .gt_font_italic {
  font-style: italic;
}

#eeynpoobin .gt_super {
  font-size: 65%;
}

#eeynpoobin .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="eeynpoobin" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

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

<td class="gt_sourcenote" colspan="6">

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

#ckzatqutbn .gt_table {
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

#ckzatqutbn .gt_heading {
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

#ckzatqutbn .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ckzatqutbn .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ckzatqutbn .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ckzatqutbn .gt_col_headings {
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

#ckzatqutbn .gt_col_heading {
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

#ckzatqutbn .gt_column_spanner_outer {
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

#ckzatqutbn .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ckzatqutbn .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ckzatqutbn .gt_column_spanner {
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

#ckzatqutbn .gt_group_heading {
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

#ckzatqutbn .gt_empty_group_heading {
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

#ckzatqutbn .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ckzatqutbn .gt_from_md > :first-child {
  margin-top: 0;
}

#ckzatqutbn .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ckzatqutbn .gt_row {
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

#ckzatqutbn .gt_stub {
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

#ckzatqutbn .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ckzatqutbn .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ckzatqutbn .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ckzatqutbn .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ckzatqutbn .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ckzatqutbn .gt_footnotes {
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

#ckzatqutbn .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ckzatqutbn .gt_sourcenotes {
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

#ckzatqutbn .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ckzatqutbn .gt_left {
  text-align: left;
}

#ckzatqutbn .gt_center {
  text-align: center;
}

#ckzatqutbn .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ckzatqutbn .gt_font_normal {
  font-weight: normal;
}

#ckzatqutbn .gt_font_bold {
  font-weight: bold;
}

#ckzatqutbn .gt_font_italic {
  font-style: italic;
}

#ckzatqutbn .gt_super {
  font-size: 65%;
}

#ckzatqutbn .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="ckzatqutbn" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

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

</tbody>

<tfoot class="gt_sourcenotes">

<tr>

<td class="gt_sourcenote" colspan="6">

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
  panel = vars(Endpoint = SEQf), 
  by = vars(Study = STUDYf), 
  stacked = TRUE
)
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#bxajyffoon .gt_table {
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

#bxajyffoon .gt_heading {
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

#bxajyffoon .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#bxajyffoon .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#bxajyffoon .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bxajyffoon .gt_col_headings {
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

#bxajyffoon .gt_col_heading {
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

#bxajyffoon .gt_column_spanner_outer {
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

#bxajyffoon .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#bxajyffoon .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#bxajyffoon .gt_column_spanner {
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

#bxajyffoon .gt_group_heading {
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

#bxajyffoon .gt_empty_group_heading {
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

#bxajyffoon .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#bxajyffoon .gt_from_md > :first-child {
  margin-top: 0;
}

#bxajyffoon .gt_from_md > :last-child {
  margin-bottom: 0;
}

#bxajyffoon .gt_row {
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

#bxajyffoon .gt_stub {
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

#bxajyffoon .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bxajyffoon .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#bxajyffoon .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#bxajyffoon .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#bxajyffoon .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#bxajyffoon .gt_footnotes {
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

#bxajyffoon .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#bxajyffoon .gt_sourcenotes {
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

#bxajyffoon .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#bxajyffoon .gt_left {
  text-align: left;
}

#bxajyffoon .gt_center {
  text-align: center;
}

#bxajyffoon .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#bxajyffoon .gt_font_normal {
  font-weight: normal;
}

#bxajyffoon .gt_font_bold {
  font-weight: bold;
}

#bxajyffoon .gt_font_italic {
  font-style: italic;
}

#bxajyffoon .gt_super {
  font-size: 65%;
}

#bxajyffoon .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="bxajyffoon" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

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
