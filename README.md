
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

<div id="eclovztkkj" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

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

<td class="gt_row gt_right gt_striped">

4.27

</td>

<td class="gt_row gt_right gt_striped">

4.08

</td>

<td class="gt_row gt_right gt_striped">

0.469

</td>

<td class="gt_row gt_right gt_striped">

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

<td class="gt_row gt_right gt_striped">

21.8

</td>

<td class="gt_row gt_right gt_striped">

21.8

</td>

<td class="gt_row gt_right gt_striped">

1.86

</td>

<td class="gt_row gt_right gt_striped">

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

<td class="gt_row gt_right gt_striped">

81.9

</td>

<td class="gt_row gt_right gt_striped">

87.0

</td>

<td class="gt_row gt_right gt_striped">

20.7

</td>

<td class="gt_row gt_right gt_striped">

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

<td class="gt_row gt_right gt_striped">

4.42

</td>

<td class="gt_row gt_right gt_striped">

4.44

</td>

<td class="gt_row gt_right gt_striped">

0.536

</td>

<td class="gt_row gt_right gt_striped">

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

<td class="gt_row gt_right gt_striped">

21.6

</td>

<td class="gt_row gt_right gt_striped">

21.3

</td>

<td class="gt_row gt_right gt_striped">

1.96

</td>

<td class="gt_row gt_right gt_striped">

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

<td class="gt_row gt_right gt_striped">

85.1

</td>

<td class="gt_row gt_right gt_striped">

87.4

</td>

<td class="gt_row gt_right gt_striped">

21.8

</td>

<td class="gt_row gt_right gt_striped">

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

<td class="gt_row gt_right gt_striped">

4.19

</td>

<td class="gt_row gt_right gt_striped">

4.31

</td>

<td class="gt_row gt_right gt_striped">

0.809

</td>

<td class="gt_row gt_right gt_striped">

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

# Categorical by study

``` r
pt_cat_study(
  data, 
  cols = vars(Sex = SEXf,"Renal function" = RFf), 
  study_col = vars(Study = STUDY)
) 
```

<!--html_preserve-->

<div id="ldpbjqvarq" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1">

</th>

<th class="gt_col_heading gt_center gt_columns_top_border gt_column_spanner" rowspan="1" colspan="4">

Study

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

<td class="gt_row gt_left gt_striped">

20 (66.7)

</td>

<td class="gt_row gt_left gt_striped">

32 (64.0)

</td>

<td class="gt_row gt_left gt_striped">

11 (27.5)

</td>

<td class="gt_row gt_left gt_striped">

17 (42.5)

</td>

<td class="gt_row gt_left gt_striped">

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

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

10 (25.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

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

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

10 (25.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

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

``` r
pt_cat_study(
  data, 
  cols = vars(Sex = SEXf,"Renal function" = RFf), 
  study_col = vars(Study = STUDY), 
  wide = TRUE
) 
```

<!--html_preserve-->

<div id="vfkhjepjhs" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Study

</th>

<th class="gt_col_heading gt_center gt_columns_top_border gt_column_spanner gt_sep_right" rowspan="1" colspan="2">

Sex

</th>

<th class="gt_col_heading gt_center gt_columns_top_border gt_column_spanner" rowspan="1" colspan="4">

Renal
function

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

<td class="gt_row gt_left gt_striped">

12-DEMO-002

</td>

<td class="gt_row gt_left gt_striped">

18 (36.0)

</td>

<td class="gt_row gt_left gt_striped">

32 (64.0)

</td>

<td class="gt_row gt_left gt_striped">

50 (100.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

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

<td class="gt_row gt_left gt_striped">

13-DEMO-001

</td>

<td class="gt_row gt_left gt_striped">

23 (57.5)

</td>

<td class="gt_row gt_left gt_striped">

17 (42.5)

</td>

<td class="gt_row gt_left gt_striped">

40 (100.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

0 (0.0)

</td>

<td class="gt_row gt_left gt_striped">

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

``` r
data <- pmtables:::data("id")
pt_data_inventory(
  data, 
  outer = vars(Study = STUDYf)
)
```

<!--html_preserve-->

<div id="cwbpfylulu" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

Study

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

SUBJ

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

MISS

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

OBS

</th>

<th class="gt_col_heading gt_center gt_columns_bottom_border" rowspan="2" colspan="1">

BQL

</th>

<th class="gt_col_heading gt_center gt_columns_top_border gt_column_spanner" rowspan="1" colspan="2">

Percent

</th>

</tr>

<tr>

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

<td class="gt_row gt_left gt_striped">

12-DEMO-002

</td>

<td class="gt_row gt_center gt_striped">

50

</td>

<td class="gt_row gt_center gt_striped">

0

</td>

<td class="gt_row gt_center gt_striped">

50

</td>

<td class="gt_row gt_center gt_striped">

0

</td>

<td class="gt_row gt_left gt_striped">

31.2

</td>

<td class="gt_row gt_left gt_striped">

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

<td class="gt_row gt_left gt_striped">

13-DEMO-001

</td>

<td class="gt_row gt_center gt_striped">

40

</td>

<td class="gt_row gt_center gt_striped">

0

</td>

<td class="gt_row gt_center gt_striped">

40

</td>

<td class="gt_row gt_center gt_striped">

0

</td>

<td class="gt_row gt_left gt_striped">

25.0

</td>

<td class="gt_row gt_left gt_striped">

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
