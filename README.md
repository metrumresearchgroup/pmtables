
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
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(pmtables)
library(pmplots)
```

    ## Loading required package: ggplot2

``` r
data <- pmplots_data_id()
```

``` r
pt_cont_study(data, cols = vars(BMI,ALB,AAG), study_col = vars(Study = STUDYc)) 
```

<!--html_preserve-->

<div id="bvuagparne" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

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

Study SAD (n=30)

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

Study MAD (n=50)

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

Study Renal (n=40)

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

Study Hepatic (n=40)

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
