# Requirements Specification: pmtables 0.4.1.9003

## Scope

The purpose of this document is to define specific criteria for each testing
task.  Testing shall be conducted in accordance with the requirements within this
document. The Requirement Specifications ensure that each requirement is tested.

 
## User Story: PMT-S001 Sanitize non-math columns

As a user, I would like non-escaped, non-math table contents sanitized when they contain `_` or `%`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                    |
|:-------------|:----------------------------|
|PMT-TEST-0183 |table contents are sanitized |

<hr>

## User Story: PMT-S002 Set table font size

As a user, I want to be able to set the font size for the table.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name           |
|:-------------|:-------------------|
|PMT-TEST-0184 |test-sizes-fontsize |

<hr>

## User Story: PMT-S003 Option to scrub column tags from column names

As a user, I want to issue a command to strip `tag.` from `tag.column-name`.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name            |
|:-------------|:--------------------|
|PMT-TEST-0209 |de-tag column labels |

<hr>

## User Story: PMT-S004 longtable functionality

As a user, I want to be able to create long tables that span multiple pages.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                  |
|:-------------|:--------------------------|
|PMT-TEST-0134 |test-longtable stable_long |

<hr>

## User Story: PMT-S005 Sanitize table notes

As a user, I want table notes sanitized for `_` or `%` characters.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name           |
|:-------------|:-------------------|
|PMT-TEST-0180 |files are sanitized |
|PMT-TEST-0181 |notes are sanitized |

<hr>

## User Story: PMT-S006 Make longtable using pipe interface

As a user, I want to be able to optionally render a table as `longtable` using the pipeable interface.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                     |
|:-------------|:-----------------------------|
|PMT-TEST-0230 |stobject equivalent longtable |

<hr>

## User Story: PMT-S007 Save table code

As a user, I want to be able to automatically save a file to an output location that also appears in the table note text.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                    |
|:-------------|:----------------------------|
|PMT-TEST-0237 |test-table-utils-stable_save |

<hr>

## User Story: PMT-S008 Pipe interface

As a user, I want to be able to assemble tables within a pipe interface.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                              |
|:-------------|:--------------------------------------|
|PMT-TEST-0215 |stobject equivalent hline              |
|PMT-TEST-0217 |stobject equivalent cols_bold          |
|PMT-TEST-0218 |stobject equivalent panel              |
|PMT-TEST-0219 |stobject equivalent sumrow             |
|PMT-TEST-0220 |stobject equivalent span               |
|PMT-TEST-0221 |stobject equivalent files              |
|PMT-TEST-0222 |stobject equivalent drop               |
|PMT-TEST-0223 |stobject equivalent align              |
|PMT-TEST-0224 |stobject equivalent notes              |
|PMT-TEST-0225 |stobject equivalent rename             |
|PMT-TEST-0227 |stobject equivalent blank              |
|PMT-TEST-0228 |stobject equivalent clear_reps         |
|PMT-TEST-0229 |stobject equivalent clear_grouped_reps |

<hr>

## User Story: PMT-S009 Add hlines based on data column

As a user, I want to be able add horizontal lines in the table based on non-repeating values of a data frame column.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name             |
|:-------------|:---------------------|
|PMT-TEST-0115 |test-hline-hline-from |

<hr>

## User Story: PMT-S010 Add hlines anywhere

As a user, I want to be able to add horizontal lines in a table at arbitrary locations.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name           |
|:-------------|:-------------------|
|PMT-TEST-0114 |test-hline-hline-at |

<hr>

## User Story: PMT-S011 Summary rows

As a user, I want to be able to identify a row (or rows) that are summary rows and add special styling to those rows, including a horizontal line above that row, bold font face in certain cells of that row, and alternate text in certain cells of that row.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name   |
|:-------------|:-----------|
|PMT-TEST-0199 |summary row |

<hr>

## User Story: PMT-S012 Annotate the table with user-defined notes

As a user, I want to be able to add notes that will get printed below a table.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name    |
|:-------------|:------------|
|PMT-TEST-0154 |mini notes   |
|PMT-TEST-0153 |tpt notes    |
|PMT-TEST-0155 |notes escape |
|PMT-TEST-0088 |notes-tpt    |
|PMT-TEST-0089 |notes-mini   |

<hr>

## User Story: PMT-S013 Annotate the table with R file name and output file name

As a user, I want to be able to pass in the name of originating R script and output latex file and add those annotations at the bottom of the table.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name        |
|:-------------|:----------------|
|PMT-TEST-0156 |test-notes-files |

<hr>

## User Story: PMT-S014 Bold column names

As a user, I want to be able to render table column names in bold font if I so choose.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name     |
|:-------------|:-------------|
|PMT-TEST-0204 |cols are bold |

<hr>

## User Story: PMT-S015 Replace column names

As a user, I want to be able to (completely) replace input data frame column names with a new set of names.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name         |
|:-------------|:-----------------|
|PMT-TEST-0203 |cols are replaced |

<hr>

## User Story: PMT-S016 Continuous covariate summary table

As a user, I want to be able to create a table summarizing continuous covariates in long or wide format.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                                |
|:-------------|:----------------------------------------|
|PMT-TEST-0038 |demo-check wide continuous panel         |
|PMT-TEST-0039 |demo-check wide continuous grouped       |
|PMT-TEST-0040 |demo-check wide continuous grouped panel |
|PMT-TEST-0041 |demo-check long continuous               |
|PMT-TEST-0042 |demo-check long continuous panel         |

<hr>

## User Story: PMT-S017 Categorical (discrete) covariate summary table

As a user, I want to be able to create a categorical covariate summary table in either wide or long format.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                           |
|:-------------|:-----------------------------------|
|PMT-TEST-0073 |discrete data table - long          |
|PMT-TEST-0075 |discrete data table - wide          |
|PMT-TEST-0065 |discrete data summary long - simple |
|PMT-TEST-0066 |discrete data summary long - by     |
|PMT-TEST-0067 |discrete data summary wide - simple |
|PMT-TEST-0068 |discrete data summary wide - by     |
|PMT-TEST-0109 |cat-long-span                       |
|PMT-TEST-0110 |cat-wide-by-panel                   |

<hr>

## User Story: PMT-S018 Data inventory table

As a user, I want to be able to summarize a data set and create a data disposition table summarizing the number of observations, the number of individuals, the number of missing observations, the number of observations below the quantitation limit, the percent of observations that are below the quantitation limit, and the percent of observation in certain subgroups.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                                 |
|:-------------|:-----------------------------------------|
|PMT-TEST-0122 |inventory table                           |
|PMT-TEST-0123 |inventory table grouped paneled           |
|PMT-TEST-0124 |inventory table - stacked                 |
|PMT-TEST-0125 |inventory table - different BQL cols      |
|PMT-TEST-0126 |inventory table - no bq col               |
|PMT-TEST-0119 |inventory data summary                    |
|PMT-TEST-0120 |stacked inventory data summary            |
|PMT-TEST-0121 |missing columns                           |
|PMT-TEST-0026 |demo-check data inventory stacked         |
|PMT-TEST-0027 |demo-check paneled data inventory stacked |
|PMT-TEST-0028 |demo-check grouped data inventory         |

<hr>

## User Story: PMT-S019 Column spanner - from user

As a user, I want to group table columns with a horizontal line above the table column labels and give a title to the group of columns.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name      |
|:-------------|:--------------|
|PMT-TEST-0189 |span from user |
|PMT-TEST-0102 |span           |

<hr>

## User Story: PMT-S020 Column spanner - from column names

As a user, I want to be able to encode column groupings in the column names as `title.name` format; the column groupings will be implemented by stating the separator on which to split the column name.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name             |
|:-------------|:---------------------|
|PMT-TEST-0186 |span split            |
|PMT-TEST-0160 |span split with title |

<hr>

## User Story: PMT-S021 Column alignment and sizing

As a user, I want to be able to alter the alignment of table columns as left, center, right with the column width determined by latex or as a fixed size.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name     |
|:-------------|:-------------|
|PMT-TEST-0001 |align helpers |
|PMT-TEST-0097 |align         |

<hr>

## User Story: PMT-S022 Set table row and column padding

As a user, I want to be able to increase or decrease the amount of padding between cells and / or columns in the table.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name    |
|:-------------|:------------|
|PMT-TEST-0104 |row-space    |
|PMT-TEST-0105 |col-space    |
|PMT-TEST-0106 |header-space |

<hr>

## User Story: PMT-S023 Add units below column labels

As a user, I want to pass in a named list of units corresponding to table column names and print the unit below the column name.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name             |
|:-------------|:---------------------|
|PMT-TEST-0206 |units                 |
|PMT-TEST-0101 |col-multi-line-units  |
|PMT-TEST-0107 |continuous-long-panel |
|PMT-TEST-0108 |continuous-wide-by    |

<hr>

## User Story: PMT-S024 Replace repeating values with whitespace in a column

As a user, I want to be able to replace repeating values with whitespace in a column.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                    |
|:-------------|:----------------------------|
|PMT-TEST-0010 |clear replicates             |
|PMT-TEST-0011 |clear grouped replicates - 1 |
|PMT-TEST-0012 |clear grouped replicates - 2 |

<hr>

## User Story: PMT-S025 Write math expressions in a table

As a user, I want to be able to write math expressions in latex in the table.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name           |
|:-------------|:-------------------|
|PMT-TEST-0177 |tab-escape          |
|PMT-TEST-0179 |units are sanitized |

<hr>

## User Story: PMT-S026 Tabular / three part table

As a user, I want to render a data frame in tabular environment within threeparttable.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name   |
|:-------------|:-----------|
|PMT-TEST-0085 |basic-table |

<hr>

## User Story: PMT-S027 Rename columns

As a user, I want to be able to have a column name appear in the latex table that is different than the column name in the data frame.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                          |
|:-------------|:----------------------------------|
|PMT-TEST-0202 |cols are renamed                   |
|PMT-TEST-0201 |cols are not renamed when no match |

<hr>

## User Story: PMT-S028 longtable captions

As a user, I want to be able to specify the caption for long tables as either text passed to the `stable_long` call or as a latex macro name.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                 |
|:-------------|:-------------------------|
|PMT-TEST-0135 |longtable - caption text  |
|PMT-TEST-0137 |longtable - caption macro |

<hr>

## User Story: PMT-S029 Break column labels into multiple lines

As a user, I want to be able to break the table column labels into multiple rows.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name        |
|:-------------|:----------------|
|PMT-TEST-0207 |col title breaks |

<hr>

## User Story: PMT-S030 Drop columns

As a user, I want to be able to optionally drop columns from the data frame so that they do not appear in the latex output.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name         |
|:-------------|:-----------------|
|PMT-TEST-0208 |column is dropped |

<hr>

## User Story: PMT-S031 ENHANCE: Add n column to pt_cat_wide

As a user, I want to have the total number of subjects in a column in the `pt_cat_wide` table



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                  |
|:-------------|:--------------------------|
|PMT-TEST-0076 |discrete - wide, summaries |

<hr>

## User Story: PMT-S032 ENHANCE: Put all data summary in pt_cat_long

As a user, I want to have an all-data summary at the bottom of pt_cat_long



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                               |
|:-------------|:---------------------------------------|
|PMT-TEST-0074 |discrete - long, summaries              |
|PMT-TEST-0034 |demo-check long categorical - n         |
|PMT-TEST-0036 |demo-check long categorical grouped - n |

<hr>

## User Story: PMT-S033 ENHANCE: add long = TRUE to as_stable

As a user, I want to be able to create long table from pmtable object.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                   |
|:-------------|:---------------------------|
|PMT-TEST-0167 |test-pmtable as_stable long |

<hr>

## User Story: PMT-S034 ENHANCE: invert pt_cont_long table

As a user, I want to be able to make a continuous covariate summary in long format, where the covariate summaries are paneled by the covariate name, rather than the paneling variable name



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name               |
|:-------------|:-----------------------|
|PMT-TEST-0018 |invert panel and cols   |
|PMT-TEST-0239 |table-utils paste units |

<hr>

## User Story: PMT-S035 BUG: passing in all_name with tex in it

As a user, I want tab_panel to succeed even if `prefix_skip` or `skip` is not a valid regular expression



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                            |
|:-------------|:------------------------------------|
|PMT-TEST-0243 |check if regular expression is valid |

<hr>

## User Story: PMT-S036 Refactor longtable and tabular row spacing

As a user, I want to be able to control row and column spacing in longtable and tabular with the same parameters.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                      |
|:-------------|:------------------------------|
|PMT-TEST-0138 |longtable - row spacing is set |
|PMT-TEST-0185 |test-sizes-rowspace            |

<hr>

## User Story: PMT-S037 BUG: output file name not saved to longtable output

As a user, I want longtable output object to retain output file name as an attribute.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                        |
|:-------------|:--------------------------------|
|PMT-TEST-0139 |longtable - output file is saved |

<hr>

## User Story: PMT-S038 Harmonize N / n in different tables

As a user, I want to see either N or n in table output



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                     |
|:-------------|:-----------------------------|
|PMT-TEST-0109 |cat-long-span                 |
|PMT-TEST-0110 |cat-wide-by-panel             |
|PMT-TEST-0024 |cont long table has n         |
|PMT-TEST-0079 |cat wide table has n          |
|PMT-TEST-0080 |cat long table has cols_extra |

<hr>

## User Story: PMT-S039 Wrap standard table notes in function call

As a user, I want to be able to get the notes for a data summary table using an R function call.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name         |
|:-------------|:-----------------|
|PMT-TEST-0022 |notes - cont-wide |
|PMT-TEST-0023 |notes - cont-long |
|PMT-TEST-0077 |notes - cat-wide  |
|PMT-TEST-0078 |notes - cat-long  |
|PMT-TEST-0127 |notes - inventory |

<hr>

## User Story: PMT-S040 Pass extra column title information as data frame

As a user, I want to be able to pass extra table labels to be included in the header as a data frame



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name           |
|:-------------|:-------------------|
|PMT-TEST-0210 |tab-cols cols_extra |

<hr>

## User Story: PMT-S041 con defaults to NULL for st_wrap in as_stable.pmtable

As a user, I want the table printed only once when calling `as_stable` with `wrapw = TRUE`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                        |
|:-------------|:--------------------------------|
|PMT-TEST-0002 |pass con to st_wrap in as_stable |

<hr>

## User Story: PMT-S042 Landscape mode for st2doc and st2article

As a user, I want to be able to preview a table in a pdf document in landscape mode.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                       |
|:-------------|:-------------------------------|
|PMT-TEST-0169 |wrap stable output in landscape |

<hr>

## User Story: PMT-S043 Short caption / title for longtable

As a user, I want to be able to specify both a short and long caption for long table



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                      |
|:-------------|:------------------------------|
|PMT-TEST-0136 |longtable - caption with short |

<hr>

## User Story: PMT-S044 Error in continuous summary when all values are missing

As a user, I want to be able to summarize continuous data items and have a result even if all values are missing.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name              |
|:-------------|:----------------------|
|PMT-TEST-0025 |cont table all missing |

<hr>

## User Story: PMT-S045 Need to accumulate hline indices in stobject

As a user, I want to call st_hline multiple times in a pipeline and accumulate hline indices



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                      |
|:-------------|:------------------------------|
|PMT-TEST-0118 |test-hline st_hline accumulate |

<hr>

## User Story: PMT-S046 st_hline with pattern argument doesn't work when cols is used?

As a user, I want to place a horizontal line across a table by specifying a pattern and a column name to in which to look



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                   |
|:-------------|:---------------------------|
|PMT-TEST-0116 |test-hline st_hline pattern |

<hr>

## User Story: PMT-S047 Add nudge argument to result from pattern arguments

As a user, I want to move the hline resulting from `st_hline / pattern` or `st_hline / at` up or back by a specified number of rows



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                 |
|:-------------|:-------------------------|
|PMT-TEST-0117 |test-hline st_hline nudge |

<hr>

## User Story: PMT-S048 Make columns bold after sanitizing

As a user, I would like to have column name sanitized prior to adding bold styling



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                      |
|:-------------|:------------------------------|
|PMT-TEST-0205 |cols are bold after sanitizing |

<hr>

## User Story: PMT-S049 Break span title

As a user, I want to be able to break span titles over multiple lines



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                 |
|:-------------|:-------------------------|
|PMT-TEST-0190 |span with breaks in title |

<hr>

## User Story: PMT-S050 Knit document with tables outside of template

As a user, I want to be able to include all required latex packages when knitting a document that includes pmtables output



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                       |
|:-------------|:-------------------------------|
|PMT-TEST-0171 |st-wrap table placement H       |
|PMT-TEST-0170 |use latex dependencies for knit |

<hr>

## User Story: PMT-S051 Option to bind yaml_as_df by column number

As a user, I want to write table data into a yaml file and have pmtables read and create a data frame based on column number rather than name



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                |
|:-------------|:------------------------|
|PMT-TEST-0246 |read simple table        |
|PMT-TEST-0247 |read study summary table |
|PMT-TEST-0248 |read prototyped table    |

<hr>

## User Story: PMT-S052 column spanners not showing up in longtable

As a user, I want column spanners to appear in long tables



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                       |
|:-------------|:-------------------------------|
|PMT-TEST-0140 |longtable - with span           |
|PMT-TEST-0141 |longtable - with units          |
|PMT-TEST-0142 |longtable - with span and units |

<hr>

## User Story: PMT-S053 Check for missing values in categorical covariates

As a user, I want pmtables to tell me when there is missing values in categorical covariate columns when creating categorical pmtable and replace with character NA.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                    |
|:-------------|:----------------------------|
|PMT-TEST-0082 |cat table with missing value |

<hr>

## User Story: PMT-S054 col_split is changing the column names too early

As a user, I want to be able to specify column spanners via both `span_split` and `span` and have `span_split` not clobber column names before calculating columns under `span`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                                         |
|:-------------|:-------------------------------------------------|
|PMT-TEST-0191 |names are not clobbered with span plus span_split |

<hr>

## User Story: PMT-S055 Add span_split via st_span

As a user, I want to specify `span_split` via `st_span()`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                  |
|:-------------|:--------------------------|
|PMT-TEST-0192 |add span_split via st_span |

<hr>

## User Story: PMT-S056 Make stable_long generic, add methods for pmtable and stobject

As a user, I want to be able to use `stable_long()` to render as long table objects with class `pmtable` and `stobject`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                       |
|:-------------|:-------------------------------|
|PMT-TEST-0143 |render long table from pmtable  |
|PMT-TEST-0144 |render long table from stobject |

<hr>

## User Story: PMT-S057 Return invisible after saving or previewing

As a user, I want pmtables to return invisible output after previewing a table or saving it to file.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                    |
|:-------------|:----------------------------|
|PMT-TEST-0237 |test-table-utils-stable_save |

<hr>

## User Story: PMT-S058 Bug in drop_miss implementation

As a user, I want `pt_data_inventory` to drop the Missing Observation summary when I pass `drop_miss = TRUE`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name        |
|:-------------|:----------------|
|PMT-TEST-0128 |drop MISS column |

<hr>

## User Story: PMT-S059 Combined demographics table

As a user, I pmtables to generate a table summarizing both continuous and categorical covariates.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                                                |
|:-------------|:--------------------------------------------------------|
|PMT-TEST-0043 |pt_demographics - call with span and summary             |
|PMT-TEST-0044 |pt_demographics - call with span, no summary             |
|PMT-TEST-0045 |pt_demographics - call with summary, no span             |
|PMT-TEST-0046 |demographics data summary - summary function             |
|PMT-TEST-0047 |handle numeric values from cont summary function         |
|PMT-TEST-0048 |demographics data summary - summary function errors      |
|PMT-TEST-0049 |demographics data summary - units                        |
|PMT-TEST-0050 |demographics data summary - column renaming (no units)   |
|PMT-TEST-0051 |demographics data summary - column renaming (with units) |
|PMT-TEST-0052 |demographics data summary - spot check values            |
|PMT-TEST-0053 |statistic column gets renamed                            |
|PMT-TEST-0054 |all data column gets renamed                             |
|PMT-TEST-0055 |paneled or unpaneled output                              |
|PMT-TEST-0056 |add notes to the output                                  |
|PMT-TEST-0057 |set width of Statistic column                            |

<hr>

## User Story: PMT-S060 Rename table columns via list

As a user, I want to pass a list of rename information to pmtables in order to rename columns.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name            |
|:-------------|:--------------------|
|PMT-TEST-0226 |rename by named list |

<hr>

## User Story: PMT-S061 Save a list of tables

As a user, I want `stable_save` to save out a list of `stable` objects



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name             |
|:-------------|:---------------------|
|PMT-TEST-0238 |save a list of tables |

<hr>

## User Story: PMT-S062 Pipe a list of tables to st2report

As a user, I want to be able to pipe a list of tables to st2report as well as passing them via dots



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                          |
|:-------------|:----------------------------------|
|PMT-TEST-0173 |pass a list of tables to st2report |

<hr>

## User Story: PMT-S063 Fix error message when duplicate panels are found

As a user, I want pmtables to name the correct argument to use to over ride an error generated when duplicate panels are used



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name        |
|:-------------|:----------------|
|PMT-TEST-0158 |panel duplicates |

<hr>

## User Story: PMT-S064 Re-named cols and table arg do not rename

As a user, I want to used renamed cols argument or pass a table list to `pt_cont_wide`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                   |
|:-------------|:---------------------------|
|PMT-TEST-0020 |cont wide with renamed cols |
|PMT-TEST-0021 |cont long with renamed cols |
|PMT-TEST-0083 |cat wide with renamed cols  |
|PMT-TEST-0084 |cat long with renamed cols  |

<hr>

## User Story: PMT-S065 Arguments are not captured when coercing pmtable output to stable_long

As a user, I want to be able to convert pmtable output to long table and pass along arguments to stable_long



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                                       |
|:-------------|:-----------------------------------------------|
|PMT-TEST-0003 |pass args to stable_long when coercing stobject |
|PMT-TEST-0004 |pass args to stable when coercing stobject      |
|PMT-TEST-0005 |pass args to stable_long when coercing pmtable  |
|PMT-TEST-0006 |pass args to stable when coercing pmtable       |

<hr>

## User Story: PMT-S066 Font size not changed for longtable

As a user, I want to control the base font size for longtable



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                  |
|:-------------|:--------------------------|
|PMT-TEST-0145 |set font size in longtable |

<hr>

## User Story: PMT-S067 Check incoming type for st_asis

As a user, I want to receive an error message when the wrong type of object is passed to `st_asis()`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                                  |
|:-------------|:------------------------------------------|
|PMT-TEST-0175 |error to call st_asis on non-stable object |

<hr>

## User Story: PMT-S068 Option to omit column names totally (including hline)

As a user, I would like to be able to completely suppress column names from appearing in the table.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                                |
|:-------------|:----------------------------------------|
|PMT-TEST-0211 |cols_omit drops column names - stable    |
|PMT-TEST-0212 |cols_omit drops column names - longtable |
|PMT-TEST-0213 |cols_omit drops units                    |
|PMT-TEST-0214 |cols_omit keeps span data                |

<hr>

## User Story: PMT-S069 Option to omit hline in panel

As a user, I want the option to omit the hline in the panel



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name             |
|:-------------|:---------------------|
|PMT-TEST-0164 |omit hline from panel |

<hr>

## User Story: PMT-S070 Use total non-missing obs as denom for percent bql

As a user, I want `pt_data_inventory` to use number of total non-missing observations in denominator for percent bql calculation.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                               |
|:-------------|:---------------------------------------|
|PMT-TEST-0129 |inventory table - denominator           |
|PMT-TEST-0130 |inventory table - bql                   |
|PMT-TEST-0131 |inventory table - obs                   |
|PMT-TEST-0132 |inventory table - missing / non-missing |

<hr>

## User Story: PMT-S071 Reverse order on span_split

As a user, I want to be able to take the span title from the left or right split when using `colsplit()`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                            |
|:-------------|:------------------------------------|
|PMT-TEST-0187 |span split with reversed title / col |

<hr>

## User Story: PMT-S072 Escape names in table list for st2article

As a user, I want pmtables to escape names in the list of tables to render with st2report



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                          |
|:-------------|:----------------------------------|
|PMT-TEST-0176 |st2report - list names are escaped |

<hr>

## User Story: PMT-S074 Jut the first column and header when panel

As a user, I want to be able to add small indent to the first column when panel is used.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                 |
|:-------------|:-------------------------|
|PMT-TEST-0166 |jut de-indents panel rows |

<hr>

## User Story: PMT-S075 Add * to panel title rows in longtable

As a user, I want pmtables to keep panel title rows together with the subsequent row.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                           |
|:-------------|:-----------------------------------|
|PMT-TEST-0165 |nopagebreak for panels in longtable |

<hr>

## User Story: PMT-S076 pt_data_inventory to update colname/footer when I use BLQ

As a user, I want pt_data_inventory to change my colname/footer description for BLQ if I use `bq_col = "BLQ"`



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                          |
|:-------------|:----------------------------------|
|PMT-TEST-0133 |handle BQL and BLQ inventory table |

<hr>

## User Story: PMT-S077 Allow alignment of span titles

As a user, I want to justify span titles to the left, right or center.



**Product risk**: 
low-risk




**Tests**


|Test ID       |Test name                    |
|:-------------|:----------------------------|
|PMT-TEST-0193 |align spanner - standard     |
|PMT-TEST-0195 |align spanner - via colsplit |
|PMT-TEST-0194 |align spanner - multiple     |

<hr>

