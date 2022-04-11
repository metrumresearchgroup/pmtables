---
title: ""
output:
  word_document: default
  pdf_document:
    latex_engine: xelatex
number_sections: yes
---

# Validation Testing: pmtables 0.4.1.9003

## Scope

The purpose of this Validation Testing document is to define the conditions for
test execution and present the test results. All tests are specified and linked
to release candidate user stories as numbered issues in the Requirements
Specification document.
 

# Automated Test Results

### pmtables-tests

**Date Run:** 2022-04-11 14:25:42

**Executor:** barrettk

**METWORX_VERSION:** 21.08

**COMMIT_HASH:** 7414d76186cc49f5e3bb412cbbd947eeedc04060

|Test ID       |Test name                                                | passed| failed|
|:-------------|:--------------------------------------------------------|------:|------:|
|PMT-TEST-0001 |align helpers                                            |      4|      0|
|PMT-TEST-0002 |pass con to st_wrap in as_stable                         |      4|      0|
|PMT-TEST-0003 |pass args to stable_long when coercing stobject          |      2|      0|
|PMT-TEST-0004 |pass args to stable when coercing stobject               |      1|      0|
|PMT-TEST-0005 |pass args to stable_long when coercing pmtable           |      2|      0|
|PMT-TEST-0006 |pass args to stable when coercing pmtable                |      1|      0|
|PMT-TEST-0010 |clear replicates                                         |      1|      0|
|PMT-TEST-0011 |clear grouped replicates - 1                             |      2|      0|
|PMT-TEST-0012 |clear grouped replicates - 2                             |      2|      0|
|PMT-TEST-0018 |invert panel and cols                                    |     10|      0|
|PMT-TEST-0020 |cont wide with renamed cols                              |      3|      0|
|PMT-TEST-0021 |cont long with renamed cols                              |      3|      0|
|PMT-TEST-0022 |notes - cont-wide                                        |      3|      0|
|PMT-TEST-0023 |notes - cont-long                                        |      5|      0|
|PMT-TEST-0024 |cont long table has n                                    |      1|      0|
|PMT-TEST-0025 |cont table all missing                                   |     17|      0|
|PMT-TEST-0026 |demo-check data inventory stacked                        |      6|      0|
|PMT-TEST-0027 |demo-check paneled data inventory stacked                |      8|      0|
|PMT-TEST-0028 |demo-check grouped data inventory                        |      6|      0|
|PMT-TEST-0034 |demo-check long categorical - n                          |      1|      0|
|PMT-TEST-0036 |demo-check long categorical grouped - n                  |      4|      0|
|PMT-TEST-0038 |demo-check wide continuous panel                         |      6|      0|
|PMT-TEST-0039 |demo-check wide continuous grouped                       |      6|      0|
|PMT-TEST-0040 |demo-check wide continuous grouped panel                 |      7|      0|
|PMT-TEST-0041 |demo-check long continuous                               |      6|      0|
|PMT-TEST-0042 |demo-check long continuous panel                         |      7|      0|
|PMT-TEST-0043 |pt_demographics - call with span and summary             |      4|      0|
|PMT-TEST-0044 |pt_demographics - call with span, no summary             |      2|      0|
|PMT-TEST-0045 |pt_demographics - call with summary, no span             |      2|      0|
|PMT-TEST-0046 |demographics data summary - summary function             |      2|      0|
|PMT-TEST-0047 |handle numeric values from cont summary function         |      2|      0|
|PMT-TEST-0048 |demographics data summary - summary function errors      |      2|      0|
|PMT-TEST-0049 |demographics data summary - units                        |      1|      0|
|PMT-TEST-0050 |demographics data summary - column renaming (no units)   |      1|      0|
|PMT-TEST-0051 |demographics data summary - column renaming (with units) |      1|      0|
|PMT-TEST-0052 |demographics data summary - spot check values            |      2|      0|
|PMT-TEST-0053 |statistic column gets renamed                            |      1|      0|
|PMT-TEST-0054 |all data column gets renamed                             |      2|      0|
|PMT-TEST-0055 |paneled or unpaneled output                              |      4|      0|
|PMT-TEST-0056 |add notes to the output                                  |      3|      0|
|PMT-TEST-0057 |set width of Statistic column                            |      1|      0|
|PMT-TEST-0065 |discrete data summary long - simple                      |      5|      0|
|PMT-TEST-0066 |discrete data summary long - by                          |      5|      0|
|PMT-TEST-0067 |discrete data summary wide - simple                      |      4|      0|
|PMT-TEST-0068 |discrete data summary wide - by                          |      4|      0|
|PMT-TEST-0073 |discrete data table - long                               |      1|      0|
|PMT-TEST-0074 |discrete - long, summaries                               |      7|      0|
|PMT-TEST-0075 |discrete data table - wide                               |      1|      0|
|PMT-TEST-0076 |discrete - wide, summaries                               |      3|      0|
|PMT-TEST-0077 |notes - cat-wide                                         |      4|      0|
|PMT-TEST-0078 |notes - cat-long                                         |      4|      0|
|PMT-TEST-0079 |cat wide table has n                                     |      1|      0|
|PMT-TEST-0080 |cat long table has cols_extra                            |      4|      0|
|PMT-TEST-0082 |cat table with missing value                             |      2|      0|
|PMT-TEST-0083 |cat wide with renamed cols                               |      3|      0|
|PMT-TEST-0084 |cat long with renamed cols                               |      3|      0|
|PMT-TEST-0085 |basic-table                                              |      1|      0|
|PMT-TEST-0088 |notes-tpt                                                |      1|      0|
|PMT-TEST-0089 |notes-mini                                               |      1|      0|
|PMT-TEST-0097 |align                                                    |      1|      0|
|PMT-TEST-0101 |col-multi-line-units                                     |      1|      0|
|PMT-TEST-0102 |span                                                     |      1|      0|
|PMT-TEST-0104 |row-space                                                |      1|      0|
|PMT-TEST-0105 |col-space                                                |      1|      0|
|PMT-TEST-0106 |header-space                                             |      1|      0|
|PMT-TEST-0107 |continuous-long-panel                                    |      1|      0|
|PMT-TEST-0108 |continuous-wide-by                                       |      1|      0|
|PMT-TEST-0109 |cat-long-span                                            |      1|      0|
|PMT-TEST-0110 |cat-wide-by-panel                                        |      1|      0|
|PMT-TEST-0114 |test-hline-hline-at                                      |      1|      0|
|PMT-TEST-0115 |test-hline-hline-from                                    |      2|      0|
|PMT-TEST-0116 |test-hline st_hline pattern                              |      1|      0|
|PMT-TEST-0117 |test-hline st_hline nudge                                |      4|      0|
|PMT-TEST-0118 |test-hline st_hline accumulate                           |      1|      0|
|PMT-TEST-0119 |inventory data summary                                   |      5|      0|
|PMT-TEST-0120 |stacked inventory data summary                           |      4|      0|
|PMT-TEST-0121 |missing columns                                          |      4|      0|
|PMT-TEST-0122 |inventory table                                          |      9|      0|
|PMT-TEST-0123 |inventory table grouped paneled                          |     13|      0|
|PMT-TEST-0124 |inventory table - stacked                                |     10|      0|
|PMT-TEST-0125 |inventory table - different BQL cols                     |      1|      0|
|PMT-TEST-0126 |inventory table - no bq col                              |      1|      0|
|PMT-TEST-0127 |notes - inventory                                        |      6|      0|
|PMT-TEST-0128 |drop MISS column                                         |      2|      0|
|PMT-TEST-0129 |inventory table - denominator                            |      4|      0|
|PMT-TEST-0130 |inventory table - bql                                    |      2|      0|
|PMT-TEST-0131 |inventory table - obs                                    |      1|      0|
|PMT-TEST-0132 |inventory table - missing / non-missing                  |      2|      0|
|PMT-TEST-0133 |handle BQL and BLQ inventory table                       |     11|      0|
|PMT-TEST-0134 |test-longtable stable_long                               |      1|      0|
|PMT-TEST-0135 |longtable - caption text                                 |      1|      0|
|PMT-TEST-0136 |longtable - caption with short                           |      1|      0|
|PMT-TEST-0137 |longtable - caption macro                                |      3|      0|
|PMT-TEST-0138 |longtable - row spacing is set                           |      3|      0|
|PMT-TEST-0139 |longtable - output file is saved                         |      1|      0|
|PMT-TEST-0140 |longtable - with span                                    |      1|      0|
|PMT-TEST-0141 |longtable - with units                                   |      2|      0|
|PMT-TEST-0142 |longtable - with span and units                          |      2|      0|
|PMT-TEST-0143 |render long table from pmtable                           |      2|      0|
|PMT-TEST-0144 |render long table from stobject                          |      2|      0|
|PMT-TEST-0145 |set font size in longtable                               |      2|      0|
|PMT-TEST-0153 |tpt notes                                                |      5|      0|
|PMT-TEST-0154 |mini notes                                               |      8|      0|
|PMT-TEST-0155 |notes escape                                             |      1|      0|
|PMT-TEST-0156 |test-notes-files                                         |      5|      0|
|PMT-TEST-0158 |panel duplicates                                         |      2|      0|
|PMT-TEST-0160 |span split with title                                    |      4|      0|
|PMT-TEST-0164 |omit hline from panel                                    |      2|      0|
|PMT-TEST-0165 |nopagebreak for panels in longtable                      |      3|      0|
|PMT-TEST-0166 |jut de-indents panel rows                                |      4|      0|
|PMT-TEST-0167 |test-pmtable as_stable long                              |      3|      0|
|PMT-TEST-0169 |wrap stable output in landscape                          |      3|      0|
|PMT-TEST-0170 |use latex dependencies for knit                          |      4|      0|
|PMT-TEST-0171 |st-wrap table placement H                                |      2|      0|
|PMT-TEST-0173 |pass a list of tables to st2report                       |      4|      0|
|PMT-TEST-0175 |error to call st_asis on non-stable object               |      1|      0|
|PMT-TEST-0176 |st2report - list names are escaped                       |      1|      0|
|PMT-TEST-0177 |tab-escape                                               |      1|      0|
|PMT-TEST-0179 |units are sanitized                                      |      2|      0|
|PMT-TEST-0180 |files are sanitized                                      |      2|      0|
|PMT-TEST-0181 |notes are sanitized                                      |      3|      0|
|PMT-TEST-0183 |table contents are sanitized                             |      5|      0|
|PMT-TEST-0184 |test-sizes-fontsize                                      |      1|      0|
|PMT-TEST-0185 |test-sizes-rowspace                                      |      1|      0|
|PMT-TEST-0186 |span split                                               |      2|      0|
|PMT-TEST-0187 |span split with reversed title / col                     |      3|      0|
|PMT-TEST-0189 |span from user                                           |      2|      0|
|PMT-TEST-0190 |span with breaks in title                                |      6|      0|
|PMT-TEST-0191 |names are not clobbered with span plus span_split        |      3|      0|
|PMT-TEST-0192 |add span_split via st_span                               |      4|      0|
|PMT-TEST-0193 |align spanner - standard                                 |      3|      0|
|PMT-TEST-0194 |align spanner - multiple                                 |      3|      0|
|PMT-TEST-0195 |align spanner - via colsplit                             |      1|      0|
|PMT-TEST-0199 |summary row                                              |      2|      0|
|PMT-TEST-0201 |cols are not renamed when no match                       |      2|      0|
|PMT-TEST-0202 |cols are renamed                                         |      1|      0|
|PMT-TEST-0203 |cols are replaced                                        |      2|      0|
|PMT-TEST-0204 |cols are bold                                            |      2|      0|
|PMT-TEST-0205 |cols are bold after sanitizing                           |      1|      0|
|PMT-TEST-0206 |units                                                    |      2|      0|
|PMT-TEST-0207 |col title breaks                                         |      7|      0|
|PMT-TEST-0208 |column is dropped                                        |      1|      0|
|PMT-TEST-0209 |de-tag column labels                                     |      1|      0|
|PMT-TEST-0210 |tab-cols cols_extra                                      |      4|      0|
|PMT-TEST-0211 |cols_omit drops column names - stable                    |      5|      0|
|PMT-TEST-0212 |cols_omit drops column names - longtable                 |      4|      0|
|PMT-TEST-0213 |cols_omit drops units                                    |      2|      0|
|PMT-TEST-0214 |cols_omit keeps span data                                |      2|      0|
|PMT-TEST-0215 |stobject equivalent hline                                |      2|      0|
|PMT-TEST-0217 |stobject equivalent cols_bold                            |      1|      0|
|PMT-TEST-0218 |stobject equivalent panel                                |      1|      0|
|PMT-TEST-0219 |stobject equivalent sumrow                               |      1|      0|
|PMT-TEST-0220 |stobject equivalent span                                 |      1|      0|
|PMT-TEST-0221 |stobject equivalent files                                |      1|      0|
|PMT-TEST-0222 |stobject equivalent drop                                 |      1|      0|
|PMT-TEST-0223 |stobject equivalent align                                |      1|      0|
|PMT-TEST-0224 |stobject equivalent notes                                |      2|      0|
|PMT-TEST-0225 |stobject equivalent rename                               |      1|      0|
|PMT-TEST-0226 |rename by named list                                     |      5|      0|
|PMT-TEST-0227 |stobject equivalent blank                                |      1|      0|
|PMT-TEST-0228 |stobject equivalent clear_reps                           |      1|      0|
|PMT-TEST-0229 |stobject equivalent clear_grouped_reps                   |      1|      0|
|PMT-TEST-0230 |stobject equivalent longtable                            |      1|      0|
|PMT-TEST-0237 |test-table-utils-stable_save                             |      7|      0|
|PMT-TEST-0238 |save a list of tables                                    |      1|      0|
|PMT-TEST-0239 |table-utils paste units                                  |      1|      0|
|PMT-TEST-0243 |check if regular expression is valid                     |      6|      0|
|PMT-TEST-0246 |read simple table                                        |      3|      0|
|PMT-TEST-0247 |read study summary table                                 |      1|      0|
|PMT-TEST-0248 |read prototyped table                                    |      2|      0|
