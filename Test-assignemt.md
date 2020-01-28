Test statistical assignment
================
Jessica Ledger
25 January 2020

## Introduction

Please change the author and date fields above as appropriate. Do not
change the output format. Once you have completed the assignment you
want to knit your document into a markdown document in the
“github\_document” format and then commit both the .Rmd and .md files
(and all the associated files with graphs) to your private assignment
repository on Github.

## Reading data (40 points)

First, we need to read the data into R. For this assignment, I ask you
to use data from the youth self-completion questionnaire (completed by
children between 10 and 15 years old) from Wave 9 of the Understanding
Society. It is one of the files you have downloaded as part of SN6614
from the UK Data Service. To help you find and understand this file you
will need the following documents:

1)  The Understanding Society Waves 1-9 User Guide:
    <https://www.understandingsociety.ac.uk/sites/default/files/downloads/documentation/mainstage/user-guides/mainstage-user-guide.pdf>
2)  The youth self-completion questionnaire from Wave 9:
    <https://www.understandingsociety.ac.uk/sites/default/files/downloads/documentation/mainstage/questionnaire/wave-9/w9-gb-youth-self-completion-questionnaire.pdf>
3)  The codebook for the file:
    <https://www.understandingsociety.ac.uk/documentation/mainstage/dataset-documentation/datafile/youth/wave/9>

<!-- end list -->

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
    ## ✓ tibble  2.1.3     ✓ dplyr   0.8.3
    ## ✓ tidyr   1.0.0     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
# This attaches the tidyverse package. If you get an error here you need to install the package first. 
Data <- read_tsv("/Users/jessicaledger/Desktop/WORK/THIRD YEAR/Data Analysis III/data2020/data/UKDA-6614-tab/tab/ukhls_w9/i_youth.tab")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
# You need to add between the quotation marks a full path to the required file on your computer.
```

## Tabulate variables (10 points)

In the survey children were asked the following question: “Do you have a
social media profile or account on any sites or apps?”. In this
assignment we want to explore how the probability of having an account
on social media depends on children’s age and gender.

Tabulate three variables: children’s gender, age (please use derived
variables) and having an account on social media.

``` r
library(tidyverse)
# Children's gender
table(Data$i_sex) 
```

    ## 
    ##    1    2 
    ## 1411 1410

``` r
#   1    2 
# 1411 1410 

# Age
table(Data$i_ypdoby)
```

    ## 
    ## 2001 2002 2003 2004 2005 2006 2007 2008 2009 
    ##  111  334  483  484  479  465  355  109    1

``` r
# 2001 2002 2003 2004 2005 2006 2007 2008 2009 
#  111  334  483  484  479  465  355  109    1 

# Account on social media
table(Data$i_ypsocweb)
```

    ## 
    ##   -9    1    2 
    ##   14 2277  530

``` r
# -9    1    2 
# 14 2277  530 
```

## Recode variables (10 points)

We want to create a new binary variable for having an account on social
media so that 1 means “yes”, 0 means “no”, and all missing values are
coded as NA. We also want to recode gender into a new variable with the
values “male” and “female” (this can be a character vector or a factor).

``` r
# New binary variable for having an account on social media.

Data$i_ypsocweb[Data$i_ypsocweb == -9] <- NA
table(Data$i_ypsocweb)
```

    ## 
    ##    1    2 
    ## 2277  530

``` r
class(Data$i_ypsocweb)
```

    ## [1] "numeric"

``` r
# Recoding gender variable

Data$i_sex <- sample(c("1","2"), 2821, replace = TRUE)
recode(Data$i_sex, '1' = "male")
```

    ##    [1] "male" "2"    "2"    "2"    "male" "male" "2"    "male" "2"    "2"   
    ##   [11] "male" "male" "male" "2"    "male" "2"    "2"    "2"    "2"    "2"   
    ##   [21] "2"    "male" "2"    "2"    "2"    "male" "male" "2"    "male" "male"
    ##   [31] "2"    "2"    "male" "male" "male" "male" "male" "2"    "2"    "male"
    ##   [41] "male" "2"    "male" "2"    "2"    "2"    "2"    "2"    "male" "2"   
    ##   [51] "male" "male" "2"    "male" "2"    "male" "male" "male" "male" "male"
    ##   [61] "2"    "2"    "male" "2"    "male" "male" "male" "2"    "male" "2"   
    ##   [71] "2"    "male" "male" "male" "male" "2"    "2"    "male" "2"    "2"   
    ##   [81] "male" "2"    "male" "2"    "male" "male" "2"    "2"    "2"    "male"
    ##   [91] "2"    "male" "male" "2"    "male" "male" "2"    "male" "male" "male"
    ##  [101] "male" "2"    "2"    "male" "2"    "2"    "2"    "2"    "2"    "2"   
    ##  [111] "male" "male" "male" "2"    "2"    "male" "male" "male" "2"    "male"
    ##  [121] "male" "2"    "male" "2"    "2"    "2"    "male" "2"    "male" "male"
    ##  [131] "male" "2"    "male" "male" "2"    "2"    "2"    "male" "male" "2"   
    ##  [141] "2"    "male" "male" "2"    "2"    "2"    "2"    "2"    "2"    "2"   
    ##  [151] "male" "2"    "2"    "male" "2"    "2"    "2"    "male" "male" "male"
    ##  [161] "male" "male" "2"    "2"    "2"    "male" "2"    "male" "male" "male"
    ##  [171] "male" "male" "2"    "2"    "2"    "2"    "male" "2"    "male" "2"   
    ##  [181] "male" "2"    "male" "2"    "2"    "male" "male" "male" "male" "2"   
    ##  [191] "male" "2"    "2"    "2"    "2"    "2"    "male" "male" "male" "2"   
    ##  [201] "2"    "2"    "2"    "male" "2"    "male" "male" "male" "2"    "2"   
    ##  [211] "2"    "male" "male" "male" "male" "2"    "male" "2"    "2"    "male"
    ##  [221] "male" "male" "male" "male" "male" "male" "2"    "male" "male" "male"
    ##  [231] "2"    "male" "male" "male" "2"    "male" "male" "male" "2"    "2"   
    ##  [241] "2"    "2"    "male" "male" "2"    "2"    "male" "male" "2"    "male"
    ##  [251] "2"    "2"    "2"    "male" "2"    "male" "male" "2"    "male" "male"
    ##  [261] "2"    "male" "2"    "male" "male" "male" "2"    "male" "male" "2"   
    ##  [271] "2"    "male" "2"    "2"    "2"    "2"    "male" "male" "2"    "male"
    ##  [281] "2"    "male" "2"    "2"    "male" "2"    "male" "2"    "2"    "2"   
    ##  [291] "2"    "male" "2"    "2"    "male" "2"    "2"    "2"    "male" "2"   
    ##  [301] "2"    "2"    "2"    "male" "male" "2"    "male" "2"    "2"    "male"
    ##  [311] "male" "male" "male" "2"    "male" "male" "male" "male" "male" "male"
    ##  [321] "male" "male" "male" "2"    "male" "male" "2"    "2"    "male" "2"   
    ##  [331] "2"    "male" "male" "male" "male" "2"    "2"    "2"    "male" "male"
    ##  [341] "2"    "male" "male" "2"    "2"    "2"    "male" "male" "2"    "male"
    ##  [351] "2"    "male" "male" "2"    "2"    "male" "2"    "male" "male" "male"
    ##  [361] "2"    "male" "male" "male" "male" "2"    "2"    "2"    "2"    "2"   
    ##  [371] "2"    "2"    "2"    "2"    "male" "2"    "2"    "2"    "male" "male"
    ##  [381] "male" "male" "2"    "2"    "male" "male" "male" "male" "male" "male"
    ##  [391] "2"    "2"    "male" "male" "2"    "2"    "male" "2"    "male" "male"
    ##  [401] "male" "male" "male" "2"    "male" "2"    "male" "male" "male" "male"
    ##  [411] "male" "2"    "male" "2"    "male" "2"    "male" "male" "male" "2"   
    ##  [421] "male" "2"    "2"    "2"    "male" "male" "male" "2"    "male" "male"
    ##  [431] "2"    "2"    "2"    "2"    "male" "male" "2"    "male" "2"    "male"
    ##  [441] "male" "2"    "male" "male" "male" "2"    "2"    "male" "2"    "2"   
    ##  [451] "male" "male" "2"    "male" "2"    "2"    "male" "2"    "male" "male"
    ##  [461] "male" "2"    "male" "2"    "male" "male" "male" "male" "2"    "2"   
    ##  [471] "2"    "male" "2"    "male" "male" "male" "2"    "2"    "male" "male"
    ##  [481] "male" "male" "male" "2"    "male" "2"    "male" "male" "male" "male"
    ##  [491] "male" "male" "2"    "male" "2"    "male" "2"    "2"    "2"    "male"
    ##  [501] "2"    "male" "2"    "male" "male" "male" "2"    "2"    "2"    "male"
    ##  [511] "male" "male" "male" "2"    "male" "male" "2"    "2"    "male" "male"
    ##  [521] "2"    "2"    "male" "2"    "male" "male" "2"    "2"    "male" "male"
    ##  [531] "male" "male" "male" "2"    "male" "2"    "2"    "2"    "2"    "2"   
    ##  [541] "male" "2"    "male" "male" "male" "male" "2"    "2"    "male" "male"
    ##  [551] "2"    "male" "2"    "male" "2"    "male" "male" "male" "male" "2"   
    ##  [561] "2"    "2"    "2"    "2"    "2"    "2"    "male" "2"    "male" "male"
    ##  [571] "2"    "male" "2"    "2"    "male" "male" "2"    "2"    "male" "male"
    ##  [581] "2"    "2"    "2"    "2"    "2"    "2"    "male" "male" "2"    "male"
    ##  [591] "2"    "2"    "2"    "2"    "2"    "male" "male" "2"    "male" "2"   
    ##  [601] "2"    "2"    "male" "male" "male" "2"    "2"    "male" "2"    "2"   
    ##  [611] "male" "2"    "male" "male" "male" "2"    "male" "2"    "2"    "2"   
    ##  [621] "male" "male" "2"    "male" "2"    "male" "male" "male" "2"    "2"   
    ##  [631] "2"    "male" "2"    "male" "2"    "male" "male" "male" "male" "2"   
    ##  [641] "male" "male" "2"    "male" "2"    "2"    "male" "male" "2"    "2"   
    ##  [651] "2"    "male" "male" "male" "male" "2"    "male" "male" "2"    "2"   
    ##  [661] "male" "male" "male" "male" "2"    "2"    "2"    "2"    "2"    "male"
    ##  [671] "2"    "male" "male" "2"    "2"    "male" "male" "male" "2"    "2"   
    ##  [681] "male" "male" "2"    "male" "2"    "2"    "male" "2"    "2"    "2"   
    ##  [691] "male" "2"    "2"    "2"    "2"    "2"    "2"    "2"    "male" "2"   
    ##  [701] "male" "male" "male" "2"    "2"    "male" "male" "male" "male" "male"
    ##  [711] "2"    "2"    "male" "male" "2"    "male" "2"    "male" "2"    "male"
    ##  [721] "2"    "2"    "2"    "2"    "male" "2"    "2"    "2"    "male" "2"   
    ##  [731] "2"    "2"    "male" "male" "2"    "2"    "male" "2"    "2"    "2"   
    ##  [741] "2"    "2"    "2"    "male" "male" "male" "male" "male" "male" "male"
    ##  [751] "male" "2"    "male" "male" "2"    "male" "male" "2"    "2"    "male"
    ##  [761] "male" "2"    "2"    "2"    "male" "2"    "male" "male" "2"    "2"   
    ##  [771] "male" "2"    "male" "male" "male" "male" "2"    "male" "male" "2"   
    ##  [781] "2"    "2"    "2"    "male" "2"    "male" "2"    "male" "male" "2"   
    ##  [791] "male" "2"    "2"    "male" "male" "2"    "2"    "male" "2"    "2"   
    ##  [801] "male" "male" "2"    "2"    "2"    "2"    "male" "male" "male" "male"
    ##  [811] "2"    "male" "male" "2"    "male" "2"    "2"    "male" "2"    "2"   
    ##  [821] "male" "2"    "2"    "2"    "male" "male" "male" "2"    "male" "male"
    ##  [831] "2"    "2"    "male" "2"    "2"    "male" "male" "male" "male" "male"
    ##  [841] "2"    "2"    "2"    "male" "2"    "male" "2"    "2"    "2"    "male"
    ##  [851] "male" "2"    "2"    "male" "male" "2"    "male" "male" "male" "2"   
    ##  [861] "male" "2"    "male" "male" "2"    "2"    "male" "male" "2"    "male"
    ##  [871] "2"    "2"    "male" "male" "male" "2"    "male" "male" "male" "male"
    ##  [881] "male" "2"    "2"    "male" "male" "male" "2"    "male" "male" "male"
    ##  [891] "2"    "male" "2"    "male" "2"    "male" "male" "male" "2"    "2"   
    ##  [901] "male" "male" "2"    "2"    "male" "male" "male" "male" "2"    "2"   
    ##  [911] "2"    "male" "male" "male" "male" "male" "2"    "male" "2"    "2"   
    ##  [921] "male" "2"    "2"    "male" "male" "male" "2"    "male" "2"    "male"
    ##  [931] "male" "male" "male" "male" "male" "male" "male" "2"    "2"    "2"   
    ##  [941] "male" "male" "2"    "male" "2"    "2"    "male" "2"    "male" "male"
    ##  [951] "male" "2"    "2"    "2"    "2"    "2"    "male" "2"    "male" "2"   
    ##  [961] "male" "2"    "2"    "male" "2"    "male" "male" "2"    "2"    "2"   
    ##  [971] "2"    "male" "male" "male" "2"    "2"    "2"    "male" "2"    "2"   
    ##  [981] "2"    "male" "2"    "2"    "male" "male" "male" "2"    "2"    "2"   
    ##  [991] "male" "2"    "male" "male" "male" "2"    "2"    "2"    "2"    "2"   
    ## [1001] "male" "male" "male" "2"    "2"    "2"    "2"    "2"    "2"    "male"
    ## [1011] "male" "male" "male" "male" "male" "male" "2"    "male" "male" "male"
    ## [1021] "2"    "2"    "male" "male" "2"    "2"    "male" "male" "male" "2"   
    ## [1031] "male" "male" "2"    "male" "male" "2"    "male" "2"    "male" "male"
    ## [1041] "male" "male" "male" "male" "male" "male" "2"    "male" "2"    "male"
    ## [1051] "male" "2"    "2"    "male" "male" "2"    "2"    "2"    "2"    "2"   
    ## [1061] "2"    "male" "2"    "2"    "male" "male" "male" "male" "2"    "2"   
    ## [1071] "male" "male" "2"    "2"    "2"    "2"    "male" "2"    "male" "male"
    ## [1081] "2"    "male" "2"    "2"    "male" "male" "male" "male" "2"    "2"   
    ## [1091] "male" "male" "male" "2"    "male" "2"    "2"    "male" "2"    "male"
    ## [1101] "2"    "2"    "2"    "male" "2"    "male" "male" "2"    "male" "male"
    ## [1111] "2"    "male" "male" "2"    "2"    "male" "2"    "male" "male" "male"
    ## [1121] "2"    "2"    "male" "2"    "2"    "male" "2"    "male" "2"    "2"   
    ## [1131] "2"    "male" "male" "2"    "male" "2"    "male" "2"    "male" "2"   
    ## [1141] "male" "2"    "2"    "2"    "2"    "male" "2"    "male" "2"    "male"
    ## [1151] "male" "2"    "2"    "2"    "2"    "male" "male" "2"    "2"    "male"
    ## [1161] "2"    "male" "2"    "2"    "male" "male" "male" "2"    "2"    "2"   
    ## [1171] "male" "2"    "2"    "male" "male" "2"    "2"    "male" "male" "2"   
    ## [1181] "male" "male" "2"    "male" "male" "male" "2"    "2"    "male" "male"
    ## [1191] "2"    "2"    "male" "2"    "2"    "2"    "male" "2"    "2"    "male"
    ## [1201] "2"    "2"    "2"    "male" "2"    "2"    "2"    "2"    "male" "2"   
    ## [1211] "2"    "male" "2"    "male" "male" "2"    "male" "2"    "male" "male"
    ## [1221] "male" "male" "male" "2"    "2"    "male" "2"    "2"    "male" "2"   
    ## [1231] "male" "2"    "male" "2"    "2"    "male" "male" "male" "male" "2"   
    ## [1241] "2"    "2"    "2"    "2"    "2"    "2"    "male" "male" "2"    "male"
    ## [1251] "male" "male" "2"    "2"    "2"    "2"    "2"    "2"    "2"    "2"   
    ## [1261] "2"    "male" "male" "male" "2"    "male" "male" "male" "male" "2"   
    ## [1271] "2"    "2"    "male" "2"    "2"    "2"    "male" "2"    "male" "2"   
    ## [1281] "2"    "male" "2"    "2"    "male" "2"    "2"    "2"    "male" "2"   
    ## [1291] "male" "2"    "2"    "2"    "male" "male" "male" "male" "2"    "male"
    ## [1301] "2"    "male" "male" "2"    "male" "2"    "2"    "2"    "male" "male"
    ## [1311] "2"    "male" "2"    "male" "male" "2"    "male" "2"    "2"    "male"
    ## [1321] "2"    "2"    "male" "male" "male" "male" "male" "male" "male" "2"   
    ## [1331] "male" "2"    "male" "male" "2"    "male" "male" "male" "2"    "2"   
    ## [1341] "male" "2"    "male" "2"    "male" "2"    "male" "male" "2"    "2"   
    ## [1351] "male" "male" "male" "male" "2"    "male" "male" "2"    "male" "male"
    ## [1361] "male" "male" "2"    "2"    "male" "2"    "male" "male" "2"    "male"
    ## [1371] "2"    "male" "2"    "male" "male" "male" "male" "2"    "2"    "male"
    ## [1381] "2"    "male" "male" "male" "2"    "2"    "male" "male" "2"    "male"
    ## [1391] "male" "2"    "male" "2"    "2"    "2"    "male" "2"    "male" "2"   
    ## [1401] "2"    "2"    "2"    "2"    "2"    "male" "2"    "male" "2"    "male"
    ## [1411] "2"    "2"    "2"    "male" "2"    "2"    "male" "2"    "2"    "2"   
    ## [1421] "male" "2"    "2"    "2"    "2"    "2"    "male" "male" "male" "male"
    ## [1431] "2"    "2"    "male" "2"    "2"    "male" "2"    "male" "2"    "2"   
    ## [1441] "2"    "male" "male" "2"    "2"    "male" "male" "2"    "male" "2"   
    ## [1451] "male" "male" "2"    "2"    "2"    "2"    "2"    "2"    "male" "male"
    ## [1461] "2"    "male" "2"    "male" "2"    "2"    "male" "2"    "2"    "2"   
    ## [1471] "male" "2"    "male" "male" "2"    "male" "2"    "male" "male" "2"   
    ## [1481] "2"    "2"    "2"    "2"    "2"    "2"    "male" "male" "male" "2"   
    ## [1491] "2"    "male" "2"    "male" "2"    "male" "male" "male" "2"    "male"
    ## [1501] "male" "male" "2"    "male" "2"    "2"    "2"    "male" "male" "2"   
    ## [1511] "2"    "2"    "2"    "2"    "2"    "male" "male" "male" "male" "2"   
    ## [1521] "2"    "male" "2"    "male" "male" "male" "male" "male" "2"    "2"   
    ## [1531] "male" "2"    "male" "2"    "male" "male" "2"    "2"    "male" "2"   
    ## [1541] "male" "male" "male" "male" "2"    "2"    "2"    "2"    "2"    "2"   
    ## [1551] "2"    "2"    "2"    "male" "male" "2"    "2"    "2"    "2"    "2"   
    ## [1561] "2"    "male" "2"    "2"    "male" "male" "male" "male" "male" "2"   
    ## [1571] "male" "male" "male" "2"    "2"    "male" "male" "2"    "male" "male"
    ## [1581] "2"    "male" "2"    "2"    "2"    "male" "male" "2"    "male" "2"   
    ## [1591] "male" "2"    "2"    "2"    "2"    "2"    "male" "2"    "2"    "2"   
    ## [1601] "2"    "male" "male" "male" "2"    "male" "male" "male" "male" "2"   
    ## [1611] "male" "male" "2"    "2"    "male" "2"    "2"    "2"    "male" "2"   
    ## [1621] "male" "male" "male" "2"    "male" "male" "2"    "2"    "2"    "2"   
    ## [1631] "2"    "2"    "2"    "2"    "2"    "male" "male" "2"    "male" "2"   
    ## [1641] "2"    "2"    "male" "male" "male" "2"    "male" "2"    "2"    "male"
    ## [1651] "2"    "male" "male" "male" "male" "male" "male" "male" "2"    "2"   
    ## [1661] "2"    "2"    "male" "male" "2"    "male" "2"    "2"    "2"    "male"
    ## [1671] "male" "male" "male" "2"    "2"    "2"    "2"    "male" "2"    "male"
    ## [1681] "2"    "male" "male" "2"    "2"    "male" "male" "male" "2"    "2"   
    ## [1691] "male" "2"    "2"    "male" "male" "2"    "male" "male" "male" "male"
    ## [1701] "male" "2"    "male" "2"    "2"    "2"    "male" "male" "2"    "2"   
    ## [1711] "male" "male" "male" "male" "male" "2"    "2"    "2"    "2"    "2"   
    ## [1721] "male" "male" "male" "male" "male" "male" "2"    "2"    "male" "2"   
    ## [1731] "2"    "2"    "2"    "2"    "2"    "2"    "male" "male" "male" "male"
    ## [1741] "2"    "male" "2"    "male" "male" "male" "male" "male" "2"    "2"   
    ## [1751] "2"    "2"    "male" "male" "male" "2"    "male" "male" "male" "male"
    ## [1761] "male" "2"    "2"    "male" "2"    "male" "2"    "2"    "male" "2"   
    ## [1771] "2"    "2"    "male" "male" "male" "male" "2"    "male" "male" "2"   
    ## [1781] "male" "2"    "2"    "male" "2"    "2"    "male" "2"    "2"    "2"   
    ## [1791] "2"    "2"    "male" "2"    "2"    "male" "2"    "2"    "2"    "2"   
    ## [1801] "2"    "male" "2"    "2"    "male" "male" "2"    "male" "male" "2"   
    ## [1811] "male" "male" "2"    "male" "2"    "2"    "male" "2"    "2"    "male"
    ## [1821] "2"    "2"    "2"    "male" "2"    "2"    "2"    "male" "male" "male"
    ## [1831] "2"    "2"    "2"    "2"    "2"    "2"    "male" "male" "2"    "male"
    ## [1841] "male" "male" "male" "male" "male" "male" "2"    "2"    "male" "2"   
    ## [1851] "male" "2"    "2"    "male" "male" "male" "male" "male" "male" "2"   
    ## [1861] "male" "male" "male" "2"    "2"    "2"    "male" "2"    "male" "2"   
    ## [1871] "2"    "male" "male" "2"    "male" "2"    "2"    "male" "2"    "male"
    ## [1881] "2"    "male" "male" "2"    "2"    "male" "2"    "2"    "male" "male"
    ## [1891] "male" "male" "male" "2"    "2"    "male" "2"    "male" "2"    "2"   
    ## [1901] "2"    "male" "male" "2"    "male" "male" "2"    "2"    "2"    "2"   
    ## [1911] "male" "2"    "male" "2"    "male" "2"    "male" "2"    "male" "male"
    ## [1921] "2"    "male" "2"    "male" "2"    "male" "male" "2"    "male" "2"   
    ## [1931] "2"    "2"    "2"    "2"    "male" "male" "2"    "2"    "male" "male"
    ## [1941] "2"    "male" "male" "male" "2"    "male" "2"    "2"    "2"    "2"   
    ## [1951] "2"    "male" "male" "2"    "2"    "male" "male" "male" "2"    "male"
    ## [1961] "male" "2"    "male" "male" "2"    "2"    "2"    "2"    "2"    "male"
    ## [1971] "2"    "male" "2"    "2"    "2"    "male" "male" "male" "male" "2"   
    ## [1981] "2"    "2"    "2"    "male" "male" "2"    "2"    "2"    "2"    "2"   
    ## [1991] "2"    "2"    "male" "2"    "2"    "male" "2"    "male" "2"    "2"   
    ## [2001] "male" "2"    "male" "male" "2"    "male" "male" "male" "2"    "2"   
    ## [2011] "male" "male" "2"    "2"    "2"    "2"    "male" "male" "2"    "male"
    ## [2021] "male" "male" "2"    "male" "2"    "male" "2"    "male" "male" "2"   
    ## [2031] "2"    "male" "male" "2"    "2"    "male" "2"    "2"    "2"    "2"   
    ## [2041] "2"    "2"    "2"    "male" "2"    "2"    "male" "2"    "male" "male"
    ## [2051] "male" "male" "male" "2"    "2"    "2"    "2"    "male" "2"    "2"   
    ## [2061] "2"    "male" "2"    "2"    "male" "2"    "male" "2"    "male" "male"
    ## [2071] "2"    "male" "2"    "2"    "male" "2"    "male" "2"    "2"    "male"
    ## [2081] "male" "male" "male" "male" "male" "2"    "2"    "2"    "2"    "male"
    ## [2091] "2"    "male" "2"    "male" "2"    "2"    "male" "male" "male" "male"
    ## [2101] "2"    "male" "2"    "male" "male" "2"    "2"    "male" "2"    "male"
    ## [2111] "male" "2"    "male" "male" "male" "male" "2"    "male" "2"    "2"   
    ## [2121] "male" "2"    "2"    "male" "2"    "male" "2"    "2"    "2"    "2"   
    ## [2131] "male" "2"    "male" "2"    "2"    "2"    "2"    "2"    "male" "male"
    ## [2141] "male" "male" "2"    "male" "male" "2"    "2"    "male" "male" "2"   
    ## [2151] "2"    "male" "male" "2"    "male" "male" "2"    "2"    "2"    "male"
    ## [2161] "2"    "2"    "2"    "male" "male" "2"    "male" "2"    "male" "2"   
    ## [2171] "2"    "male" "male" "male" "2"    "2"    "male" "2"    "male" "2"   
    ## [2181] "2"    "2"    "2"    "male" "2"    "male" "male" "2"    "male" "2"   
    ## [2191] "male" "male" "male" "male" "2"    "male" "male" "male" "male" "male"
    ## [2201] "male" "2"    "male" "male" "male" "2"    "male" "male" "male" "male"
    ## [2211] "male" "2"    "male" "2"    "male" "2"    "male" "male" "2"    "male"
    ## [2221] "2"    "2"    "male" "2"    "male" "male" "2"    "2"    "2"    "2"   
    ## [2231] "2"    "male" "2"    "2"    "2"    "male" "2"    "2"    "2"    "male"
    ## [2241] "2"    "male" "2"    "2"    "male" "male" "male" "male" "male" "male"
    ## [2251] "2"    "male" "2"    "male" "2"    "2"    "male" "male" "2"    "2"   
    ## [2261] "male" "2"    "2"    "male" "2"    "2"    "2"    "2"    "2"    "male"
    ## [2271] "male" "male" "male" "male" "male" "male" "male" "2"    "male" "2"   
    ## [2281] "2"    "male" "2"    "2"    "2"    "male" "2"    "male" "male" "2"   
    ## [2291] "male" "male" "2"    "2"    "male" "2"    "2"    "male" "male" "2"   
    ## [2301] "2"    "2"    "2"    "2"    "2"    "2"    "2"    "2"    "male" "male"
    ## [2311] "2"    "male" "male" "2"    "male" "male" "2"    "2"    "2"    "male"
    ## [2321] "male" "2"    "2"    "male" "2"    "2"    "male" "male" "male" "male"
    ## [2331] "2"    "2"    "2"    "2"    "2"    "2"    "male" "male" "2"    "male"
    ## [2341] "2"    "male" "male" "male" "male" "male" "male" "male" "male" "2"   
    ## [2351] "male" "male" "2"    "2"    "male" "male" "2"    "2"    "2"    "male"
    ## [2361] "2"    "male" "male" "male" "2"    "male" "2"    "2"    "male" "2"   
    ## [2371] "male" "2"    "2"    "male" "2"    "2"    "male" "male" "male" "2"   
    ## [2381] "2"    "male" "male" "male" "2"    "2"    "male" "2"    "2"    "male"
    ## [2391] "male" "2"    "2"    "male" "male" "2"    "2"    "2"    "male" "male"
    ## [2401] "2"    "2"    "male" "male" "male" "2"    "2"    "male" "male" "2"   
    ## [2411] "2"    "male" "male" "male" "2"    "2"    "male" "2"    "2"    "male"
    ## [2421] "2"    "2"    "2"    "male" "male" "2"    "male" "2"    "2"    "2"   
    ## [2431] "male" "male" "male" "male" "male" "male" "2"    "male" "male" "2"   
    ## [2441] "2"    "2"    "male" "2"    "2"    "male" "2"    "male" "male" "2"   
    ## [2451] "2"    "male" "male" "2"    "male" "2"    "male" "2"    "male" "2"   
    ## [2461] "male" "2"    "2"    "2"    "male" "2"    "2"    "male" "2"    "male"
    ## [2471] "male" "male" "male" "male" "2"    "male" "male" "male" "2"    "2"   
    ## [2481] "2"    "2"    "male" "2"    "2"    "male" "2"    "2"    "male" "2"   
    ## [2491] "male" "2"    "2"    "2"    "male" "male" "male" "male" "2"    "2"   
    ## [2501] "2"    "male" "2"    "male" "2"    "male" "2"    "male" "2"    "male"
    ## [2511] "male" "male" "2"    "2"    "2"    "male" "male" "2"    "male" "2"   
    ## [2521] "male" "male" "male" "male" "2"    "male" "male" "male" "2"    "2"   
    ## [2531] "2"    "male" "male" "2"    "2"    "2"    "2"    "2"    "2"    "2"   
    ## [2541] "2"    "2"    "male" "male" "2"    "2"    "male" "2"    "male" "2"   
    ## [2551] "2"    "2"    "2"    "male" "2"    "male" "2"    "2"    "2"    "male"
    ## [2561] "male" "male" "2"    "2"    "2"    "male" "2"    "2"    "male" "2"   
    ## [2571] "2"    "male" "2"    "2"    "2"    "male" "2"    "male" "2"    "2"   
    ## [2581] "male" "male" "2"    "2"    "2"    "male" "male" "male" "2"    "male"
    ## [2591] "2"    "male" "male" "male" "2"    "2"    "male" "male" "2"    "2"   
    ## [2601] "male" "2"    "male" "2"    "2"    "male" "male" "male" "male" "male"
    ## [2611] "male" "2"    "male" "2"    "male" "2"    "2"    "male" "male" "male"
    ## [2621] "male" "2"    "2"    "2"    "male" "male" "2"    "male" "male" "male"
    ## [2631] "2"    "male" "male" "2"    "2"    "male" "male" "2"    "male" "male"
    ## [2641] "male" "male" "male" "2"    "male" "male" "male" "2"    "2"    "male"
    ## [2651] "male" "male" "male" "male" "male" "2"    "2"    "male" "male" "male"
    ## [2661] "2"    "male" "male" "male" "2"    "2"    "male" "male" "male" "male"
    ## [2671] "2"    "2"    "2"    "male" "male" "male" "2"    "male" "male" "2"   
    ## [2681] "2"    "male" "male" "male" "2"    "male" "2"    "2"    "male" "male"
    ## [2691] "male" "2"    "male" "male" "male" "male" "male" "2"    "male" "male"
    ## [2701] "male" "2"    "male" "2"    "2"    "2"    "2"    "2"    "2"    "2"   
    ## [2711] "male" "male" "male" "male" "2"    "male" "male" "2"    "male" "male"
    ## [2721] "male" "male" "2"    "male" "male" "male" "male" "male" "2"    "2"   
    ## [2731] "male" "2"    "male" "2"    "male" "2"    "2"    "2"    "2"    "male"
    ## [2741] "2"    "male" "2"    "male" "2"    "male" "2"    "2"    "2"    "2"   
    ## [2751] "male" "male" "2"    "2"    "male" "2"    "male" "male" "male" "2"   
    ## [2761] "male" "2"    "2"    "2"    "2"    "2"    "male" "male" "male" "2"   
    ## [2771] "2"    "male" "male" "male" "male" "2"    "2"    "2"    "2"    "2"   
    ## [2781] "male" "2"    "male" "2"    "2"    "male" "male" "male" "2"    "male"
    ## [2791] "male" "2"    "2"    "male" "male" "2"    "male" "2"    "2"    "2"   
    ## [2801] "2"    "male" "2"    "male" "male" "2"    "2"    "2"    "2"    "male"
    ## [2811] "2"    "male" "male" "male" "2"    "male" "2"    "male" "male" "male"
    ## [2821] "2"

``` r
recode(Data$i_sex, '1' = "male", '2' = "female")
```

    ##    [1] "male"   "female" "female" "female" "male"   "male"   "female" "male"  
    ##    [9] "female" "female" "male"   "male"   "male"   "female" "male"   "female"
    ##   [17] "female" "female" "female" "female" "female" "male"   "female" "female"
    ##   [25] "female" "male"   "male"   "female" "male"   "male"   "female" "female"
    ##   [33] "male"   "male"   "male"   "male"   "male"   "female" "female" "male"  
    ##   [41] "male"   "female" "male"   "female" "female" "female" "female" "female"
    ##   [49] "male"   "female" "male"   "male"   "female" "male"   "female" "male"  
    ##   [57] "male"   "male"   "male"   "male"   "female" "female" "male"   "female"
    ##   [65] "male"   "male"   "male"   "female" "male"   "female" "female" "male"  
    ##   [73] "male"   "male"   "male"   "female" "female" "male"   "female" "female"
    ##   [81] "male"   "female" "male"   "female" "male"   "male"   "female" "female"
    ##   [89] "female" "male"   "female" "male"   "male"   "female" "male"   "male"  
    ##   [97] "female" "male"   "male"   "male"   "male"   "female" "female" "male"  
    ##  [105] "female" "female" "female" "female" "female" "female" "male"   "male"  
    ##  [113] "male"   "female" "female" "male"   "male"   "male"   "female" "male"  
    ##  [121] "male"   "female" "male"   "female" "female" "female" "male"   "female"
    ##  [129] "male"   "male"   "male"   "female" "male"   "male"   "female" "female"
    ##  [137] "female" "male"   "male"   "female" "female" "male"   "male"   "female"
    ##  [145] "female" "female" "female" "female" "female" "female" "male"   "female"
    ##  [153] "female" "male"   "female" "female" "female" "male"   "male"   "male"  
    ##  [161] "male"   "male"   "female" "female" "female" "male"   "female" "male"  
    ##  [169] "male"   "male"   "male"   "male"   "female" "female" "female" "female"
    ##  [177] "male"   "female" "male"   "female" "male"   "female" "male"   "female"
    ##  [185] "female" "male"   "male"   "male"   "male"   "female" "male"   "female"
    ##  [193] "female" "female" "female" "female" "male"   "male"   "male"   "female"
    ##  [201] "female" "female" "female" "male"   "female" "male"   "male"   "male"  
    ##  [209] "female" "female" "female" "male"   "male"   "male"   "male"   "female"
    ##  [217] "male"   "female" "female" "male"   "male"   "male"   "male"   "male"  
    ##  [225] "male"   "male"   "female" "male"   "male"   "male"   "female" "male"  
    ##  [233] "male"   "male"   "female" "male"   "male"   "male"   "female" "female"
    ##  [241] "female" "female" "male"   "male"   "female" "female" "male"   "male"  
    ##  [249] "female" "male"   "female" "female" "female" "male"   "female" "male"  
    ##  [257] "male"   "female" "male"   "male"   "female" "male"   "female" "male"  
    ##  [265] "male"   "male"   "female" "male"   "male"   "female" "female" "male"  
    ##  [273] "female" "female" "female" "female" "male"   "male"   "female" "male"  
    ##  [281] "female" "male"   "female" "female" "male"   "female" "male"   "female"
    ##  [289] "female" "female" "female" "male"   "female" "female" "male"   "female"
    ##  [297] "female" "female" "male"   "female" "female" "female" "female" "male"  
    ##  [305] "male"   "female" "male"   "female" "female" "male"   "male"   "male"  
    ##  [313] "male"   "female" "male"   "male"   "male"   "male"   "male"   "male"  
    ##  [321] "male"   "male"   "male"   "female" "male"   "male"   "female" "female"
    ##  [329] "male"   "female" "female" "male"   "male"   "male"   "male"   "female"
    ##  [337] "female" "female" "male"   "male"   "female" "male"   "male"   "female"
    ##  [345] "female" "female" "male"   "male"   "female" "male"   "female" "male"  
    ##  [353] "male"   "female" "female" "male"   "female" "male"   "male"   "male"  
    ##  [361] "female" "male"   "male"   "male"   "male"   "female" "female" "female"
    ##  [369] "female" "female" "female" "female" "female" "female" "male"   "female"
    ##  [377] "female" "female" "male"   "male"   "male"   "male"   "female" "female"
    ##  [385] "male"   "male"   "male"   "male"   "male"   "male"   "female" "female"
    ##  [393] "male"   "male"   "female" "female" "male"   "female" "male"   "male"  
    ##  [401] "male"   "male"   "male"   "female" "male"   "female" "male"   "male"  
    ##  [409] "male"   "male"   "male"   "female" "male"   "female" "male"   "female"
    ##  [417] "male"   "male"   "male"   "female" "male"   "female" "female" "female"
    ##  [425] "male"   "male"   "male"   "female" "male"   "male"   "female" "female"
    ##  [433] "female" "female" "male"   "male"   "female" "male"   "female" "male"  
    ##  [441] "male"   "female" "male"   "male"   "male"   "female" "female" "male"  
    ##  [449] "female" "female" "male"   "male"   "female" "male"   "female" "female"
    ##  [457] "male"   "female" "male"   "male"   "male"   "female" "male"   "female"
    ##  [465] "male"   "male"   "male"   "male"   "female" "female" "female" "male"  
    ##  [473] "female" "male"   "male"   "male"   "female" "female" "male"   "male"  
    ##  [481] "male"   "male"   "male"   "female" "male"   "female" "male"   "male"  
    ##  [489] "male"   "male"   "male"   "male"   "female" "male"   "female" "male"  
    ##  [497] "female" "female" "female" "male"   "female" "male"   "female" "male"  
    ##  [505] "male"   "male"   "female" "female" "female" "male"   "male"   "male"  
    ##  [513] "male"   "female" "male"   "male"   "female" "female" "male"   "male"  
    ##  [521] "female" "female" "male"   "female" "male"   "male"   "female" "female"
    ##  [529] "male"   "male"   "male"   "male"   "male"   "female" "male"   "female"
    ##  [537] "female" "female" "female" "female" "male"   "female" "male"   "male"  
    ##  [545] "male"   "male"   "female" "female" "male"   "male"   "female" "male"  
    ##  [553] "female" "male"   "female" "male"   "male"   "male"   "male"   "female"
    ##  [561] "female" "female" "female" "female" "female" "female" "male"   "female"
    ##  [569] "male"   "male"   "female" "male"   "female" "female" "male"   "male"  
    ##  [577] "female" "female" "male"   "male"   "female" "female" "female" "female"
    ##  [585] "female" "female" "male"   "male"   "female" "male"   "female" "female"
    ##  [593] "female" "female" "female" "male"   "male"   "female" "male"   "female"
    ##  [601] "female" "female" "male"   "male"   "male"   "female" "female" "male"  
    ##  [609] "female" "female" "male"   "female" "male"   "male"   "male"   "female"
    ##  [617] "male"   "female" "female" "female" "male"   "male"   "female" "male"  
    ##  [625] "female" "male"   "male"   "male"   "female" "female" "female" "male"  
    ##  [633] "female" "male"   "female" "male"   "male"   "male"   "male"   "female"
    ##  [641] "male"   "male"   "female" "male"   "female" "female" "male"   "male"  
    ##  [649] "female" "female" "female" "male"   "male"   "male"   "male"   "female"
    ##  [657] "male"   "male"   "female" "female" "male"   "male"   "male"   "male"  
    ##  [665] "female" "female" "female" "female" "female" "male"   "female" "male"  
    ##  [673] "male"   "female" "female" "male"   "male"   "male"   "female" "female"
    ##  [681] "male"   "male"   "female" "male"   "female" "female" "male"   "female"
    ##  [689] "female" "female" "male"   "female" "female" "female" "female" "female"
    ##  [697] "female" "female" "male"   "female" "male"   "male"   "male"   "female"
    ##  [705] "female" "male"   "male"   "male"   "male"   "male"   "female" "female"
    ##  [713] "male"   "male"   "female" "male"   "female" "male"   "female" "male"  
    ##  [721] "female" "female" "female" "female" "male"   "female" "female" "female"
    ##  [729] "male"   "female" "female" "female" "male"   "male"   "female" "female"
    ##  [737] "male"   "female" "female" "female" "female" "female" "female" "male"  
    ##  [745] "male"   "male"   "male"   "male"   "male"   "male"   "male"   "female"
    ##  [753] "male"   "male"   "female" "male"   "male"   "female" "female" "male"  
    ##  [761] "male"   "female" "female" "female" "male"   "female" "male"   "male"  
    ##  [769] "female" "female" "male"   "female" "male"   "male"   "male"   "male"  
    ##  [777] "female" "male"   "male"   "female" "female" "female" "female" "male"  
    ##  [785] "female" "male"   "female" "male"   "male"   "female" "male"   "female"
    ##  [793] "female" "male"   "male"   "female" "female" "male"   "female" "female"
    ##  [801] "male"   "male"   "female" "female" "female" "female" "male"   "male"  
    ##  [809] "male"   "male"   "female" "male"   "male"   "female" "male"   "female"
    ##  [817] "female" "male"   "female" "female" "male"   "female" "female" "female"
    ##  [825] "male"   "male"   "male"   "female" "male"   "male"   "female" "female"
    ##  [833] "male"   "female" "female" "male"   "male"   "male"   "male"   "male"  
    ##  [841] "female" "female" "female" "male"   "female" "male"   "female" "female"
    ##  [849] "female" "male"   "male"   "female" "female" "male"   "male"   "female"
    ##  [857] "male"   "male"   "male"   "female" "male"   "female" "male"   "male"  
    ##  [865] "female" "female" "male"   "male"   "female" "male"   "female" "female"
    ##  [873] "male"   "male"   "male"   "female" "male"   "male"   "male"   "male"  
    ##  [881] "male"   "female" "female" "male"   "male"   "male"   "female" "male"  
    ##  [889] "male"   "male"   "female" "male"   "female" "male"   "female" "male"  
    ##  [897] "male"   "male"   "female" "female" "male"   "male"   "female" "female"
    ##  [905] "male"   "male"   "male"   "male"   "female" "female" "female" "male"  
    ##  [913] "male"   "male"   "male"   "male"   "female" "male"   "female" "female"
    ##  [921] "male"   "female" "female" "male"   "male"   "male"   "female" "male"  
    ##  [929] "female" "male"   "male"   "male"   "male"   "male"   "male"   "male"  
    ##  [937] "male"   "female" "female" "female" "male"   "male"   "female" "male"  
    ##  [945] "female" "female" "male"   "female" "male"   "male"   "male"   "female"
    ##  [953] "female" "female" "female" "female" "male"   "female" "male"   "female"
    ##  [961] "male"   "female" "female" "male"   "female" "male"   "male"   "female"
    ##  [969] "female" "female" "female" "male"   "male"   "male"   "female" "female"
    ##  [977] "female" "male"   "female" "female" "female" "male"   "female" "female"
    ##  [985] "male"   "male"   "male"   "female" "female" "female" "male"   "female"
    ##  [993] "male"   "male"   "male"   "female" "female" "female" "female" "female"
    ## [1001] "male"   "male"   "male"   "female" "female" "female" "female" "female"
    ## [1009] "female" "male"   "male"   "male"   "male"   "male"   "male"   "male"  
    ## [1017] "female" "male"   "male"   "male"   "female" "female" "male"   "male"  
    ## [1025] "female" "female" "male"   "male"   "male"   "female" "male"   "male"  
    ## [1033] "female" "male"   "male"   "female" "male"   "female" "male"   "male"  
    ## [1041] "male"   "male"   "male"   "male"   "male"   "male"   "female" "male"  
    ## [1049] "female" "male"   "male"   "female" "female" "male"   "male"   "female"
    ## [1057] "female" "female" "female" "female" "female" "male"   "female" "female"
    ## [1065] "male"   "male"   "male"   "male"   "female" "female" "male"   "male"  
    ## [1073] "female" "female" "female" "female" "male"   "female" "male"   "male"  
    ## [1081] "female" "male"   "female" "female" "male"   "male"   "male"   "male"  
    ## [1089] "female" "female" "male"   "male"   "male"   "female" "male"   "female"
    ## [1097] "female" "male"   "female" "male"   "female" "female" "female" "male"  
    ## [1105] "female" "male"   "male"   "female" "male"   "male"   "female" "male"  
    ## [1113] "male"   "female" "female" "male"   "female" "male"   "male"   "male"  
    ## [1121] "female" "female" "male"   "female" "female" "male"   "female" "male"  
    ## [1129] "female" "female" "female" "male"   "male"   "female" "male"   "female"
    ## [1137] "male"   "female" "male"   "female" "male"   "female" "female" "female"
    ## [1145] "female" "male"   "female" "male"   "female" "male"   "male"   "female"
    ## [1153] "female" "female" "female" "male"   "male"   "female" "female" "male"  
    ## [1161] "female" "male"   "female" "female" "male"   "male"   "male"   "female"
    ## [1169] "female" "female" "male"   "female" "female" "male"   "male"   "female"
    ## [1177] "female" "male"   "male"   "female" "male"   "male"   "female" "male"  
    ## [1185] "male"   "male"   "female" "female" "male"   "male"   "female" "female"
    ## [1193] "male"   "female" "female" "female" "male"   "female" "female" "male"  
    ## [1201] "female" "female" "female" "male"   "female" "female" "female" "female"
    ## [1209] "male"   "female" "female" "male"   "female" "male"   "male"   "female"
    ## [1217] "male"   "female" "male"   "male"   "male"   "male"   "male"   "female"
    ## [1225] "female" "male"   "female" "female" "male"   "female" "male"   "female"
    ## [1233] "male"   "female" "female" "male"   "male"   "male"   "male"   "female"
    ## [1241] "female" "female" "female" "female" "female" "female" "male"   "male"  
    ## [1249] "female" "male"   "male"   "male"   "female" "female" "female" "female"
    ## [1257] "female" "female" "female" "female" "female" "male"   "male"   "male"  
    ## [1265] "female" "male"   "male"   "male"   "male"   "female" "female" "female"
    ## [1273] "male"   "female" "female" "female" "male"   "female" "male"   "female"
    ## [1281] "female" "male"   "female" "female" "male"   "female" "female" "female"
    ## [1289] "male"   "female" "male"   "female" "female" "female" "male"   "male"  
    ## [1297] "male"   "male"   "female" "male"   "female" "male"   "male"   "female"
    ## [1305] "male"   "female" "female" "female" "male"   "male"   "female" "male"  
    ## [1313] "female" "male"   "male"   "female" "male"   "female" "female" "male"  
    ## [1321] "female" "female" "male"   "male"   "male"   "male"   "male"   "male"  
    ## [1329] "male"   "female" "male"   "female" "male"   "male"   "female" "male"  
    ## [1337] "male"   "male"   "female" "female" "male"   "female" "male"   "female"
    ## [1345] "male"   "female" "male"   "male"   "female" "female" "male"   "male"  
    ## [1353] "male"   "male"   "female" "male"   "male"   "female" "male"   "male"  
    ## [1361] "male"   "male"   "female" "female" "male"   "female" "male"   "male"  
    ## [1369] "female" "male"   "female" "male"   "female" "male"   "male"   "male"  
    ## [1377] "male"   "female" "female" "male"   "female" "male"   "male"   "male"  
    ## [1385] "female" "female" "male"   "male"   "female" "male"   "male"   "female"
    ## [1393] "male"   "female" "female" "female" "male"   "female" "male"   "female"
    ## [1401] "female" "female" "female" "female" "female" "male"   "female" "male"  
    ## [1409] "female" "male"   "female" "female" "female" "male"   "female" "female"
    ## [1417] "male"   "female" "female" "female" "male"   "female" "female" "female"
    ## [1425] "female" "female" "male"   "male"   "male"   "male"   "female" "female"
    ## [1433] "male"   "female" "female" "male"   "female" "male"   "female" "female"
    ## [1441] "female" "male"   "male"   "female" "female" "male"   "male"   "female"
    ## [1449] "male"   "female" "male"   "male"   "female" "female" "female" "female"
    ## [1457] "female" "female" "male"   "male"   "female" "male"   "female" "male"  
    ## [1465] "female" "female" "male"   "female" "female" "female" "male"   "female"
    ## [1473] "male"   "male"   "female" "male"   "female" "male"   "male"   "female"
    ## [1481] "female" "female" "female" "female" "female" "female" "male"   "male"  
    ## [1489] "male"   "female" "female" "male"   "female" "male"   "female" "male"  
    ## [1497] "male"   "male"   "female" "male"   "male"   "male"   "female" "male"  
    ## [1505] "female" "female" "female" "male"   "male"   "female" "female" "female"
    ## [1513] "female" "female" "female" "male"   "male"   "male"   "male"   "female"
    ## [1521] "female" "male"   "female" "male"   "male"   "male"   "male"   "male"  
    ## [1529] "female" "female" "male"   "female" "male"   "female" "male"   "male"  
    ## [1537] "female" "female" "male"   "female" "male"   "male"   "male"   "male"  
    ## [1545] "female" "female" "female" "female" "female" "female" "female" "female"
    ## [1553] "female" "male"   "male"   "female" "female" "female" "female" "female"
    ## [1561] "female" "male"   "female" "female" "male"   "male"   "male"   "male"  
    ## [1569] "male"   "female" "male"   "male"   "male"   "female" "female" "male"  
    ## [1577] "male"   "female" "male"   "male"   "female" "male"   "female" "female"
    ## [1585] "female" "male"   "male"   "female" "male"   "female" "male"   "female"
    ## [1593] "female" "female" "female" "female" "male"   "female" "female" "female"
    ## [1601] "female" "male"   "male"   "male"   "female" "male"   "male"   "male"  
    ## [1609] "male"   "female" "male"   "male"   "female" "female" "male"   "female"
    ## [1617] "female" "female" "male"   "female" "male"   "male"   "male"   "female"
    ## [1625] "male"   "male"   "female" "female" "female" "female" "female" "female"
    ## [1633] "female" "female" "female" "male"   "male"   "female" "male"   "female"
    ## [1641] "female" "female" "male"   "male"   "male"   "female" "male"   "female"
    ## [1649] "female" "male"   "female" "male"   "male"   "male"   "male"   "male"  
    ## [1657] "male"   "male"   "female" "female" "female" "female" "male"   "male"  
    ## [1665] "female" "male"   "female" "female" "female" "male"   "male"   "male"  
    ## [1673] "male"   "female" "female" "female" "female" "male"   "female" "male"  
    ## [1681] "female" "male"   "male"   "female" "female" "male"   "male"   "male"  
    ## [1689] "female" "female" "male"   "female" "female" "male"   "male"   "female"
    ## [1697] "male"   "male"   "male"   "male"   "male"   "female" "male"   "female"
    ## [1705] "female" "female" "male"   "male"   "female" "female" "male"   "male"  
    ## [1713] "male"   "male"   "male"   "female" "female" "female" "female" "female"
    ## [1721] "male"   "male"   "male"   "male"   "male"   "male"   "female" "female"
    ## [1729] "male"   "female" "female" "female" "female" "female" "female" "female"
    ## [1737] "male"   "male"   "male"   "male"   "female" "male"   "female" "male"  
    ## [1745] "male"   "male"   "male"   "male"   "female" "female" "female" "female"
    ## [1753] "male"   "male"   "male"   "female" "male"   "male"   "male"   "male"  
    ## [1761] "male"   "female" "female" "male"   "female" "male"   "female" "female"
    ## [1769] "male"   "female" "female" "female" "male"   "male"   "male"   "male"  
    ## [1777] "female" "male"   "male"   "female" "male"   "female" "female" "male"  
    ## [1785] "female" "female" "male"   "female" "female" "female" "female" "female"
    ## [1793] "male"   "female" "female" "male"   "female" "female" "female" "female"
    ## [1801] "female" "male"   "female" "female" "male"   "male"   "female" "male"  
    ## [1809] "male"   "female" "male"   "male"   "female" "male"   "female" "female"
    ## [1817] "male"   "female" "female" "male"   "female" "female" "female" "male"  
    ## [1825] "female" "female" "female" "male"   "male"   "male"   "female" "female"
    ## [1833] "female" "female" "female" "female" "male"   "male"   "female" "male"  
    ## [1841] "male"   "male"   "male"   "male"   "male"   "male"   "female" "female"
    ## [1849] "male"   "female" "male"   "female" "female" "male"   "male"   "male"  
    ## [1857] "male"   "male"   "male"   "female" "male"   "male"   "male"   "female"
    ## [1865] "female" "female" "male"   "female" "male"   "female" "female" "male"  
    ## [1873] "male"   "female" "male"   "female" "female" "male"   "female" "male"  
    ## [1881] "female" "male"   "male"   "female" "female" "male"   "female" "female"
    ## [1889] "male"   "male"   "male"   "male"   "male"   "female" "female" "male"  
    ## [1897] "female" "male"   "female" "female" "female" "male"   "male"   "female"
    ## [1905] "male"   "male"   "female" "female" "female" "female" "male"   "female"
    ## [1913] "male"   "female" "male"   "female" "male"   "female" "male"   "male"  
    ## [1921] "female" "male"   "female" "male"   "female" "male"   "male"   "female"
    ## [1929] "male"   "female" "female" "female" "female" "female" "male"   "male"  
    ## [1937] "female" "female" "male"   "male"   "female" "male"   "male"   "male"  
    ## [1945] "female" "male"   "female" "female" "female" "female" "female" "male"  
    ## [1953] "male"   "female" "female" "male"   "male"   "male"   "female" "male"  
    ## [1961] "male"   "female" "male"   "male"   "female" "female" "female" "female"
    ## [1969] "female" "male"   "female" "male"   "female" "female" "female" "male"  
    ## [1977] "male"   "male"   "male"   "female" "female" "female" "female" "male"  
    ## [1985] "male"   "female" "female" "female" "female" "female" "female" "female"
    ## [1993] "male"   "female" "female" "male"   "female" "male"   "female" "female"
    ## [2001] "male"   "female" "male"   "male"   "female" "male"   "male"   "male"  
    ## [2009] "female" "female" "male"   "male"   "female" "female" "female" "female"
    ## [2017] "male"   "male"   "female" "male"   "male"   "male"   "female" "male"  
    ## [2025] "female" "male"   "female" "male"   "male"   "female" "female" "male"  
    ## [2033] "male"   "female" "female" "male"   "female" "female" "female" "female"
    ## [2041] "female" "female" "female" "male"   "female" "female" "male"   "female"
    ## [2049] "male"   "male"   "male"   "male"   "male"   "female" "female" "female"
    ## [2057] "female" "male"   "female" "female" "female" "male"   "female" "female"
    ## [2065] "male"   "female" "male"   "female" "male"   "male"   "female" "male"  
    ## [2073] "female" "female" "male"   "female" "male"   "female" "female" "male"  
    ## [2081] "male"   "male"   "male"   "male"   "male"   "female" "female" "female"
    ## [2089] "female" "male"   "female" "male"   "female" "male"   "female" "female"
    ## [2097] "male"   "male"   "male"   "male"   "female" "male"   "female" "male"  
    ## [2105] "male"   "female" "female" "male"   "female" "male"   "male"   "female"
    ## [2113] "male"   "male"   "male"   "male"   "female" "male"   "female" "female"
    ## [2121] "male"   "female" "female" "male"   "female" "male"   "female" "female"
    ## [2129] "female" "female" "male"   "female" "male"   "female" "female" "female"
    ## [2137] "female" "female" "male"   "male"   "male"   "male"   "female" "male"  
    ## [2145] "male"   "female" "female" "male"   "male"   "female" "female" "male"  
    ## [2153] "male"   "female" "male"   "male"   "female" "female" "female" "male"  
    ## [2161] "female" "female" "female" "male"   "male"   "female" "male"   "female"
    ## [2169] "male"   "female" "female" "male"   "male"   "male"   "female" "female"
    ## [2177] "male"   "female" "male"   "female" "female" "female" "female" "male"  
    ## [2185] "female" "male"   "male"   "female" "male"   "female" "male"   "male"  
    ## [2193] "male"   "male"   "female" "male"   "male"   "male"   "male"   "male"  
    ## [2201] "male"   "female" "male"   "male"   "male"   "female" "male"   "male"  
    ## [2209] "male"   "male"   "male"   "female" "male"   "female" "male"   "female"
    ## [2217] "male"   "male"   "female" "male"   "female" "female" "male"   "female"
    ## [2225] "male"   "male"   "female" "female" "female" "female" "female" "male"  
    ## [2233] "female" "female" "female" "male"   "female" "female" "female" "male"  
    ## [2241] "female" "male"   "female" "female" "male"   "male"   "male"   "male"  
    ## [2249] "male"   "male"   "female" "male"   "female" "male"   "female" "female"
    ## [2257] "male"   "male"   "female" "female" "male"   "female" "female" "male"  
    ## [2265] "female" "female" "female" "female" "female" "male"   "male"   "male"  
    ## [2273] "male"   "male"   "male"   "male"   "male"   "female" "male"   "female"
    ## [2281] "female" "male"   "female" "female" "female" "male"   "female" "male"  
    ## [2289] "male"   "female" "male"   "male"   "female" "female" "male"   "female"
    ## [2297] "female" "male"   "male"   "female" "female" "female" "female" "female"
    ## [2305] "female" "female" "female" "female" "male"   "male"   "female" "male"  
    ## [2313] "male"   "female" "male"   "male"   "female" "female" "female" "male"  
    ## [2321] "male"   "female" "female" "male"   "female" "female" "male"   "male"  
    ## [2329] "male"   "male"   "female" "female" "female" "female" "female" "female"
    ## [2337] "male"   "male"   "female" "male"   "female" "male"   "male"   "male"  
    ## [2345] "male"   "male"   "male"   "male"   "male"   "female" "male"   "male"  
    ## [2353] "female" "female" "male"   "male"   "female" "female" "female" "male"  
    ## [2361] "female" "male"   "male"   "male"   "female" "male"   "female" "female"
    ## [2369] "male"   "female" "male"   "female" "female" "male"   "female" "female"
    ## [2377] "male"   "male"   "male"   "female" "female" "male"   "male"   "male"  
    ## [2385] "female" "female" "male"   "female" "female" "male"   "male"   "female"
    ## [2393] "female" "male"   "male"   "female" "female" "female" "male"   "male"  
    ## [2401] "female" "female" "male"   "male"   "male"   "female" "female" "male"  
    ## [2409] "male"   "female" "female" "male"   "male"   "male"   "female" "female"
    ## [2417] "male"   "female" "female" "male"   "female" "female" "female" "male"  
    ## [2425] "male"   "female" "male"   "female" "female" "female" "male"   "male"  
    ## [2433] "male"   "male"   "male"   "male"   "female" "male"   "male"   "female"
    ## [2441] "female" "female" "male"   "female" "female" "male"   "female" "male"  
    ## [2449] "male"   "female" "female" "male"   "male"   "female" "male"   "female"
    ## [2457] "male"   "female" "male"   "female" "male"   "female" "female" "female"
    ## [2465] "male"   "female" "female" "male"   "female" "male"   "male"   "male"  
    ## [2473] "male"   "male"   "female" "male"   "male"   "male"   "female" "female"
    ## [2481] "female" "female" "male"   "female" "female" "male"   "female" "female"
    ## [2489] "male"   "female" "male"   "female" "female" "female" "male"   "male"  
    ## [2497] "male"   "male"   "female" "female" "female" "male"   "female" "male"  
    ## [2505] "female" "male"   "female" "male"   "female" "male"   "male"   "male"  
    ## [2513] "female" "female" "female" "male"   "male"   "female" "male"   "female"
    ## [2521] "male"   "male"   "male"   "male"   "female" "male"   "male"   "male"  
    ## [2529] "female" "female" "female" "male"   "male"   "female" "female" "female"
    ## [2537] "female" "female" "female" "female" "female" "female" "male"   "male"  
    ## [2545] "female" "female" "male"   "female" "male"   "female" "female" "female"
    ## [2553] "female" "male"   "female" "male"   "female" "female" "female" "male"  
    ## [2561] "male"   "male"   "female" "female" "female" "male"   "female" "female"
    ## [2569] "male"   "female" "female" "male"   "female" "female" "female" "male"  
    ## [2577] "female" "male"   "female" "female" "male"   "male"   "female" "female"
    ## [2585] "female" "male"   "male"   "male"   "female" "male"   "female" "male"  
    ## [2593] "male"   "male"   "female" "female" "male"   "male"   "female" "female"
    ## [2601] "male"   "female" "male"   "female" "female" "male"   "male"   "male"  
    ## [2609] "male"   "male"   "male"   "female" "male"   "female" "male"   "female"
    ## [2617] "female" "male"   "male"   "male"   "male"   "female" "female" "female"
    ## [2625] "male"   "male"   "female" "male"   "male"   "male"   "female" "male"  
    ## [2633] "male"   "female" "female" "male"   "male"   "female" "male"   "male"  
    ## [2641] "male"   "male"   "male"   "female" "male"   "male"   "male"   "female"
    ## [2649] "female" "male"   "male"   "male"   "male"   "male"   "male"   "female"
    ## [2657] "female" "male"   "male"   "male"   "female" "male"   "male"   "male"  
    ## [2665] "female" "female" "male"   "male"   "male"   "male"   "female" "female"
    ## [2673] "female" "male"   "male"   "male"   "female" "male"   "male"   "female"
    ## [2681] "female" "male"   "male"   "male"   "female" "male"   "female" "female"
    ## [2689] "male"   "male"   "male"   "female" "male"   "male"   "male"   "male"  
    ## [2697] "male"   "female" "male"   "male"   "male"   "female" "male"   "female"
    ## [2705] "female" "female" "female" "female" "female" "female" "male"   "male"  
    ## [2713] "male"   "male"   "female" "male"   "male"   "female" "male"   "male"  
    ## [2721] "male"   "male"   "female" "male"   "male"   "male"   "male"   "male"  
    ## [2729] "female" "female" "male"   "female" "male"   "female" "male"   "female"
    ## [2737] "female" "female" "female" "male"   "female" "male"   "female" "male"  
    ## [2745] "female" "male"   "female" "female" "female" "female" "male"   "male"  
    ## [2753] "female" "female" "male"   "female" "male"   "male"   "male"   "female"
    ## [2761] "male"   "female" "female" "female" "female" "female" "male"   "male"  
    ## [2769] "male"   "female" "female" "male"   "male"   "male"   "male"   "female"
    ## [2777] "female" "female" "female" "female" "male"   "female" "male"   "female"
    ## [2785] "female" "male"   "male"   "male"   "female" "male"   "male"   "female"
    ## [2793] "female" "male"   "male"   "female" "male"   "female" "female" "female"
    ## [2801] "female" "male"   "female" "male"   "male"   "female" "female" "female"
    ## [2809] "female" "male"   "female" "male"   "male"   "male"   "female" "male"  
    ## [2817] "female" "male"   "male"   "male"   "female"

``` r
class(Data$i_sex)
```

    ## [1] "character"

## Calculate means (10 points)

Produce code that calculates probabilities of having an account on
social media (i.e. the mean of your new binary variable produced in the
previous problem) by age and gender.

``` r
library("dplyr")

# Probability of having an account on social media by age
aggregate(x = Data$i_ypdoby,
          by = list(Data$i_ypsocweb),
          FUN = mean)
```

    ##   Group.1        x
    ## 1       1 2004.176
    ## 2       2 2005.925

``` r
Data %>%
  group_by(i_ypsocweb, na.rm = TRUE) %>%
  summarize(age = mean(i_ypdoby))
```

    ## # A tibble: 3 x 3
    ## # Groups:   i_ypsocweb [3]
    ##   i_ypsocweb na.rm   age
    ##        <dbl> <lgl> <dbl>
    ## 1          1 TRUE  2004.
    ## 2          2 TRUE  2006.
    ## 3         NA TRUE  2005.

``` r
# Probability of having an account on social media by gender
  
Data$i_sex <- as.numeric(as.character(Data$i_sex))
class(Data$i_sex)
```

    ## [1] "numeric"

``` r
Data %>%
  group_by(i_ypsocweb, na.rm = TRUE) %>%
  summarize(sex = mean(i_sex))
```

    ## # A tibble: 3 x 3
    ## # Groups:   i_ypsocweb [3]
    ##   i_ypsocweb na.rm   sex
    ##        <dbl> <lgl> <dbl>
    ## 1          1 TRUE   1.49
    ## 2          2 TRUE   1.50
    ## 3         NA TRUE   1.86

## Write short interpretation (10 points)

# The probability of those who have a social media account (i\_ypsocweb = 1) are on average born in 2004 (mean for group 1 is 2004.176). Therefore they are on average at least 15 years old. Furthermore, the average age of those who do not have a social media account (i\_ypsocweb = 2) is 14 or those who are born in the latter part of 2005 (mean for group 2 is 2005.925).

# The probabilty of having a social media account by gender shows that males and females are almsost just as likely to have an account. As the probablity of having a social media account is (1.507246) and on the other hand the probability of not having a social media account is (1.488679).

## Visualise results (20 points)

Create a statistical graph (only one, but it can be faceted)
illustrating your results (i.e. showing how the probability of having an
account on social media changes with age and gender). Which type of
statistical graph would be most appropriate for this?

``` r
# dependent variable = i_ypsocweb
# independent variable = i_ypdoby & i_sex
regression <- lm(Data$i_ypsocweb ~ Data$i_ypdoby + Data$i_sex, data = Data)
summary(regression)
```

    ## 
    ## Call:
    ## lm(formula = Data$i_ypsocweb ~ Data$i_ypdoby + Data$i_sex, data = Data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.54670 -0.23121 -0.07071  0.01503  1.09528 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -1.597e+02  7.519e+00 -21.236   <2e-16 ***
    ## Data$i_ypdoby  8.025e-02  3.751e-03  21.393   <2e-16 ***
    ## Data$i_sex     5.495e-03  1.371e-02   0.401    0.689    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3631 on 2804 degrees of freedom
    ##   (14 observations deleted due to missingness)
    ## Multiple R-squared:  0.1403, Adjusted R-squared:  0.1397 
    ## F-statistic: 228.9 on 2 and 2804 DF,  p-value: < 2.2e-16

``` r
library(ggplot2)

ggplot(Data, aes(x = Data$i_sex, y = Data$i_ypdoby)) +
  geom_point() 
```

![](Test-assignemt_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
