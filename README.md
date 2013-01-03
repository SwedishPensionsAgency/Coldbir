# coldbir

(Keep it simple stupid) column database in R.

## Installation

Use `devtools` for easy installation

    # install.packages("devtools")
    devtools:::install_github('coldbir', 'mikula')
    library(coldbir)

## Introduction

### Database
The coldbir database consists of folders where each folder represent a variable. Each variable may have several dimensions, e.g. months and years. The data is stored as a [column-oriented DBMS](http://en.wikipedia.org/wiki/Column-oriented_DBMS). Below is an example of a database, named *mydb*, with a couple of variables:

```
mydb/
  income/
    data/
      income[2011].cdb.gz
      income[2012].cdb.gz
    LOOKUP.txt
    README.md
  unemployment/
    data/
      unemployment[2011].cdb.gz
      unemployment[2012].cdb.gz
    LOOKUP.txt
    README.md
```

## Examples

To simplify the explanation we will introduce with another example. We will use the *survey* dataset from the `MASS` package. First, load the necessary packages

```{r}
library(coldbir)
library(MASS)
```

Then, we could simply write one specific variable to cdb file format with `put_v(survey$Exer, "exercise", "survey")`, but we will instead convert the whole dataset at once:

```{r}
put_db(survey, "survey")
# Files were successfully written to disk
# [1] TRUE
```

To read a variable from disk, one simply type `variable <- get_v("m.i", "survey")`, or

```{r}
setwd("survey")
head(get_v("m.i"))
# [1]  2  1 NA  2  2  1
```

where we now changed our working directory to our *survey* database path. It is also possible to save a dictionary explaining what the keys in the data represents, for example:

```{r}
df <- data.frame(key = c(1, 2), value = c("Imperial", "Metric"))

put_dict(df, "m.i")
# Dictionary was successfully written to disk
# [1] TRUE

get_dict("m.i")
#   key    value
# 1   1 Imperial
# 2   2   Metric

v <- get_v("m.i")
v <- to_values(v, "m.i")
head(v)
# [1] "Metric"   "Imperial" NA         "Metric"   "Metric"   "Imperial"
```
