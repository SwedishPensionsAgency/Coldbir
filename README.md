# coldbir

## Installation

Use `devtools` for easy installation

```r
library(devtools)
install_github('coldbir', 'SthlmR')
```

## Introduction

First, make sure to load the package with `library(coldbir)`.

Then the next step is to decide where the database is to be saved, or where it already exists:

```r
db <- coldbir:::db$new("my_path")
```

To save data to the database one simply use `put_v`:

```r
variable <- 1:10
db$put_v(variable)
```

and to get the variable it just to use `get_v`:

```r
a <- db$get_v("variable")
```

It is also possible to put a data frame to the coldbir database - each column will then represent one variable each.
Once again, make use of `put_v`:

```r
variables <- MASS::survey
db$put_v(variables)
```

However, since they are all saved as variables one cannot get all of them back at once. 
Use `get_v` to get them back. Their names are the same as their previous column names. For example:

```r
b <- db$get_v("Pulse")
```
    
This was an quick introduction to coldbir. Soon more to come.

### File structure

The coldbir database consists of folders where each folder represent a variable. 
Each variable may have several dimensions, e.g. months and years. 
The data is stored as a [column-oriented DBMS](http://en.wikipedia.org/wiki/Column-oriented_DBMS). 
Below is an example of a database, named *mydb*, with a couple of variables:

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

## Contact

The project is currently maintained by Thomas Reinholdsson (<reinholdsson@gmail.com>).
