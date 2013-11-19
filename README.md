# Coldbir
-----------------------

The Coldbir package is a column-oriented DBMS in R.

The project is currently maintained by Thomas Reinholdsson (<reinholdsson@gmail.com>).

## Installation

Use `devtools` for easy installation:

    devtools::install_github('Coldbir', 'SthlmR')

## Introduction

First, make sure to load the package with `library(Coldbir)`.

Then, to decide where the database is to be saved, or whether it already exists, do:

    a <- cdb('database_path')

To save data to the database, simply write:

    a['foo'] <- 1:10

and to get a variable:

    b <- a['foo']

It is also possible to put a data frame to the coldbir database - each column will then represent one variable each;

    a[] <- MASS::survey

However, since they are all saved as variables one cannot get all of them back at once. Their names are the same as their previous column names. For example:

    b <- a['Pulse']

### Data types

Currently supported data types:

- `integer`
- `double`
- `logical`
- `factor`
- `Date`
- `POSIXct` / `POSIXlt` ** **EXPERIMENTAL** **

Timezones are not supported. All timestamps are written as `GMT` without timezone conversion. E.g. `2013-04-29 01:00:00 CST` is stored (and returned) as `2013-04-29 01:00:00 GMT`. `POSIXlt` is automatically converted to `POSIXct`.

### Variable documentation

An additional feature is to add documentation to a variable. 

Create an object of the `doc` class and add it to a variable:

    a['foo'] <- doc(
          'Foo' = 'This is a variable', 
          'Info' = list(
            'Stats' = paste('The minimum value is', min(1:10)),
            'Source' = "Some db"
            )
          )

*As one may notice, the doc object is build up as a list. Thus it makes it possible to e.g. include variable statistics that updates automatically.* 

To get the documentation (as a list):

    d <- a$get_doc("foo")
    
    d$Info$Stats
    # [1] "The minimum value is 1"

### File structure

The coldbir database consists of folders where each folder represent a variable. 
Each variable may have several dimensions, e.g. months and years. 
The data is stored as a [column-oriented DBMS](http://en.wikipedia.org/wiki/Column-oriented_DBMS). 
Below is an example of a database, named *mydb*, with a couple of variables:

    mydb/
      income/
        data/
          d[2011].cdb.gz
          d[2012].cdb.gz
        documentation.yml
        lookup.txt
        readme.md
      unemployment/
        data/
          d[2011].cdb.gz
          d[2012].cdb.gz
        documentation.yml
        lookup.txt
        readme.md

The `doc.json` includes the variable documentation that is read with `get_doc()` method. `readme.md` includes the same information but in markdown.


## Examples

### Example: Survey data

#### Write data and documentation

Link to database

    a <- cdb()

Add survey data to database

    require(MASS)
    a["Age"] <- survey$Age
    a["Pulse"] <- survey$Pulse

Add documentation for `Age` and `Pulse`

    a["Age"] <- doc(
        title = "Age",
        description = "Age of the student in years."
        )

    a["Pulse"] <- doc(
        title = "Pulse",
        description = "Pulse rate of student (beats per minute)."
        )
        
#### How to use

Scatter plot

    plot(a["Age"], a["Pulse"],
         xlab = a$get_doc("Age")$description, 
         ylab = a$get_doc("Pulse")$description
         )

Histogram

    d <- a$get_doc("Pulse")
    hist(a["Pulse"], main = paste("Histogram of", d$title), xlab = d$title, sub = d$description)

## Development

### Git branches

The *dev* branch is the development branch, and might therefore be a bit unstable. *master* is the latest stable version and larger releases are marked with tags, e.g. v1.0, where the first number represents a new stable release and the second number imply new bug fixes within the given release version.

### Build package and run tests

Use the `makefile` to run tests and to build the package:

- `make build`: to build the package
- `make install`: to build and install the package
- `make check`: to build and check (cran requirements) the package
- `make test`: to run the package tests

The `testthat` package is required to run the package tests and the related test code is available in `inst/tests/testthat/`. The build/test structure of this package takes a lot of inspiration from the [pander](https://github.com/Rapporter/pander) package - many thanks to its developers!

## License

Coldbir is licensed under the AGPLv3, the terms of which are included in the file LICENSE.
