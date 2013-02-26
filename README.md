# Coldbir
-----------------------

The Coldbir package is a column-oriented DBMS in R.

The project is currently maintained by Thomas Reinholdsson (<reinholdsson@gmail.com>).

## Installation

Use `devtools` for easy installation

    library(devtools)
    install_github('Coldbir', 'SthlmR', 'v0.2')

## Introduction

First, make sure to load the package with `library(Coldbir)`.

Then the next step is to decide where the database is to be saved, or where it already exists:

    a <- cdb('database_path')

To save data to the database one simply write:

    a['foo'] <- 1:10

and to get the variable it is just to use:

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
- `Date` (v0.3)
- `POSIXct` / `POSIXlt` (v0.3)

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
        lookup.txt
        readme.md
      unemployment/
        data/
          d[2011].cdb.gz
          d[2012].cdb.gz
        lookup.txt
        readme.md


## Development

The *master* branch is the development branch, and might therefore be a bit instable. Stable releases are marked with tags, e.g. v1.0, where the first number represents a new stable release and the second number imply new bug fixes within the given release version.


## License

Coldbir is licensed under the AGPLv3, the terms of which are included in the file LICENSE.
