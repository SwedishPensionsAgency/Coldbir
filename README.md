# coldbir

## Installation

Use `devtools` for easy installation

    # install.packages("devtools")
    devtools:::install_github('coldbir', 'SthlmR')
    library(coldbir)

## Introduction

Below is an introduction on how to simply use coldbir.

### Variables

Write a variable to disk:

    x <- 1:10  # example vector
    put_v(x, "variable-name")

Read a variable from disk:

    x <- get_v("variable-name")

### Databases

Write a database to disk:

    x <- MASS::survey  # example data frame
    put_db(x, "db-name")

Read a database from disk:

    get_db("db-name")

### File structure

The coldbir database consists of folders where each folder represent a variable. Each variable may have several dimensions, e.g. months and years. The data is stored as a [column-oriented DBMS](http://en.wikipedia.org/wiki/Column-oriented_DBMS). Below is an example of a database, named *mydb*, with a couple of variables:

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
