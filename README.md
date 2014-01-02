# Coldbir

Coldbir is a column database in R. The main purpose of this package is to simplify the workflow with panel data on disk, including features such as:

- Simple syntax to work with data
- Small storage size
- Impressive read and write speed
- Variable documentation
- Support for various data types

The Coldbir database could be seen as a large table including a lot of columns. The data itself is stored as a [column-oriented DBMS](http://en.wikipedia.org/wiki/Column-oriented_DBMS), where each individual column, also called `variable`, has its own folder including data, documentation and lookup files. A variable data could also be divided into different dimensions (e.g. months and years), hence making it possible to store time series data. *As a notation, this feature could also be used to improve read performance by pre-aggreggating values into years, when data is originally divided into months.*

Below is an example of a database, named *mydb*, including variables on `income` and `unemployment` and year-month as dimensions:

    mydb/
      income/
        data/
          d[2012][12].cdb.gz
          d[2013][12].cdb.gz
        documentation.json
        lookup.txt
      unemployment/
        data/
          d[2012][12].cdb.gz
          d[2013][12].cdb.gz
        documentation.yml
        lookup.txt
        
## Getting started

The package is currently not available on `CRAN`, therefore make sure to use `devtools` when installing the package:

    devtools::install_github('Coldbir', 'SwedishPensionsAgency')

Then, to access or create a database, one has to first initialize a connection:

    library(Coldbir)
    a <- cdb('mydb')
    
The package make use of `get` and `put` methods to read and write data, somewhat simplied the syntax is `a[] <- x` to put data `x` on to disk and then `a[]` to read from disk. The `[]` notation is used for data selection, e.g. to define which variable and dimensions to read, see the API section below for more details.

## API

First, make sure to load the package with `library(Coldbir)`.

Method                                  | Query example
--------------------------------------- | -------------
Initialize database                     | `a <- cdb('mydb')`
Unlock read only                        | `a$read_only <- FALSE`
Get database path                       | `a$path`
                                        | 
Put variable                            | `a['foo'] <- 1:10`
Put variable with dimension             | `a['foo', 2013] <- 1:10`
Put variable with multiple dimensions   | `a['foo', c(2013, 12)] <- 1:10`
Put data.frame                          | `a[] <- MASS::survey`
Put data.frame with dimensions          | `a[, c(2013, 12)] <- MASS::survey`
Put variable documentation              | `a['foo'] <- doc(title = "Foo", desc = "Bar")`
Put variable documentation (list)       | `a['foo'] <- doc(list(title = "Foo"))`
Put config file                         | `a$put_config()`
                                        | 
Get variable                            | `a['foo']`
Get multiple variables                  | `a[c('foo', 'bar')]`
Get variable with specific dimensions   | `a['foo', c(.ANY, 12)]`
Get data of dimensionality of two       | `a['foo', c(.ANY, .ANY)]`
Get all data                            | `a[]`
Get variable documentation              | `a$get_doc("foo")`
                                        | 
Delete variable                         | `a['foo'] <- NULL`
Delete specific dimension               | `a['foo', c(2013, 12)] <- NULL`
Delete all database content             | `a$clean()`

The documentation object has its own class `doc` and is constructed as a list.

## Supported data types

The package currently support the following data types:

- `integer`
- `double`
- `logical`
- `factor`
- `Date`
- `POSIXct` / `POSIXlt` ** **EXPERIMENTAL** **

Timezones are not supported. All timestamps are written as `GMT` without timezone conversion. E.g. `2013-04-29 01:00:00 CST` is stored (and returned) as `2013-04-29 01:00:00 GMT`. `POSIXlt` is automatically converted to `POSIXct`.

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

## See also

- [bigmemory](http://www.bigmemory.org/)
- [ff](http://ff.r-forge.r-project.org/)

## License

Coldbir is licensed under the AGPL-3, for more information on the license please read: [http://www.r-project.org/Licenses/AGPL-3](http://www.r-project.org/Licenses/AGPL-3).
