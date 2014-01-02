# Coldbir

The Coldbir package is a column database in R. The main purpose of this package is to simplify the workflow with panel data on disk, including features such as:

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
    
Then the package use `get` and `put` methods to read and write data from/to disk, which could be somewhat simplied as:

- put: `a[] <- x`
- get: `a[]`

where `x` is some data. The `[]` notation is used for data selection, e.g. to define which variable and dimensions to read.

## API

First, make sure to load the package with `library(Coldbir)`.

**Initialize database**

    a <- cdb('mydb')
    
**Read/write a vector**

- put: `a['foo'] <- 1:10`
- get: `a['foo']`

**Read/write a data frame**

- put: `a[] <- MASS::survey`
- get: `a[]`

**Variable documentation**

The documentation object has its own class `doc` and is constructed as a list.

- put:

```
a['foo'] <- doc(
  'Foo' = 'This is a variable', 
  'Info' = list(
    'Stats' = paste('The minimum value is', min(1:10)),
    'Source' = "Some db"
  )
)
```

- get:

```
d <- a$get_doc("foo")

d$Info$Stats
# [1] "The minimum value is 1"
```

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
