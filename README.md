# Coldbir
-----------------------

The Coldbir package is a column-oriented DBMS in R.

The project is currently maintained by Thomas Reinholdsson (<reinholdsson@gmail.com>).

## Installation

Use `devtools` for easy installation

<<<<<<< HEAD
    library(devtools)
    install_github('Coldbir', 'SthlmR', 'v0.2')
=======
```r
library(devtools)
install_github('Coldbir', 'SthlmR', 'v0.3')
```

>>>>>>> Update readme

## Introduction

First, make sure to load the package with `library(Coldbir)`.

Then the next step is to decide where the database is to be saved, or where it already exists:

    a <- cdb('database_path')

To save data to the database one simply write:

    a['foo'] <- 1:10

and to get the variable:

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

### Variable documentation

An additional feature is to add documentation to a variable. 

Create an object of the `doc` class and add it to a variable:

```r
a['foo'] <- doc(
      'Foo' = 'This is a variable', 
      'Info' = list(
        'Stats' = paste('The minimum value is', min(1:10)),
        'Source' = "Some db"
        )
      )
```

*As one may notice, the doc object is build up as a list. Thus it makes it possible to e.g. include variable statistics that updates automatically.* 

To get the documentation (as a list):

```r
d <- get_doc(a, "foo")

d$Info$Stats
# [1] "The minimum value is 1"
```

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
        doc.json
      unemployment/
        data/
          d[2011].cdb.gz
          d[2012].cdb.gz
        lookup.txt
        readme.md
        doc.json

The `doc.json` includes the variable documentation that is read with `get_doc()` method. `readme.md` includes the same information but in markdown.


## Examples

### Example 1: Survey data

#### Write data and documentation

Link to database
```r
a <- cdb()
```

Add survey dataset to database
```r
a[] <- MASS::survey
```
Add documentation for `Age` and `Pulse`
```r
a["Age"] <- doc(
    title = "Age",
    description = "Age of the student in years."
    )

a["Pulse"] <- doc(
    title = "Pulse",
    description = "Pulse rate of student (beats per minute)."
    )
```
#### Use data and documentation with plots

Scatter plot
```r
plot(a["Age"], a["Pulse"],
     xlab = get_doc(a, "Age")$description, 
     ylab = get_doc(a, "Pulse")$description
     )
```

Histogram
```r
d <- get_doc(a, "Pulse")
hist(a["Pulse"], main = paste("Histogram of", d$title), xlab = d$title, sub = d$description)
```

## Development

The *master* branch is the development branch, and might therefore be a bit instable. Stable releases are marked with tags, e.g. v1.0, where the first number represents a new stable release and the second number imply new bug fixes within the given release version.


## License

Coldbir is licensed under the AGPLv3, the terms of which are included in the file LICENSE.
