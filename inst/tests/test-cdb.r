# rm(list=ls());gc()
library(testthat)


path <- tempfile()

cat(chartr("\\","/",path),"\n")

# PREPARE DATA
set.seed(1234)
size <- 10000L


# Prepare a "LONG" data set
VARS      <- paste("myVar",1:5,sep="")
SELECTORa <- paste("test",1:2,sep="")
YEARS     <- rep(2003:2005)
MONTHS    <- 1:12

testSet <- 
  expand.grid(    var =  VARS , 
                  sel = SELECTORa, 
                  y = YEARS, 
                  m = MONTHS,
                  value = 1:size,
                  stringsAsFactors = F)
cns <- names(testSet)
attributes(testSet) <- NULL
names(testSet) <- cns

testSet <- as.data.table(testSet)
testSet[,value := as.integer(round(rnorm(.N)*10,0))]
# "LONG" format ready

# Prepare a "WIDE" data set
x <- copy(testSet)
x[, var_sel_y_m := 
    apply(testSet[,
                  .(var, sel , y = as.character(y), m = as.character(m))],
                  1, FUN=paste, collapse = "_")
            ][,var := NULL][,sel := NULL][,y:=NULL][,m:=NULL]
# alternartive using tidyr x <- data.table(unite_(testSet, "var_sel_y_m", c("var","sel","y","m")))

setkeyv(x,"var_sel_y_m")
x <- x[,id := 1:(.N), keyby=var_sel_y_m]
testSetWide <- dcast.data.table(x,id ~ var_sel_y_m)
#str(testSetWide)
# "WIDE" format ready
##############################


context("INITIALIZE DATABASE")

test_that("initialize database", {
  db <- cdb(path, read_only = T)
  expect_equal(path, db$path)
  expect_true(db$read_only)
  
  db <- cdb(path, read_only = F)
  expect_false(db$read_only) 
  
  db <- cdb(path)
  expect_false(db$read_only)  
})

context("FILE HANDLING")

test_that("saving & reading data.table", {
  db <- cdb(path, read_only = F)
  cat("writing:", paste(round(system.time(db[] <- testSet)[1:3],3),"s"),"\n")
  cat("reading:", paste(round(system.time(x <- db[] )[1:3],3),"s"),"\n")
  
  x[,var := as.character(var)][,sel := as.character(sel)] # dtrings are savet as factors
  setcolorder(x,names(testSet))  # always alphabetical order
  expect_equal(x,testSet)
  db[] <- NULL
})

test_that("LONG data reading and manupilating another instance", {
  db1   <- cdb(path, read_only = F)  # two 
  db2   <- cdb(path, read_only = F)  # instances
  
  cat("writing:", paste(round(system.time(db1[] <- testSet)[1:3],3),"s"),"\n")
  
  cat("reading1:", paste(round(system.time(x <- db1[])[1:3],3),"s"),"\n")
  cat("reading2:", paste(round(system.time(y <- db2[] )[1:3],3),"s"),"\n")
  expect_equal(x, y)  
  
  db1["m"] <- NULL
  expect_null(db2["m"])
  
  expect_equal(db2[],db1[]) # equal dure to syncronisation
  
  db1["m"] <- testSet$m
  
  expect_equal(db1["m"], db2["m"]) # equal dure to syncronisation
  
  db1[] <- NULL
  db2[] <- NULL # not necessary
  expect_null(db2[])
})


test_that("WIDE data reading and manupilating another instance", {
  db1   <- cdb(path, read_only = F)  # two 
  db2   <- cdb(path, read_only = F)  # instances
  
  cat("writing:",  paste(round(system.time(db1[] <-  testSetWide)[1:3],3),"s"),"\n")
  
  T <- round(system.time(x <- db2["*Var*", .("test1", 2001,._)])[1:3],3)
  cat("reading ", ncol(x) ,"cols T = ", paste(T, " s",sep=""),"\n")
  # str(x)

  T <- round(system.time(x <- db2["*Var*", .(._, ._,1:10)])[1:3],3)
  cat("reading ", ncol(x) , "cols T = ", paste(T, " s",sep=""),"\n")  
  
  T <- round(system.time(x <- db2[._, ._])[1:3],3)
  cat("reading ", ncol(x) , "cols T = ", paste(T, " s",sep=""),"\n")  
  
  db2[] <- NULL
})



test_that("wild cards test", {
    db1   <- cdb(path, read_only = F) 
    db1[]   <- testSetWide
    T <- round(system.time(x <- db1["*V*",.(._, ._, 9)])[1:3],3)
    cat("reading ", ncol(x) , "cols",db1$n_row, "rows ", "T =", paste(T, " s",sep=""),"\n") 
    db1[] <- NULL
})


# SLUT
# *** MERA TESTER AV DEN NYGAMLA COLDBIR BEHÖVS NEDAN ***


db[] <- MASS::survey

db2 <- cdb(path, read_only = F)

db[] <- NULL


db2[]

db2[] <- MASS::survey

db[]



db["Age*"] <- NULL

db["Age2",.("test1",2011,1)] <- MASS::survey[,"Age"]
db["Age2",.("test1",2011,2)] <- MASS::survey[,"Age"]
db["Age2",.("test1",2011,3)] <- MASS::survey[,"Age"]
db["Age2",.("test1",2012,1)] <- MASS::survey[,"Age"]
db["Age2",.("test1",2012,2)] <- MASS::survey[,"Age"]
db["Age2",.("test1",2012,3)] <- MASS::survey[,"Age"]
db["Age2",.("test2",2011,1)] <- MASS::survey[,"Age"]
db["Age2",.("test2",2011,2)] <- MASS::survey[,"Age"]
db["Age2",.("test2",2011,3)] <- MASS::survey[,"Age"]
db["Age2",.("test2",2012,1)] <- MASS::survey[,"Age"]
db["Age2",.("test2",2012,2)] <- MASS::survey[,"Age"]
db["Age2",.("test2",2012,3)] <- MASS::survey[,"Age"]
db["Age2",.("test1",2055,1)] <- MASS::survey[,"Age"]
db["Age"]<-NULL

db <- cdb(path, read_only = F)

names(db["Age*",.(.("test1","test2"))])

db["Age2",.("test2",2012,4)] <- MASS::survey[,"Age"]

db["Age2",.("test3",2011,1)] <- MASS::survey[,"Age"]

db["A*2",.(._,._,1)]

db["A*",.("test1",._,1)]
db["A*",.("test1", 2012, ._)]

db["A*",c("test1", 2012, ._)] #FEL

db["Age*",.(._, 2011, c(1,3))]

db["A*",.(c("test1","test2"),2011,c(1,3))]

db[]

db["Age*",.(._,2011,._)] <- NULL

db["A*2",.all]

if(FALSE)
{

  table(as.integer(MASS::survey[,"Sex"]))
  table(db["Sex"][[1]])
  
  expect_equal(db[][,names(MASS::survey),with=FALSE],MASS::survey)

levels(db["Sex"][[1]])

MASS::survey[,"Sex"]

sum(as.integer(db["Sex"][[1]]) != as.integer(MASS::survey[,"Sex"]))
sum(as.integer(db["Sex"][[1]]) != as.integer(MASS::survey[,"Sex"]))
table(as.integer(db["Sex"][[1]]))

length(db["Sex"][[1]])
length((as.integer(MASS::survey[,"Sex"])))
nrow(MASS::survey)

}

db["W*.Hnd"]

test_that("re-initialize database", {
  expect_equal(as.data.frame(db[]), MASS::survey)
})

db2 <- cdb(path, read_only = F)

test_that("re-initialize database", {
  expect_equal(db[], db2[])
})


db2[] <- NULL
db2 <- NULL

db$clean()

is.na(db1$db_nrow())
context("DATABASE CONFIG")
##########################


context("DATABASE VARIABLE LENGTH")
###################################
test_that("nrow when database is empty", {
  expect_true(is.na(db2$n_row)
})

db["x"] <- sample(1:5, size, replace = T)
test_that("nrow when database contains one variable without any dimentions", {
  expect_equal(db$db_nrow(), size)
})
db$clean()

db["x"] <- 1:3
test_that("throw error if assigning non-allowed variable length", {
  expect_error({db["x"] <- 1:2})
  expect_error({db["y"] <- 1:2})
})
db$clean()

context("VARIABLE TYPES")
#########################
x <- sample(c(T, F), size, replace = T)
db["x"] <- x
test_that("logical", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(c(T, F, NA), size, replace = T)
db["x"] <- x
test_that("logical na", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(c(0, 1, 10000, .Machine$integer.max, NA), size, replace = T)
db["x"] <- x
test_that("integer", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(c(-100, -50, 0, 50, 100, NA), size, replace = T)
db["x"] <- x
test_that("double", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

x <- sample(LETTERS, size, replace = T)
db["x"] <- x
test_that("character", {
  expect_equal(as.factor(x), db["x"][[1]])
})
db$clean()

db["x"] <- x <- as.factor(c(NA, NA))
test_that("factor with only na", {
  expect_equal(x, db["x"][[1]])
})
db$clean()

# Test if escape characters works
db["x"] <- x <- c("a\n", "\tc\v\n", "d\a\vx\ry\f\tz")
test_that("escape_char", {
  expect_equal(escape_char(x), as.character(db["x"][[1]]))
})
db$clean()

db["x"] <- x <- .POSIXct(runif(size) * unclass(Sys.time()))
test_that("POSIXct", {
  expect_equal(as.character(x), as.character(db["x"][[1]]))
})
db$clean()

test_that("non-existing", {
  expect_true(is.null(db["non-existing"]))
})

context("VARIABLE DOCUMENTATION")
#################################
db["x"] <- doc(a = 1, b = "c")
test_that("add docs as parameters", {
  expect_equal(list(a = 1, b = "c"), db$get_doc("x"))
})
db$clean()

x <- list(a = "text", b = list(c = 1:3, d = 4), c = "åäö")
db["x"] <- doc(x)
test_that("add docs as a list", {
  expect_equal(x, db$get_doc("x"))
})
db$clean()

x <- list(b = 1, c = 2)
db["x"] <- doc(a = x)  # special case
test_that("add docs as one parameter that includes a list", {
  expect_equal(list(a = x), db$get_doc("x"))
})
db$clean()

context("VARIABLE DIMENSIONS")
##############################
dims <- c(2012, "a")
db["x", dims] <- x <- sample(1:5, size, replace = T)
test_that("put/get variable with dimensions", {
  expect_equal(x, db["x", dims][[1]])
  expect_true(file.exists(file.path(db$path, "x", "data", "d[2012][a].cdb.gz")))
})
db$clean()

dims <- NULL
db["x", dims] <- x <- sample(1:5, size, replace = T)
test_that("put/get variable with dims = NULL", {
  expect_equal(x, db["x", dims][[1]])
  expect_true(file.exists(file.path(db$path, "x", "data", "d.cdb.gz")))
})
db$clean()

test_that("non-existing dimensions", {
  expect_true(is.null(db["non-existing", dims]))
})

context("REPLACE NA")
#####################
db["x"] <- x <- c(T, F, NA, F, T)
test_that("logical replaces NA", {
  expect_equal(sum(x, na.rm = T), sum(db["x", na = F][[1]]))
})
db$clean()

db["x"] <- x <- c(1, NA, 3)
test_that("integer replaces NA", {
  expect_equal(1:3, db["x", na = 2][[1]])
})
db$clean()

db["x"] <- x <- c("a", NA)
test_that("character replaces NA", {
  expect_equal(as.factor(c("a", "b")), db["x", na = "b"][[1]])
})
db$clean()

context("DATASETS")
###################
x <- data.table(MASS::survey)[, list(
  Sex, Fold, Pulse, Clap, Exer, Smoke, Height, Age
)]

# In addition we change the column names
# to specifically test issue 49.
setnames(x, c("Smoke", "Height", "Age"), c("var", "x", "z"))

db[] <- x

setcolorder(x, sort(names(x)))

test_that("get dataset", {
  expect_equal(x, db[])
})

test_that("select multiple variables", {
  expect_equal(x[, list(Clap, Pulse)], db[c("Clap", "Pulse")])
})

db$clean()

x1 <- cdb(tempfile())
x2 <- cdb(tempfile())
x1[] <- MASS::survey
x1[, 2012] <- MASS::survey
x1[, c(2012,12)] <- MASS::survey
x2[] <- x1[]

test_that("Copy database", {
  expect_equal(x1[], x2[])
})

x1$clean()
x2$clean()

context("READ ONLY")
####################
db$read_only <- T

test_that("put variable", {
  expect_error({ db["x"] <- 1:10})
})

test_that("put docs", {
  expect_error({ db["x"] <- doc(a = 1, b = 2) })
})

test_that("put config", {
  expect_warning(db$put_config())
})

test_that("clean", {
  expect_error(db$clean())
})

db$read_only <- F
db$clean()

context("LOOKUP TABLES")
########################
db["x", "a"] <- a <- c("b", "c", "c", "b")
db["x", "b"] <- b <- c("a", "b", NA, NA)
db["x", "c"] <- c <- c("d", "c", NA, "c")
db["x", "d"] <- d <- rep("c", 4)
db["x", "e"] <- e <- rep(as.character(NA), 4)
test_that("Different lookup tables between dimensions", {
  expect_equal(a, as.character(db["x", "a"][[1]]))
  expect_equal(b, as.character(db["x", "b"][[1]]))
  expect_equal(c, as.character(db["x", "c"][[1]]))
  expect_equal(d, as.character(db["x", "d"][[1]]))
  expect_equal(e, as.character(db["x", "e"][[1]]))
})

db$clean()
