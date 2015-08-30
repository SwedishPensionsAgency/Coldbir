#' Assign new (or existing) coldbir database
#' 
#' Method to assign either a new or existing coldbir database to an R object.
#' The current working directory is set as the default path.
#' 
#' @param path database path; the location of your new or existing Coldbir database,
#' where the last folder name of the path is the name of the database,
#' e.g. `a <- cdb('data/MyCDB')` (default: `tempfile()`) 
#' @param compress file compression level
#' @param encoding set documentation encoding (default: UTF-8)
#' @param read_only read only (default: T)
#' 
#' @examples a <- cdb()
#' @export
cdb <- setRefClass(
  "cdb",
  fields = list(
    path         = "character",
    instanceID   = "numeric",
    compress     = "integer",
    encoding     = "character",
    read_only    = "logical",
    db_version   = "numeric",
    n_row        = "integer",
    status       = "integer",
    currHashCode = "character",
    variables    = "ANY",
    ftabC        = "character", # longFileName to table of content
    fconF        = "character"  # longFileName to config file
  ),
  methods = list(
    initialize = function(
      path      = tempfile(),
      compress  = 5L,
      encoding  = "UTF-8",
      read_only = as.logical(NA)
    ) {
      
      # Set parameters
      .self$path        <- path
      .self$instanceID  <- Sys.getpid() + as.double(Sys.time()) * runif(1) # unique ID
      .self$compress    <- compress
      .self$encoding    <- encoding
      .self$read_only   <- read_only
      .self$n_row       <- 0L
      .self$variables   <- data.table()
      .self$currHashCode<- digest(.self$variables,"xxhash64")
      
      .self$ftabC <- file.path(.self$path, .tableOfContens_filename)
      .self$fconF <- file.path(.self$path, .config_filename)
      
      # Check and administrate config files
      
      if(!file.exists(.self$fconF)){
        doTheUpdate <- TRUE
        if(is.na(.self$read_only)) .self$read_only <- FALSE #default for a new storage
      } else {
        
        readonlyArg <- .self$read_only
        get_config()
        
        if(!is.na(readonlyArg))
        {  #forcing the new principle according current initialization
          .self$read_only   <- readonlyArg
          put_config()
        }
        
        if(!file.exists(.self$ftabC)) {
          doTheUpdate <- TRUE
        } else {
          get_tableOfContents()
          hashOfToC <- digest(.self$variables,"xxhash64")
          if(hashOfToC != .self$currHashCode) {
            doTheUpdate <- TRUE  
          } else {
            # the configuration file and TableOfContent match => no more check (by default) 
            doTheUpdate <- FALSE   
            
          }
        }    
      }
      
      if(doTheUpdate){  # updating table of contents without changing the database
        rollback <- TRUE
        
        while(rollback) {
          block_database()
          if(get_blocking_instance() ==  .self$instanceID) {
            update_tableOfContent_from_files()
            put_tableOfContents()
            
            .self$currHashCode<- digest(.self$variables,"xxhash64")
            if(get_blocking_instance() == .self$instanceID) {
              put_config(status = .STATUS_EVAILABLE_FOR_EDITING)
              rollback <- FALSE
            }
          } 
        }     
      }

  
    },
 
    #' Get table of contents
    #' 
    get_tableOfContents = function() {
      

      
      if(file.exists(.self$ftabC)){
        .self$variables <- readRDS(.self$ftabC)
        
        if(nrow(.self$variables) == 0L) {
          .self$n_row <- 0L
          return(TRUE)
        }
        # check nr of observations
        firstFile <- unlist(.self$variables[1,])
        vName  <- firstFile[1]
        vDims  <- unlist(firstFile[-1])

        .self$n_row <-  length(.self$get_variable(name = vName, dims = vDims)) #TOO SLOW, RECODE!
        return(TRUE)
      } else {
        update_tableOfContent_from_files()
      }
    },

    #' put table of contents
    #' 
    put_tableOfContents = function() {
      
      saveRDS(.self$variables, .self$ftabC)
 
    },
    
    #' Save configuration settings to disk
    #' 
    #' Note: The config file can be updated even if the database is read only,
    #' as it would otherwise be more difficult to actually change the same option.
    put_config = function(status = .STATUS_EVAILABLE_FOR_EDITING) {
      if (.self$read_only) wrn(1)
      
      if (is.na(file.info(.self$path)$isdir)) {
        dir.create(.self$path, recursive = T)
      }
      
      if(status == .STATUS_EVAILABLE_FOR_EDITING) holder <- 0.0 else holder <- .self$instanceID 
      
      .self$db_version <- new_time_stamp()
      .self$currHashCode <- digest(.self$variables, algo = "xxhash64")
      dta <- list(
        read_only              = .self$read_only,
        db_version             = .self$db_version,
        db_tableOfContents     = .self$currHashCode,
        n_row                  = .self$n_row,
        status                 = status,
        holder                 = holder
        
      )
      
      saveRDS(dta, file = .self$fconF)
    },
    
    #' Get configuration settings from disk
    get_config = function(overwrite = TRUE){
      
      if (file.exists(.self$fconF)) {
        
        dta <- readRDS(file = .self$fconF)
        
        if(overwrite){
          # Set field values
          .self$read_only        <- dta$read_only
          .self$db_version       <- dta$db_version
          .self$currHashCode     <- dta$db_tableOfContents
          .self$n_row            <- dta$n_row
          }
        return(list(status = dta$status, holder = dta$holder))
        
      } else { 
        put_config() #initiate
        return (NULL)
      } 
    },
    
    
    update = function(){
      # assert actuality of the table of contents
      prevCode <- .self$currHashCode
      .self$get_config()
      if(.self$currHashCode != prevCode) get_tableOfContents()
      
    },
    
    #' Put variable documentation to disk
    #'
    #' Write documentation of a variable to disk.
    #'
    #' @param x documentation string
    #' @param name Variable name
    #' 
    put_doc = function(x, name) {
      
      f <- file_path(name, .self$path, create_dir = T, file_name = F, data_folder = F)
      f <- file.path(f, .doc_file)
      
      # Create temporary file
      tmp <- create_temp_file(f)
      
      tryCatch(
        {
          con <- file(tmp, encoding = .self$encoding)
          writeLines(x, con)
          close(con)
          
          file.copy(tmp, f, overwrite = T)
        },
        finally = file.remove(tmp),
        error = function(e) {
          msg(2, name, e)
        }
      )
      
      return(T)
    },
    
    #' Get variable documentation from disk
    #'
    #' Read documentation of a variable from disk.
    #'
    #' @param name Variable name
    #' 
    get_doc = function(name) {
      
      f <- file_path(name, .self$path, create_dir = F, file_name = F, data_folder = F)
      if (is.na(f)) {
        wrn(3, .self$path); return(NULL)
      }
      
      f <- file.path(f, .doc_file)
      if (!file.exists(f)) {
        wrn(4, .self$path, name); return(NULL)
      }
      
      con <- file(f, "r", encoding = .self$encoding)
      lns <- readLines(con, n = -1, warn = F)
      close(con)
      
      d <- paste(lns, collapse = "\n")
      d <- RJSONIO::fromJSON(d)
      return(d)
    },
    
    #' Add entries in the table of contents (variables)
    #' 
    #' @param new_entries
    add_entries = function(new_entries = NULL){
      
      
      ## ASSERTIONS
      # Anything at all?
      if(is.null(new_entries)) return(NULL)
      # Proper formated and iis so any new entries?
      if(!is.data.table(new_entries) || nrow(new_entries)==0L) return(NULL)
      # Proper columnnames ?
      # later ...
      # Assert uniqueness
      new_entries <- unique(new_entries)
      new_entries <- as.data.table(lapply(new_entries, as.character))
      
      if(nrow(.self$variables)==0L) {
          .self$variables <- new_entries
      } else {
      
        nPrev <- nrow(.self$variables)
        
        l <- list(.self$variables, new_entries)
        l <- unique(rbindlist(l, fill = TRUE))
        
        if(nrow(l) == nPrev) return(NULL)
        
        
        .self$variables <- l # replace
      }
      
      setkeyv(.self$variables, names(.self$variables))
      return(NULL)
      
    },
    
    #' Match the data request with existing columns
    #' 
    #' @param name name
    #' @param dims dims
    data_selection = function(currRequest){
      
      if(is.null(currRequest) || (is.list(currRequest) && length(currRequest)== 0L) ){
        return(data.table()) # trivial case
      }
      
      update()
      
      if(nrow(.self$variables) == 0L) return(.self$variables)
      
      # do the selection on variables
      if(isALL(currRequest$VAR[1])) {
        dataSel <- .self$variables
      } else {
        currRequest$VAR <- unique(.self$variables$name[grep(glob2rx(currRequest$VAR),.self$variables$name)])
        dataSel <- .self$variables[SJ(name=currRequest$VAR)]
      }
      
      # do the selection on dimensions except missings
      if(isALL(currRequest$DIMS)) {
        return(dataSel)   
      } else {
        colNumbers     <- sapply(currRequest$DIMS,function(x) !all(is.na(x))) #filter non-NA:s
        sel            <- currRequest$DIMS[,colNumbers,with=F]
        sel            <- as.data.table(lapply(sel,as.character))
        setkeyv(sel,names(sel)); setkeyv(dataSel,names(sel))
        dataSel        <-  merge(sel,dataSel, all.x=F,all.y=F, by=names(sel)) # sel[dataSel]
        setcolorder(dataSel,sort(names(dataSel)))
        
      }

     return(dataSel)
      
    },
    
    #' Remove entries from table of content
    #' 
    #' @param Sel name
    #' @param dims dims
    del_entries = function(Sel = NULL ){
      
      if(is.null(Sel) || nrow(Sel) == nrow(.self$variables)) { 
        .self$variables<- data.table()
        .self$currHashCode<- digest(.self$variables,"xxhash64") 
        .self$n_row      <- 0L  
        return(NULL)           
      }
      
      if(nrow(Sel) == 0L) return(NULL) # no instances to remove
      
      setkeyv(Sel,names(Sel))
      .self$variables <- .self$variables[!Sel] # remove all instances of variable
      .self$currHashCode<- digest(.self$variables,"xxhash64") 
      
      return(NULL)
    },
    
    #' Init table of contentens from file structure
    #' 
    #' Update list representation from file
    #' 
    #' @param dims tells if column with dimensions is required
    update_tableOfContent_from_files = function() {
      
      .self$variables <- data.table() # empty table as starting point
      
      files <- search_files(path = .self$path)
      
      # Extract variable names
      vars = basename(dirname(dirname(files)))
      
      if (length(vars) > 0) {
        # Extract dims
        var_dims <- str_extract_all(files, "\\[(\\w*)\\]")
        
        # Remove square brackets and prepare lists
        var_dims <- lapply(var_dims, function(x) {
          x <- gsub("\\[", "", x)
          x <- gsub("\\]", "", x)
          if (length(x) == 0) x <- NA
          x <- as.data.table(as.list(x))
          setnames(x, paste("V",1:length(x),sep=""))
          return(x)
        })
        
        #bulid the table of dimensions
        var_dims <- rbindlist(var_dims,  use.names= TRUE, fill = TRUE)
        
        Tbl <- cbind(data.table(name=vars),var_dims)
        
        # empty the existing list of content
        del_entries()
        
        # create a new one
        add_entries(Tbl)
        
        
      }
      #register hash (even if data.table is empty)
      
      .self$currHashCode<- digest(.self$variables,"xxhash64")
      
      return(.self$currHashCode)
    },
    
    #' Block database when editing
    #' 
    block_database = function() {
      
      # Block while updating to avoid conflicts
      # no strategies for deadlocks (at least yet) since they are considered unusual
      # in targeted applications (most bulk loading of data for analysis)
      
      startT <-proc.time()[3]
      lastT <- 0
      repeat
      {
        bi <- get_blocking_instance()
        if(bi == 0 || bi == .self$instanceID) { # attempt to bloock
          
          if(bi == 0) put_config(status = .STATUS_REQUESTED_FOR_EDITING)
          
          Sys.sleep(0.01) #wait in case any other precess interferes (find proper value of delay)
          
          if(get_blocking_instance() == .self$instanceID) break  # still blocked by me? => proceed 
          
            #otherwise failure, thus repat waiting until the interapter releases database
        } else {
          
          waitT <- proc.time()[3] - startT  
          if(waitT-lastT > 1.0) {cat(".");flush.console();
                                 lastT <- waitT}
          if(waitT  > 10L) {
              # opportunistic, brutal approach:
              # for current applications it is fair enough
              # wait a while giving the other process time to finish the current task
              # after seconds without result: steal and force the competitor to rollback
              # issue: is wait time 10 seconds long/short enough?
            put_config(status = .STATUS_REQUESTED_FOR_EDITING)
              # issue for future release: queuing-principle
            
          }
          
        }
      }
      put_config(status = .STATUS_IN_EDITING )

      return(TRUE)
     },
    
    #' release database after editing
    #' 
    release_database = function() {
      # see block_database (above)
      put_tableOfContents()                                 # save representation
      put_config(status = .STATUS_EVAILABLE_FOR_EDITING)   
    },
    
    #' Get holder of a sucsessefull block, otherwise 0.0
    #'    
    get_blocking_instance = function () {
      
      cf <- get_config(overwrite = FALSE)
      
      if(is.null(cf)) return(0.0)
      
      if(  cf$status == .STATUS_REQUESTED_FOR_EDITING ||
           cf$status == .STATUS_IN_EDITING
           ) return(cf$holder) else return(0.0)
        
    },

    #' Delete variable data, only one column at one call 
    #' 
    #' @param name variable name
    #' @param dims the specified observation in the space of dimensions
     #' 
    delete_variable = function(i,j) {  
      
      currRequest <- rephraseDataRequest(i,j)   # deal with all wildcards and 
      
      if(length(currRequest)==0L) return(x)  #unclear what to do, thus do nothing and exit
      
      
      if(isALL(currRequest$VAR)) return(clean())

      
      obs2del <- data_selection(currRequest)
      
      if(nrow(obs2del) == nrow(.self$variables)) rmALL <- TRUE
 

      # Return NULL if variable doesn't exist
      if (nrow(obs2del) == 0) {
        # wrn(23, paste(names,"(",paste(obs2del$name,collapse=","),")",sep=""))
        return(NULL)
      }
      
      rollback <- TRUE
      
      while(rollback) {
        block_database()
        if(get_blocking_instance() ==  .self$instanceID) {
          
          if(nrow(obs2del) == nrow(.self$variables)){
            # Remove the whole directory if there is only one dimension
            # including the documentation
            unlink(file.path(.self$path, name), recursive = T)
            
          } else {
            
            for(i in 1:nrow(obs2del)){
              name <- obs2del[i,name]
              dim  <- obs2del[i,-1,with=F]
              dim  <- unlist(dim)
              if(!is.null(dim)){
                dim  <- dim[!is.na(dim)];if(length(dim) == 0L) dim <- NULL
              }
              cdbF  <- file_path(name, .self$path, dim, ext = c("cdb.gz", "cdb"), create_dir = F)
              
              if(file.exists(cdbF[1])){
                unlink(file.path(cdbF[1])) # compressed
              } else {
                unlink(file.path(cdbF[2])) # uncompressed
              }
              
              if(length(list.files(file.path(.self$path,name,"data"))) == 0L) unlink(file.path(.self$path,name), recursive = TRUE)
              
            }
            
            del_entries(obs2del)
            
           }        
          rollback <- FALSE      
          } 
      }

      release_database()
    },
    


    # Get variable data
    #'    
    #' @param name variable name
    #' @param dims the specified observation in the space of dimensions 
    #' @param na the value of missing values. NA by default. 
    #'     
    get_variable = function(name, dims = NULL, na = NA) {
      
      if(length(dims) == 0L || is.na(dims)) dims <- NULL
      
      if(!is.null(dims)){
        dims <- dims[!is.na(dims)]; if(length(dims)==0L) dims <- NULL
      }
      # Get file path
      cdbF <- file_path(name, .self$path, dims, ext = c("cdb.gz", "cdb"), create_dir = F)
      
      # Connect to compressed/uncompressed file
      if (file.exists(cdbF[1])) {
        bin_file <- gzfile(cdbF[1], "rb")
        
      } else if (file.exists(cdbF[2])) {
        bin_file <- file(cdbF[2], "rb")
        
      } else err(6, paste(name,dims,collapse=","))
      
      header_len <- readBin(bin_file, integer(), n = 1, size = 8)
      header_str <- rawToChar(readBin(bin_file, raw(), n = header_len))
      header <- RJSONIO::fromJSON(header_str, simplifyWithNames = F)
      
      vector_len <- readBin(bin_file, integer(), n = 1, size = 8)
      
      if (header$bytes <= 4) {
        x <- readBin(bin_file, integer(), n = vector_len, size = header$bytes)
      } else {
        x <- readBin(bin_file, double(), n = vector_len)
      }
      
      close(bin_file)
      
      # Check if using an old version of colbir
      if (header$db_ver != as.integer(.cdb_file_version)) err(7, name)
      
      # Prepare data depending on vector type
      
      ## integer or factor
      if (header$type == "integer") {
        
        if (!is.na(na)) x[is.na(x)] <- as.integer(na)
        
      } else if (header$type == "factor") {
        
        # Get lookup table, where values are to be used as levels
        lt <- get_lookup(name = name)
        
        if (!is.null(lt)) {
          
          # Check whether variable has any NA
          has_na <- any(is.na(x)) && !any(is.na(lt[[2]]))
          
          # Add one level for NA
          levels_len <- max(lt[[1]]) + as.integer(has_na)
          
          # Replace NA in data with factor level
          if(has_na) x[which(is.na(x))] <- levels_len   # NA values by leveles
          
          # Create factor levels
          levels <- rep(NA_character_, levels_len)  # assert odd cases, usually 1:nrow(lt) + occasional NA
          levels[lt[[1]]] <- lt[[2]]
          if(!is.na(na)) levels[is.na(levels)] <- na
          
          # Convert to factor variable
          levels(x) <- levels
          class(x) <- "factor"
          
        } else {
          
          # This is outside the `if (!is.null(df))` due to the 
          # odd case of a factor variable with only NA-values
          levels(x) <- character(0)
          class(x) <- "factor"
          
        }
        
      ## double
      } else if (header$type == "double") {
        if (!is.null(header$exponent)) 
          x <- x / 10^header$exponent
        if (!is.na(na))
          x[is.na(x)] <- as.double(na)
        
      ## logical
      } else if (header$type == "logical") {
        # NA's are stored as -1, thus they are replaced with NA
        x[x == -1L] <- NA
        
        # Replace 0/1 with TRUE/FALSE
        x <- (x > 0L)
        
        if (!is.na(na))
          x[is.na(x)] <- as.logical(na)
          
      ## Date
      } else if (header$type == "Date") {
        origin <- "1970-01-01"
        x <- as.Date(x, origin = origin)
        
      ## POSIXt
      } else if (header$type %in% c("POSIXct", "POSIXlt")) {
        origin <- as.POSIXct("1970-01-01 00:00:00", tz = .tzone)
        x <- as.POSIXct(x, tz = .tzone, origin = origin)
      }
      
      # Add attributes to vector
      if (!is.null(header$attributes)) {
        attributes(x) <- c(attributes(x), header$attributes)
      }
      
      return(x)
    },
    
    put_data_frame = function(x, name = NULL, dims = NULL){
      
      ### ASSERT
      if (read_only) err(8) # ERROR
      
      if(!is.data.frame(x)) return(NULL) # ERROR ...
      
      if(nrow(x) != .self$n_row && nrow(.self$variables)>0L) return(NULL) # ERROR ...
      
      cnames <- colnames(x)
      if (length(cnames) != length(unique(cnames)))  err(26) # ERROR
      
      # try do the task
      
      if(!is.data.table(x)) x <- as.data.table(x)
      
      rollback <- TRUE
      while(rollback) {
        block_database()
        if(get_blocking_instance() ==  .self$instanceID) {
          
          # ___ to do
         
          ## PARS var-names and dimensions
          lst <- str_split(cnames, .col_sep$regexp)  # first parse dimensions from names of coulumns
          
          if(is.null(name)){
            VARS <- unlist(lapply(lst, FUN = head, n = 1))
            lst <- lapply(lst, FUN = function(x) x[-1])
          } else {
            #var-names are explicit given. Use them instead for columnnames
            # columnnames are interpreted as dimensions in this case
            VARS <- rep(name, length.out=ncol(x))
          }
          
          if(is.null(dims)){
            # no explicit dimensions. Use columnnames
            DIMS <- lapply(lst, 
                           FUN = function(x) {if(length(x)==0L) NULL else x})
          } else {
            #explicit columnnames are provided
            dims <- rep(dims, length.out = ncol(x)) #  compatibility of lengths
            if(!is.list(dims)) dims <- as.list(dims) # list for uniform formating
            
            DIMS <- lapply(1:ncol(x), 
                           FUN = function(i) {c(lst[[i]],dims[[i]])}) #concatenate parsed with provided dimensions
          
            DIMS <- lapply(DIMS, 
                           FUN = function(x) { if(length(x) == 0) NULL else x}) #empty dim -> NULL
          
          }
          
          
          for(i in 1:ncol(x)) {
            put_variable_aux(
              x = x[[i]],
              name = VARS[i],
              dims = DIMS[[i]]
            )    
          }
          
          if(.self$n_row == 0L){ 
            .self$n_row <- nrow(x)
          } else if(.self$n_row != nrow(x)) err(11, name)
          
          # ___
          
          #save table oof content to the file:
          
          

          if(get_blocking_instance() == .self$instanceID) rollback <- FALSE # succes?
        } 
      } # roll back


      release_database() 

      return(T)
      
      
    },

    #' Put variable data
    #' 
    #' @param x vector of values. Should be of the same length as all the other variables in the database.
    #' @param name variable name
    #' @param dims the specified observation in the space of dimensions 
    #' 
    put_variable = function(x, name = NULL, dims = NULL) {
      

      rollback <- TRUE
      while(rollback) {
        block_database()
        if(get_blocking_instance() ==  .self$instanceID) {
          
          put_variable_aux(x,name,dims)
          
          if(get_blocking_instance() == .self$instanceID) rollback <- FALSE # succes?
        }
      }
      

      release_database() 
    },
  

    #' Put variable data
    #' 
    #' @param x vector of values. Should be of the same length as all the other variables in the database.
    #' @param name variable name
    #' @param dims the specified observation in the space of dimensions 
    #' 
    put_variable_aux = function(x, name = NULL, dims = NULL) {

      
      if (read_only) err(8)
      
      dimsV <- dims
      if(is.list(dimsV) && !is.null(dimsV)) dimsV <- unlist(dimsV)
      
      # If x is a data frame it will recursively run put_variable over all columns
      # this is the only way of adding columns regardless dimension-coding of column names
      # in other case the names are decoded using .col_sep - strings
      if (is.data.frame(x)) return(put_data_frame(x, name , dims ))
        
      # else
      {
        
        # Read object name if name argument is missing
        if (is.null(name)) name <- deparse(substitute(x))
        
        # Column seperators aren't allowed in column names
        # since it used as a seperator of name and dims
        if (length(grep(.col_sep$regexp, name)) == 0) {
          
          # if null => exit
          if (is.null(x)) {
            wrn(9, name)
            return(F)
          }
          
          if(.self$n_row == 0L || nrow(.self$variables)==0L){
            
            .self$n_row <- length(x)
            
          } else if(.self$n_row != length(x)) err(11, name)
          
          # Create empty header
          header <- list()
          
          # Check/set vector type and number of bytes
          if (is.numeric(x)) {
                
            if (is.integer(x)) {
              header$type <- "integer"
              header$bytes <- 4L  # H_itemSize, note: NA for integers is already -2147483648 in R
              
            } else if (is.double(x)) {
              header$type <- "double"
              header$exponent <- find_exp(x)
              
              if (header$exponent <= 9L) {
                x <- round(x * 10^header$exponent, 0)
                header$bytes <- check_repr(x)
                if (header$bytes <= 4L) {
                  header$bytes <- 4L
                }
              } else {
                header$bytes <- 8L
                header$exponent <- 0L
              }
            }
    
          } else if (is.logical(x)) {
            header$type <- "logical"
            header$bytes <- 1L
            
            # Replace integer value with NA,
            # unless -2147483648 is in the range
            x[is.na(x)] <- -1L
            
          } else if ("POSIXt" %in% class(x)) {  # OBS: must be checked before is.double
            header$type <- "POSIXct"
            header$bytes <- 8L  # save as double
            
            x <- lubridate::force_tz(x, .tzone)  # convert to GMT
            x <- as.double(x)  # convert to double
            
          } else if ("Date" %in% class(x)) {
            header$type <- "Date"
            header$bytes <- 8L
            
          } else if (is.factor(x) || is.character(x)) {
            if (is.character(x)) {
              x <- as.factor(x)  # convert to factor
            }
            
            # Get previous lookup table
            lookup <- get_lookup(name = name)
            
            # Get new levels (compared to lookup table)
            lvl <- levels(x)[!(levels(x) %in% lookup[[2]])]
            
            # Add new levels (if there are any)
            if (length(lvl) > 0) {
              
              # Set previous lookup table length              
              len <- nrow(lookup)
              if (is.null(len)) len <- 0
              
              # Create new lookup table or add to existing (cols: key, value)
              lookup <- rbindlist(list(
                lookup, 
                data.table((len + 1):(len + length(lvl)), lvl)
              ))
              
              # Write lookup table
              put_lookup(name = name, table = lookup)
            }
            
            # Convert variable (TODO: rewrite this part)
            if (!is.null(lookup)) {
              setnames(lookup, c("k", "v"))
              x_data <- data.table(k = as.integer(x), v = x, order = 1:length(x))
              x <- merge(x_data, lookup, by = "v", all.x = T, all.y = F)
              x <- x$k.y[order(x$order)]
            }
            
            header$type <- "factor"
            header$bytes <- 4L
                
          } else err(13, name)
          
          ext <- if (.self$compress > 0) "cdb.gz" else "cdb"
          
          # Construct file path
          cdbF <- file_path(
            name = name,
            path = .self$path,
            dims = dimsV,
            ext = ext,
            create_dir = T
          )
          
          # Check if we need a new entry in the in-memory representation
          file.existed <- file.exists(cdbF)
          
          # File header
          header$db_ver <- as.integer(.cdb_file_version)
            
          # Add attributes (reconsider)
          # header$attributes <- attrib #, attrib = NULL) from formel arg-list
            
          header_raw <- charToRaw(RJSONIO::toJSON(header, digits = 50))
          header_len <- length(header_raw)
          
          # Removes attributes from vector
          if (header$bytes == 8) {
            x <- as.double(x)
          } else {
            x <- as.integer(x)
          }
          
          # Create temporary file
          tmp <- create_temp_file(cdbF)
          
          # Try to write file to disk
           tryCatch(
           
           {
            
            # future issue: no check for two vesions of the file. One compressed one not
            # Create file and add file extension
            if (.self$compress > 0) {
              bin_file <- gzfile(tmp, open = "wb", compression = .self$compress)
            } else {
              bin_file <- file(tmp, "wb")
            }
 
             
            # Write binary file
            writeBin(header_len, bin_file, size = 8)
            writeBin(header_raw, bin_file)
            writeBin(length(x),  bin_file, size = 8)
            writeBin(x, bin_file, size = header$bytes)  # write each vector element to bin_file
            close(bin_file)
       
            # Rename temporary variable to real name (overwrite)
            file.copy(tmp, cdbF, overwrite = T)
            
             #browser()
            # * Add to in-memory list representation & ...
            
            if(is.null(dims)) dims <- data.table(V1=NA) else if (!is.data.table(dims)) names(dims) <-  paste("V", 1:length(dims), sep="")
            new_entry <- cbind(data.table(name=name), as.data.table(as.list(dims)))
            add_entries(new_entry )
            # * Update database version in config file 
            
            } ,
           
            finally = file.remove(tmp),
          
            error = function(e) { err(14, name, e)}
          
          )
          
          # Return TRUE if variable is successfully written
          

          
          return(T)
        } else wrn(25, name); return(F)
      }
    },
    
    #' Write lookup table to disk
    #'
    #' Write lookup table that represents variable data to disk.
    #'
    #' @param name Variable name
    #' @param table Two-column data table with keys and values
    put_lookup = function(name, table) {
      
      if (read_only) err(8)
      
      if (!is.data.frame(table) || ncol(table) != 2) err(16)
      
      # Escape characters
      table[[2]] <- escape_char(table[[2]])
      
      folder_path <- file_path(name, .self$path, create_dir = T, file_name = F, data_folder = F)
      f <- file.path(folder_path, .lookup_filename)
      
      write_lookup <- function() {
        # Write temporary doc file to disk
        write.table(table, file = tmp, quote = F, col.names = F, row.names = F, sep = "\t")
        
        # Rename temporary doc to real name (overwrite)
        file.copy(tmp, f, overwrite = T)
      }
      
      # Create temporary file
      tmp <- create_temp_file(f)
      
      # Try to write doc file to disk
      tryCatch(
        write_lookup(),
        finally = file.remove(tmp),
        error = function(e) {
          err(14, name, e)
        }
      )
      
      return(T)
    },
    
    #' Read dictionary from disk
    #'
    #' Read dictionary that represents variable data from disk.
    #'
    #' @param name Variable name
    get_lookup = function(name) {
      if (file.exists(file.path(.self$path, name))) {
        folder_path <- file_path(name, .self$path, create_dir = F, file_name = F, data_folder = F)
        file <- file.path(folder_path, .lookup_filename)
        
        if (file.exists(file)) {
          table <- read.table(file = file, header = F, quote = "", sep = "\t", stringsAsFactors = F)
          if (!is.data.frame(table) || ncol(table) != 2) {
            err(16)
          }
          return(as.data.table(table))
        } else { 
          return(NULL)
        }
      } else {
        return(NULL)
      }
    },
    
    attachCB = function(){
      attach.CB(.self) 
    },
    
    dettachCB = function(){
      deatach.CB(.self) 
    },
  
    
      
    #' Check if variable exist
    variable_exists = function(Nm) {
      Nm <- grep(glob2rx(currRequest$VAR),.self$variables$name)
      return(length(Nm)>0L)
    },
    
    
    #' Remove all content in database
    clean = function() {
      
      if (read_only) err(8)
      
      rollback <- TRUE
      
      while(rollback) {
        block_database()
        if(get_blocking_instance() ==  .self$instanceID) {
          
          # ___ to do
          unlink(file.path(path,"*"), recursive = T)  # order to remove all
          
          del_entries()
          # ___
      
          rollback <- FALSE # succes?
        } 
      } # roll back

      release_database() 

    }
  )
)

#' Extract content from variable
#' 
#' Function to extract data content from a Coldbir variable.
#' Overloads the `[`-method.
#' 
#' @param x cdb object
#' @param i variable name
#' @param j variable dims
#' @param na NA value (default: NA)
#' 
#' @name `[`
#' @docType methods
#' @rdname extract-methods
setMethod(
  f = "[",
  signature = "cdb",
  definition = function(x, i, j, ... ) {   
    # the first argument - named variables 
    #       vector or list or missing (or value == .all)
    #       list-notation . == list is allowed
    #       result:  a vector of variable names or value == .all
    #       look rephraseDataRequest() for details   
    
    # deal with all wildcards and loop up in the data list
    currRequest <- rephraseDataRequest(i,j,...)   # rephrasing
    tableToRead <- x$data_selection(currRequest) #  applying on existent data
    
    if (nrow(tableToRead ) > 0L) {
      
      # Create data.table with first variable
      
      Nm   <- tableToRead[1,name]
      Dms  <- unlist(tableToRead[1,-1,with=FALSE])
      
      v <- data.table(V1 = x$get_variable(name = Nm,dims = Dms))
      
      setnames(v, create_colname(Nm, Dms))
      
      # Add all other variables
      # Temporary function (since data.table otherwise think .self is
      # a column name, if that name is used)
      read_var <- function(...) x$get_variable(...)
      #
      
      if (nrow(tableToRead) > 1L) {
        for(i in 2:nrow(tableToRead)){
          
          Nm   <- tableToRead[i, name]
          Dms  <- unlist(tableToRead[i,-1, with=FALSE])
                         
          v[, create_colname(Nm, Dms) := read_var(Nm,Dms, na = NA), with = F]
          
        }
      }
    } else v <- NULL
    
    return(v)
  }
)

#' Assign content to variable 
#' 
#' Function to assign content, either data or documentation,
#' to a Coldbir variable. Overloads the `[<-`-method.
#' 
#' @param x cdb object
#' @param i variable name
#' @param j variable dims
#' @param value value
#'
#' @name `[<-`
#' @docType methods
#' @rdname replace-methods
setMethod(
  f = "[<-",
  signature = "cdb",
  definition = function(x, i, j, value){  # A["x",2001,2,4] <- NULL   ?????

    
    if (x$read_only) err(8)
    
    
    if( is.null(value)){ # clear variable if any
      

        x$delete_variable(i,j)

      return(x)
      
    } # else !is.null(value), thus new addition to database
    
    # treat one coolumn data.table/data.frame as a vector
    if(is(value, "data.frame") && ncol(value) == 1L) value <- value[[1]]
    
    if (is(value, "data.frame")) {

       x$put_data_frame(x = value)
      
      return(x)
      #______ data.frame/data.table is copied to trhe file representation
      
    } else if(is.vector(value)) {
      
      currRequest <- rephraseDataRequest(i,j)
      if(isALL(currRequest$VAR))
      {
          wrn(24);return(x)
      }
      theName <- currRequest$VAR[1]     # skip all except the first variable
      theDim  <- currRequest$DIMS[1]
      if(isALL(theDim)){
        x$put_variable(x = value, name = theName) 
      } else {
        x$put_variable(x = value, name = theName, dims = theDim)
      }
      
      return(x)
      
    } else  if (all(class(value) == "doc")) {
      
      # Create readme.json
      x$put_doc(x = value$to_json(), name = i)
    
    # Delete variables
    } else {
      wrn(22, class(value)); return(x)
    }
    
    return(x)
  }
)
