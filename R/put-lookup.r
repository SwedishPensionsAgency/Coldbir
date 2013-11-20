#' Write lookup table to disk
#'
#' Write lookup table that represents variable data to disk.
#'
#' @param df Two-column data frame with keys and values
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param create_dir If folder should be created when missing
#' 
#' @export
#'
put_lookup <- function(df, name, path = getwd(), create_dir = T) {
    
    if (!is.data.frame(df) || ncol(df) != 2) 
        stop("input must be a two-column data frame")
    
    if (is.data.table(df)) {
      setnames(df, c("key", "value"))
    } else {
      colnames(df) <- c("key", "value")
    }
    
    # Escape characters
    df$value <- escape_char(df$value)
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = F, data_folder = F)
    f <- file.path(folder_path, .lookup_filename)
    
    write_lookup <- function() {
        # Write temporary doc file to disk
        write.table(df, file = tmp, quote = F, row.names = F, sep = "\t")
        
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
            flog.fatal("%s - writing failed; rollback! (%s)", name, e)
        }
    )
    
    flog.info(f)
    return(TRUE)
} 
