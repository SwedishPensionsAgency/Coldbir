#' Put variable documentation to disk
#'
#' Write documentation of a variable to disk.
#'
#' @param x Character string
#' @param name Variable name
#' @param path Directory of where the file are to be created
#' @param create_dir If folder should be created when missing
#' @param file_name Documentation file name
#' 
#' @export
#'
put_variable_doc <- function(x, name, path = getwd(), create_dir = TRUE, file_name = .doc_json) {
    
    folder_path <- file_path(name, path, create_dir = create_dir, file_name = FALSE, data_folder = FALSE)
    f <- file.path(folder_path, file_name)
    
    write_doc <- function() {
        # Write temporary doc file to disk
        sink(tmp)
        cat(x)
        sink()
        
        # Rename temporary doc to real name (overwrite)
        file.copy(tmp, f, overwrite = TRUE)
    }

    # Create temporary file
    tmp <- create_temp_file(f)

    # Try to write doc file to disk
    tryCatch(
        write_doc(),
        finally = file.remove(tmp),
        error = function(e) {
            flog.fatal("%s - writing failed; rollback! (%s)", name, e)
        }
    )

    flog.info(f)
    return(TRUE)
} 
