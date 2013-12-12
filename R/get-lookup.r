#' Read dictionary from disk
#'
#' Read dictionary that represents variable data from disk.
#'
#' @param name Variable name
#' @param path Directory of where the file is located
#' 
#' @export
#'
get_lookup <- function(name, path = getwd()) {
  if (file.exists(file.path(path, name))) {
    folder_path <- file_path(name, path, create_dir = F, file_name = F, data_folder = F)
    file <- file.path(folder_path, .lookup_filename)
    df <- read.table(file = file, header = T, quote = "", sep = "\t", stringsAsFactors = F)
    if (!is.data.frame(df) || ncol(df) != 2) {
      stop("lookup must be a two-column data frame")
    }
  } else { 
    df <- NULL
  }
  
  return(df)
}
