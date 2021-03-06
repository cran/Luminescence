#' Export PSL-file(s) to CSV-files
#'
#' This function is a wrapper function around the functions [read_PSL2R] and
#' [write_RLum2CSV] and it imports an PSL-file (SUERC portable OSL reader file format)
#' and directly exports its content to CSV-files.
#' If nothing is set for the argument `path` ([write_RLum2CSV]) the input folder will
#' become the output folder.
#'
#' @param file [character] (**required**):
#' name of the PSL-file to be converted to CSV-files
#'
#' @param ... further arguments that will be passed to the function
#' [read_PSL2R] and [write_RLum2CSV]
#'
#' @return
#' The function returns either a CSV-file (or many of them) or for the option
#' `export = FALSE` a list comprising objects of type [data.frame] and [matrix]
#'
#' @section Function version: 0.1.0
#'
#' @author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#' @seealso [RLum.Analysis-class], [RLum.Data-class], [RLum.Results-class],
#' [utils::write.table], [write_RLum2CSV], [read_PSL2R]
#'
#' @keywords IO
#'
#' @examples
#'
#' \dontrun{
#' ##select your BIN-file
#' file <- file.choose()
#'
#' ##convert
#' convert_PSL2CSV(file)
#'
#' }
#'
#' @md
#' @export
convert_PSL2CSV <- function(
  file,
  ...

){

  # General tests -------------------------------------------------------------------------------

  ##file is missing?
  if(missing(file)){
    stop("[convert_PSL2CSV()] 'file' is missing!", call. = FALSE)

  }


  ##set input arguments
  convert_PSL2R_settings.default <- list(
    drop_bg = FALSE,
    as_decay_curve = TRUE,
    smooth = FALSE,
    merge = FALSE,
    export = TRUE
  )

  ##modify list on demand
  convert_PSL2R_settings <- modifyList(x = convert_PSL2R_settings.default, val = list(...))

  # Import file ---------------------------------------------------------------------------------
  if(!inherits(file, "RLum")){
    object <- read_PSL2R(
      file = file,
      drop_bg = convert_PSL2R_settings$drop_bg,
      as_decay_curve = convert_PSL2R_settings$as_decay_curve,
      smooth = convert_PSL2R_settings$smooth,
      merge = convert_PSL2R_settings$merge

   )
  }else{
    object <- file

  }

  # Export to CSV -------------------------------------------------------------------------------

  ##get all arguments we want to pass and remove the doubled one
  arguments <- c(list(object = object, export = convert_PSL2R_settings$export), list(...))
  arguments[duplicated(names(arguments))] <- NULL

  ##this if-condition prevents NULL in the terminal
  if(convert_PSL2R_settings$export == TRUE){
    invisible(do.call("write_RLum2CSV", arguments))

  }else{
    do.call("write_RLum2CSV", arguments)

  }

}
