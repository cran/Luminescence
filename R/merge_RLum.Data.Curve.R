#' Merge function for RLum.Data.Curve S4 class objects
#'
#' Function allows merging of RLum.Data.Curve objects in different ways
#'
#' This function simply allowing to merge [RLum.Data.Curve-class]
#' objects without touching the objects itself. Merging is always applied on
#' the 2nd column of the data matrix of the object.
#'
#' **Supported merge operations are [RLum.Data.Curve-class]**
#'
#' `"sum"`
#'
#' All count values will be summed up using the function [rowSums].
#'
#' `"mean"`
#'
#' The mean over the count values is calculated using the function
#' [rowMeans].
#'
#' `"median"`
#'
#' The median over the count values is calculated using the function
#' [matrixStats::rowMedians].
#'
#' `"sd"`
#'
#' The standard deviation over the count values is calculated using the function
#' [matrixStats::rowSds].
#'
#' `"var"`
#'
#' The variance over the count values is calculated using the function
#' [matrixStats::rowVars].
#'
#' `"min"`
#'
#' The min values from the count values is chosen using the function
#' [matrixStats::rowMins][matrixStats::rowRanges].
#'
#' `"max"`
#'
#' The max values from the count values is chosen using the function
#' [matrixStats::rowMins][matrixStats::rowRanges].
#'
#' `"append"`
#'
#' Appends count values of all curves to one combined data curve. The channel width
#' is automatically re-calculated, but requires a constant channel width of the
#' original data.
#'
#' `"-"`
#'
#' The row sums of the last objects are subtracted from the first object.
#'
#' `"*"`
#'
#' The row sums of the last objects are multiplied with the first object.
#'
#' `"/"`
#'
#' Values of the first object are divided by row sums of the last objects.
#'
#' @param object [list] of [RLum.Data.Curve-class] (**required**):
#' list of S4 objects of class `RLum.Curve`.
#'
#' @param merge.method [character] (**required**):
#' method for combining of the objects, e.g.  `'mean'`, `'sum'`, see details for
#' further information and allowed methods.  Note: Elements in slot info will
#' be taken from the first curve in the list.
#'
#' @param method.info [numeric] (*optional*):
#' allows to specify how info elements of the input objects are combined,
#' e.g. `1` means that just the elements from the first object are kept,
#' `2` keeps only the info elements from the 2 object etc.
#' If nothing is provided all elements are combined.
#'
#' @return Returns an [RLum.Data.Curve-class] object.
#'
#' @note
#' The information from the slot `recordType` is taken from the first
#' [RLum.Data.Curve-class] object in the input list. The slot
#' 'curveType' is filled with the name `merged`.
#'
#' @section S3-generic support:
#'
#' This function is fully operational via S3-generics:
#' ``+``, ``-``, ``/``, ``*``, `merge`
#'
#' @section Function version: 0.2.1
#'
#' @author
#' Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @seealso [merge_RLum], [RLum.Data.Curve-class]
#'
#'
#' @keywords utilities internal
#'
#' @examples
#'
#'
#' ##load example data
#' data(ExampleData.XSYG, envir = environment())
#'
#' ##grep first and 3d TL curves
#' TL.curves  <- get_RLum(OSL.SARMeasurement$Sequence.Object, recordType = "TL (UVVIS)")
#' TL.curve.1 <- TL.curves[[1]]
#' TL.curve.3 <- TL.curves[[3]]
#'
#' ##plot single curves
#' plot_RLum(TL.curve.1)
#' plot_RLum(TL.curve.3)
#'
#' ##subtract the 1st curve from the 2nd and plot
#' TL.curve.merged <- merge_RLum.Data.Curve(list(TL.curve.3, TL.curve.1), merge.method = "/")
#' plot_RLum(TL.curve.merged)
#'
#' @md
#' @export
merge_RLum.Data.Curve<- function(
  object,
  merge.method = "mean",
  method.info
){

# Integrity checks ----------------------------------------------------------------------------

  ##(1) check if object is of class RLum.Data.Curve
  temp.recordType.test <- sapply(1:length(object), function(x){
    if(!inherits(object[[x]], "RLum.Data.Curve")){
      .throw_error("At least object ", x, " is not of class 'RLum.Data.Curve'")
    }

    ##provide class of objects
    return(object[[x]]@recordType)

  })

  ##(2) Check for similar record types
  record.types <- unique(temp.recordType.test)
  if (length(record.types) > 1) {
    .throw_error("Only similar record types are supported; you are trying to merge: ",
                 paste0("'", record.types, "'", collapse = ", "))
  }

# Merge objects ----------------------------------------------------------------
  ##merge data objects
  ##problem ... how to handle data with different resolution or length?

  ##(1) build new data matrix
    ##first find shortest object
    check.length <- vapply(object, function(x) nrow(x@data), numeric(1))

    ## do something about it
    temp.matrix  <- .warningCatcher(sapply(1:length(object), function(x){
      ##check if the objects are of equal length
      if (length(unique(check.length)) != 1) {
        ##but we have to at least check the resolution (roughly)
        if (round(diff(object[[x]]@data[,1]),1)[1] != round(diff(object[[1]]@data[,1]),1)[1])
          .throw_error("The objects do not seem to have the same channel resolution")

        ## either way, throw a warning
        .throw_warning("The number of channels between the curves differs. ",
                       "Resulting curve has the length of shortest curve.")

      ##if this is OK, we can continue and shorten the rest of the objects
      return(object[[x]]@data[1:min(check.length),2])

    }else{
      object[[x]]@data[,2]

    }

  }))

  ##(2) apply selected method for merging
  if(merge.method == "sum"){
    temp.matrix <- rowSums(temp.matrix)

  }else if(merge.method == "mean"){
    temp.matrix <- rowMeans(temp.matrix)

  }else if(merge.method == "median"){
    temp.matrix <- matrixStats::rowMedians(temp.matrix)

  }else if(merge.method == "sd"){
    temp.matrix <- matrixStats::rowSds(temp.matrix)

  }else if(merge.method == "var"){
    temp.matrix <- matrixStats::rowVars(temp.matrix)

  }else if(merge.method == "max"){
    temp.matrix <- matrixStats::rowMaxs(temp.matrix)

  }else if(merge.method == "min"){
    temp.matrix <- matrixStats::rowMins(temp.matrix)

  }else if(merge.method == "append") {
    temp.matrix <- sapply(temp.matrix, c)

  }else if(merge.method == "-"){
    if(ncol(temp.matrix) > 2){
      temp.matrix  <- temp.matrix[,1] - rowSums(temp.matrix[,-1])
    }else{
      temp.matrix <-  temp.matrix[,1] - temp.matrix[,2]
    }

  }else if(merge.method == "*"){
    if(ncol(temp.matrix) > 2){
      temp.matrix  <- temp.matrix[,1] * rowSums(temp.matrix[,-1])
    }else{
      temp.matrix <-  temp.matrix[,1] * temp.matrix[,2]
    }

  }else if(merge.method == "/"){
    if(ncol(temp.matrix) > 2){
      temp.matrix  <- temp.matrix[,1] / rowSums(temp.matrix[,-1])
    }else{
      temp.matrix <-  temp.matrix[,1] / temp.matrix[,2]
    }

    ##get index of inf values
    id.inf <- which(is.infinite(temp.matrix) == TRUE)

    ##replace with 0 and throw warning
    if (length(id.inf) > 0) {
      temp.matrix[id.inf]  <- 0
      .throw_warning(length(id.inf),
                     " 'inf' values have been replaced by 0 in the matrix.")
    }
  }else{
    .throw_error("Unsupported or unknown merge method")
  }

  ##add first column
  #If we append the data of the second to the first curve we have to recalculate
  #the x-values (probably time/channel). The difference should always be the
  #same, so we just expand the sequence if this is true. If this is not true,
  #we revert to the default behaviour (i.e., append the x values)
  if (merge.method[1] == "append" & length(unique(diff(object[[1]]@data[,1])))) {
      step <- unique(diff(object[[1]]@data[,1]))
      newx <- seq(from = min(object[[1]]@data[,1]), by = step, length.out = sum(check.length))
      temp.matrix <- cbind(newx, temp.matrix)
  } else {
    temp.matrix <- cbind(object[[1]]@data[1:min(check.length),1], temp.matrix)
  }



  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##merge info objects as simple as possible ... just keep them all ... other possibility
  ##would be to chose on the the input objects

  ##unlist is needed here, as otherwise i would cause unexpected behaviour further using
  ##the RLum.object
  if(missing(method.info)){
    temp.info <- unlist(lapply(1:length(object), function(x){
      object[[x]]@info

    }), recursive = FALSE)

  }else{
    temp.info <- object[[method.info]]@info

  }

  # Build new RLum.Data.Curve object --------------------------------------------------------------
  temp.new.Data.Curve <- set_RLum(
    class = "RLum.Data.Curve",
    originator = "merge_RLum.Data.Curve",
    recordType = object[[1]]@recordType,
    curveType =  "merged",
    data = temp.matrix,
    info = temp.info,
    .pid = unlist(lapply(object, function(x) {
      x@.uid
    }))

  )

# Return object -------------------------------------------------------------------------------
  return(temp.new.Data.Curve)

}
