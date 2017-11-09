setClass("cytoFrame", contains = "flowFrame" ,               
    representation=representation(pointer = "externalptr"))

setMethod("nrow",
    signature=signature(x="cytoFrame"),
    definition=function(x)
      getnrow(x@pointer)
)

setMethod("ncol",
    signature=signature(x="cytoFrame"),
    definition=function(x)
      getncol(x@pointer)
)


setMethod("exprs",
    signature=signature(object="cytoFrame"),
    definition=function(object){
      getData(object@pointer)
    })

setReplaceMethod("colnames",
    signature=signature(x="cytoFrame",
        value="ANY"),
    definition=function(x, value)
    {
      old.names <- colnames(x)
      if(length(value) != length(old.names))
        stop("colnames don't match dimensions of data matrix",
            call.=FALSE)
      
      for(i in seq_along(value))
        setChannel(x@pointer, old.names[i], value[i])
      return(x)
    })

setReplaceMethod("markernames",
    signature=signature(object="cytoFrame", value="ANY"), function(object, value){
      old.names <- parameters(object)[["desc"]]
      if(!is.character(value)){
        stop("value must be a named character vector!")
      }else{
        chnls <- names(value)
        if(is.null(chnls))
          stop("value must be a named character vector!")
        inds <- match(chnls, colnames(object))
        misMatch <- is.na(inds)
        if(any(misMatch))
          stop("channel names not found in flow data: ", paste0(chnls[misMatch], collapse = ","))
        
        for(i in seq_along(inds)){
          ind <- inds[i]
          setMarker(object@pointer, old.names[ind], value[i])
        }
          
        
        
        
        object
      }
      
      
    })

setMethod("parameters",
    signature=signature(object="cytoFrame"),
    definition=function(object, names=FALSE)
    {
      if(!names)
      {
        pdata <- getpdata(object@pointer)
        new("AnnotatedDataFrame",
            data=pdata,
            varMetadata=data.frame(row.names=I(c("name","desc","range",
                        "minRange", "maxRange")),
                labelDescription=I(c("Name of Parameter","Description of Parameter",
                        "Range of Parameter", "Minimum Parameter Value after Transforamtion",
                        "Maximum Parameter Value after Transformation"))))
        
        
      }else
        as.character(parameters(object)[["name"]])
    })

setMethod("keyword",
    signature=signature(object="cytoFrame",
        keyword="character"),
    function(object, keyword){
      val <- getKeyword(object@pointer,keyword)
      if(val=="")
        val <- NULL
      structure(list(val), names=keyword)
    })


## this is equivalent to the description method
setMethod("keyword",
    signature=signature(object="cytoFrame",
        keyword="missing"),
    function(object, compact = FALSE)
    {           
      
      desc <- getKeywords(object@pointer)

      if(compact)
        desc <- kwfilter(desc)
      desc <- as.list(desc) 
      FCSversion <- desc[["FCSversion"]]
      desc[["FCSversion"]] <- NULL
      desc <- c(FCSversion = FCSversion, desc)  
      desc
    })

# coerce cytoFrame to flowFrame
as.flowFrame <- function(fr){
  fr@exprs <- exprs(fr)
  fr@description = keyword(fr)
  fr@parameters <- parameters(fr)
  as(fr, "flowFrame")
}

#' save the cytoFrame as h5 format
#' @param fr cytoFrame object
#' @param filename the full path of the output h5 file
#' @export
write.h5 <- function(fr, filename){
  writeH5(fr@pointer,filename)
}
