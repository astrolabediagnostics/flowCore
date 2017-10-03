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

