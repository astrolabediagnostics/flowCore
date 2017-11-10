setClass("cytoSet", contains = "flowSet")


## ==========================================================================
## subsetting methods
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
## to flowSet
setMethod("[",
		signature=signature(x="cytoSet"),
		definition=function(x, i, j, ..., drop=FALSE)
		{
			if(missing(i) && missing(j)) 
				return(x)
			orig <- x@frames
			fr  <- new.env(hash=TRUE, parent=emptyenv())
			if(missing(i)) {
				for(nm in ls(orig))
					fr[[nm]] <- orig[[nm]][, j, ..., drop=FALSE]
				pd <- phenoData(x)
			} else {
				if(is.numeric(i) || is.logical(i)) {
					copy <- sampleNames(x)[i]
				} else {
					copy <- i
					i <- match(i,sampleNames(x))
				}
				if(any(is.na(copy)))
					stop("Subset out of bounds", call.=FALSE)
				if(missing(j))
					for(nm in copy)
						fr[[nm]] <- orig[[nm]][, , ..., drop=FALSE]
				else
					for(nm in copy)
						fr[[nm]] <- orig[[nm]][, j, ..., drop=FALSE]
				pd <- phenoData(x)[i,]
			}
			fr <- as(fr,"cytoSet")
			phenoData(fr) <- pd
			if(!missing(j)){
				if(is.character(j))
					colnames(fr) <- colnames(x)[match(j, colnames(x))]
				else
					colnames(fr) <- colnames(x)[j] 
				if(any(is.na(colnames(fr))))
					stop("Subset out of bounds", call.=FALSE)
			}
			return(fr)
		})



## ==========================================================================
## accessor and replace methods for slot colnames
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("colnames",
		signature=signature(x="cytoSet"),
		definition=function(x, do.NULL="missing", prefix="missing")
			colnames(x[[1]])
      )

setReplaceMethod("colnames",
		signature=signature(x="cytoSet",
				value="ANY"),
		definition=function(x, value)
		{
			         for(i in sampleNames(x))
                            colnames(x@frames[[i]]) <- value
			x
		})


setMethod("phenoData",
          signature=signature(object="cytoSet"),
          definition=function(object) {
            stop("not be implemented!")
            })

setMethod("phenoData<-",
          signature=signature(object="cytoSet",
                              value="ANY"),
          definition=function(object, value)
          {
            #dummy setter to ensure the phenoData is always empty when it is invoked by flowSet method dispatching
            object
          })

setReplaceMethod("pData",
		signature=signature(object="cytoSet",
				value="data.frame"),
		definition=function(object,value)
		{
			stop("not be implemented!")
		})



## Note that the replacement method also replaces the GUID for each flowFrame
setReplaceMethod("sampleNames",
		signature=signature(object="cytoSet"),
		definition=function(object, value)
		{
			oldNames <- sampleNames(object)
			value <- as.character(value)
			if(length(oldNames)!=length(value) ||
					!is.character(value))
				stop(" replacement values must be character vector ",
						"of length equal to number of frames in the set'",
						call.=FALSE)
			if(any(duplicated(value)))
				stop("Replacement values are not unique.", call.=FALSE)
			env <- new.env(hash=TRUE,parent=emptyenv())
			for(f in seq_along(oldNames)){
				tmp <- get(oldNames[f], object@frames)
				identifier(tmp) <- value[f]
				assign(value[f], tmp, env)
			}
			object@frames <- env
			return(object)
		})



## ==========================================================================
## apply method for flowSet
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("fsApply",
		signature=signature(x="cytoSet",
				FUN="ANY"),
		definition=function(x,FUN,...,simplify=TRUE, use.exprs=FALSE)
		{
			if(missing(FUN))
				stop("fsApply function missing")
			FUN <- match.fun(FUN)
			if(!is.function(FUN))
				stop("This is not a function!")
			
			res <- structure(lapply(sampleNames(x),function(n) {
								y <- x[[n]]
								FUN(if(use.exprs) exprs(y) else y,...)
							}),names=sampleNames(x))
			if(simplify) {
				if(all(sapply(res,is,"cytoFrame"))) {
					res <- as(res,"cytoSet")
					
				} else if(all(sapply(res,is.numeric)) || all(sapply(res,is.character)) &&
						diff(range(sapply(res,length))) == 0) {
					res <- do.call(rbind,res)
				}
			}
			res
		})


