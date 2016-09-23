getNumSlicesPerPatient = function(X)
{
  aggregate(slice(slice(X, 'i', 0), 'j', 0), FUN="count(*)", by=list("patient_id"))[]
}

insertKey = function(path)
{
  iquery(
    sprintf("insert(redimension(build(<folderpath:string>[patient_id=%.0f:%.0f, 1, 0], '%s'), <folderpath:string>[patient_id=0:*, 1, 0]), IMGDB_KEY)", patient_id_, patient_id_, path)
  )
}

getNextPatientID = function()
{iquery("op_count(IMGDB_KEY)", return=TRUE)}

getImgSlice = function(X, patientNum, sliceNum)
{scidb(sprintf("slice(%s, k, %.0f, patient_id, %.0f)", X@name, sliceNum, patientNum))}

getRoi = function(X, roi_i0, roi_i1, roi_j0, roi_j1)
{scidb(sprintf("between(%s, %.0f, %.0f, %.0f, %.0f)", X@name, roi_i0, roi_j0, roi_i1, roi_j1))}

#' SciDB reshape operator
#' @param x a \code{scidb} object
#' @param schema optional new schema
#' @param shape optional vector of new array coordinate dimensions
#' @param dimnames optional new vector of array coordniate dimension name
#' @param start optional vector of new array starting coordinate index values
#' @param chunks optional vector of new array chunk sizes
#' @return a \code{scidb} object
#' @export
reshape_scidb_ = function(x, schema, shape, dimnames, start, chunks)
{
  if(!missing(schema))
  {
    if(is.scidb(schema)) schema = schema(schema) # <- that's nutty notation Malkovich!
    query = sprintf("reshape(%s,%s)",x@name,schema)
    return(.scidbeval(query, depend=list(x)))
  }
  if(missing(shape)) stop("Missing dimension shape")
  N = length(shape)
  if(missing(dimnames))
  {
    dimnames=letters[9:(9 + N - 1)]
  }
  if(missing(chunks))
  {
    chunks = ceiling(1e6 ^ (1 / N))
  }
  if(missing(start)) start = rep(0, N)
  shape = shape - 1 + start
  shape[is.infinite(shape)] = "*"
  overlaps = rep(0,N)
  D = scidb:::build_dim_schema(x, newstart=start, newnames=dimnames, newend=shape, newchunk=chunks, I=seq(1, N), newoverlap = overlaps)
  query = sprintf("reshape(%s,%s%s)", x@name,scidb:::build_attr_schema(x), D)
  scidb:::.scidbeval(query, depend=list(x))
}

ThreeDarray2scidb = function(X,
                             name=scidb:::tmpnam(),
                             start,
                             gc=TRUE, 
                             asImgStack,
                             ...)
{
  voldim = dim(X)

    # Check for a bunch of optional hidden arguments
  args = list(...)
  attr_name = "val"
  nullable = TRUE
  if(!is.null(args$nullable)) nullable = as.logical(args$nullable) # control nullability
  if(!is.null(args$attr)) attr_name = as.character(args$attr)      # attribute name
  do_reshape = TRUE
  nd_reshape = NULL
  type = force_type = scidb:::.Rtypes[[typeof(X)]]
  if(is.null(type)) {
    stop(paste("Unupported data type. The package presently supports: ",
               paste(unique(names(.Rtypes)), collapse=" "), ".", sep=""))
  }
  if(!is.null(args$reshape)) do_reshape = as.logical(args$reshape) # control reshape
  if(!is.null(args$type)) force_type = as.character(args$type) # limited type conversion
  chunkSize = c(min(1000L, nrow(X)), min(1000L, ncol(X)))
  chunkSize = as.numeric(chunkSize)
  if(length(chunkSize) == 1) chunkSize = c(chunkSize, chunkSize)
  overlap = c(0,0)
  if(missing(start)) start = c(0,0)
  start     = as.numeric(start)
  if(length(start) ==1) start = c(start, start)
  D = dim(X)
  start = as.integer(start)
  overlap = as.integer(overlap)
  dimname = scidb:::make.unique_(attr_name, "i")
  
  nd_reshape = dim(X)
  do_reshape = FALSE
  X = as.matrix(as.vector(aperm(X)))
  schema = sprintf(
    "< %s : %s null>  [%s=%.0f:%.0f,%.0f,%.0f]", attr_name, force_type, dimname, start[[1]],
    nrow(X) - 1 + start[[1]], min(nrow(X), chunkSize), overlap[[1]])
  load_schema = sprintf("<%s:%s null>[__row=1:%.0f,1000000,0]", attr_name, force_type, length(X))
  
  if(!is.matrix(X)) stop ("X must be a matrix or a vector")
  
  DEBUG = FALSE
  if(!is.null(options("scidb.debug")[[1]]) && TRUE == options("scidb.debug")[[1]]) DEBUG=TRUE
  td1 = proc.time()
  # Obtain a session from shim for the upload process
  session = scidb:::getSession()
  on.exit( scidb:::SGET("/release_session", list(id=session), err=FALSE) ,add=TRUE)
  
  # Upload the data
  bytes = .Call("scidb_raw", as.vector(t(X)), PACKAGE="scidb")
  ans = scidb:::POST(bytes, list(id=session))
  ans = gsub("\n", "", gsub("\r", "", ans))
  if(DEBUG)
  {
    cat("Data upload time", (proc.time() - td1)[3], "\n")
  }
  
  # Load query
  if(!is.null(nd_reshape) & missing(asImgStack))
  {
    return(scidbeval(reshape_scidb_(scidb(sprintf("input(%s,'%s', 0, '(%s null)')",load_schema, ans, type)), shape=nd_reshape),name=name, temp=TRUE))
  }
  else if (!missing(asImgStack))
  {
    return(
      scidbeval(
        scidb(
          sprintf("reshape(input(%s,'%s', 0, '(%s null)'),
                  <val:%s NULL>[i=0:%.0f,%.0f,0,j=0:%.0f,%.0f,0,k=0:%.0f,10,0])",
                  load_schema, ans, type,
                  type, 
                  voldim[1]-1, voldim[1],
                  voldim[2]-1, voldim[2],
                  voldim[3]-1)
          ),
        temp=TRUE)
      )
  }
  ans = scidb(name, gc=gc)
  if(!nullable) ans = replaceNA(ans)
  ans
}

=======
histscidb = function(X, bin_by, breaks, min, max)
{
  if (missing(min) | missing(max)) {
    stats = aggregate(X, FUN="min(val), max(val)")[]
  }
  if (missing(min)) min = stats$val_min
  if (missing(max)) max = stats$val_max
  
  if (missing(breaks)) {
    numSteps = 22
    breaks = seq(min, max, length.out = numSteps)
  }
  
  ### Formulate the binning string
  left = sapply(breaks, function(i){sprintf("val > %.0f", i)})
  left[1] = sprintf("val >= %.0f", breaks[1]) # Manually correcting the left most condition
  right = sapply(c(breaks[2:length(breaks)], Inf), function(i){sprintf("val <= %.0f", i)})
  conditions = paste(left, right, sep = " AND ")
  # Assign bin numbers 
  bins = 1:length(breaks)
  # Finally formulate the string
  str = NULL
  for (i in 1:(length(breaks)-2)) {
   str = paste(str, sprintf("iif(%s, %.0f, ", conditions[i], bins[i]))
  }
  str = paste(str, sprintf("%.0f", length(breaks)-1), paste(rep(")", length(breaks)-2), collapse=""))
  
  # Apply the bins
  applybins = transform(X, bin=str)
  if (missing(bin_by))
  {
    hist = aggregate(applybins, FUN="count(*)", by="bin")
    hist2 = hist[, drop=TRUE]
    hist2 <- hist2[order(hist2$bin),]
    
    hist3 = rep(0, length(breaks))
    for (i in 1:nrow(hist2)) {hist3[hist2$bin[i]] = hist2$count[i]}
    
    if (hist3[length(hist3)] == 0) { # if the last count is zero
      data.frame(breaks=breaks[1:(length(hist3)-1)], counts=hist3[1:(length(hist3)-1)])
    } else {stop("error")}
  } else if (length(bin_by) == 1) {
    hist = aggregate(applybins, FUN="count(*)", by=list(bin_by, "bin"))
    hist
  } else if (length(bin_by) == 2){
    bin_by[[3]] = "bin"
    hist = aggregate(applybins, FUN="count(*)", by=bin_by)
    hist
  }
}
