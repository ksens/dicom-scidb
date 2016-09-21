# Convert a data-frame with three columns into a matrix
# ordered: is the dataframe already ordered 
df_to_2dmat = function(df, ordered = TRUE)
{
  if (length(names(df)) == 3) # When rownames, and column-names are explicitly provided
  { 
    # Column 1 has the matrix column name info
    # Column 2 has the matrix row name info
    # Column 3 has the matrix data
    if (!ordered)
    {
      # Make sure the data frame is ordered exactly 
      df = df[with(df, order(df[,1], df[,2])), ]
    }
    
    ncols = length(unique(df[, 1]))
    nrows = length(unique(df[, 2]))
    mat = matrix(df[, 3], nrow=nrows, ncol=ncols)
    #dim(mat)  
    
    #### Formulate the rownames and verify
    rowlist = df[, 2]
    rnames = unique(rowlist)
    
    if (ordered) {
      # Verify
      check = all.equal(rep(rnames, ncols), rowlist)
      # print(check)
      stopifnot(check)
    }
    
    #### Formulate the rownames and verify
    collist = df[, 1]
    cnames = unique(collist)
    xx = unlist(lapply(cnames, function(x) {rep(x, nrows)}))
    
    if (ordered) {
      # Verify
      check = all.equal(xx, collist)
      # print(check)
      stopifnot(check)
    }
    
    # now paste the rownames and colnames to the matrix
    rownames(mat) = rnames
    colnames(mat) = cnames
    
    mat
  } else { # Assume that the column names are not provided
    # Column 1 has the matrix row name info
    # Column 2 has the matrix data
    # NOTE: Here we assume that data is always ordered
    stopifnot(ordered == TRUE)
    rowlist = df[, 1]
    rnames = unique(rowlist)
    nrows = length(rnames)
    ncols = nrow(df) / nrows
    
    mat = matrix(df[,2], nrow = nrows)
    rownames(mat) = rnames
    colnames(mat) = c(0: (ncols-1))
    
    mat
  }
}


library(oro.dicom)
abdo <- readDICOMFile("~/dicomtests/data/PRCA-20090113/CA0101/1.2.826.0.1.3680043.2.656.4.1.6.1/S505A01/1.2.826.0.1.3680043.2.656.4.1.6.8.1456.dcm")
names(abdo)
head(abdo$hdr)
tail(abdo$hdr)
class(abdo)
class(abdo$hdr)
class(abdo$img)
x = abdo$img
dim(x)

hk40 = readDICOM("~/dicomtests/data/PRCA-20090113/CA0101/1.2.826.0.1.3680043.2.656.4.1.6.1/S505A01/")
unlist(lapply(hk40, length))
class(hk40)
x = hk40$img
class(x)
length(x)
z = x[[1]]
class(z)
dim(z)

library(scidb)
scidbconnect()
Z = as.scidb(z)
str(Z)

zz = df_to_2dmat(subset(Z, i > 50 && i <= 350 && j > 50 && j <= 350)[])
image(zz, col = gray.colors(256))
