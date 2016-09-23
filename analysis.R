rm(list=ls())
## Need `df_to_2dmat` function from eqtl library
source('/home/scidb/eqtl/eqtlstream/functions.R')

## Need dicom related functions
source('/home/scidb/p4scratch/dicom-scidb/functions.R')

library(scidb)
scidbconnect()

imgDB = scidb("IMGDB")
imgDB_Key = scidb("IMGDB_KEY")

getNumSlicesPerPatient(imgDB)

image( t(df_to_2dmat(getImgSlice(imgDB, patientNum = 1, sliceNum = 110)[])), col = gray.colors(256))

yy = aggregate(imgDB, FUN="max(val), min(val)")
yy[]

x = getImgSlice(imgDB, patientNum = 1, sliceNum = 70)
y = getRoi(x, 50, 350, 50, 350)
image( t(df_to_2dmat(y[])), col = gray.colors(256))

xx = project(transform(imgDB, v2 = "iif(val>1000, 1, 0)"), "v2")
xx = scidbeval(xx, temp=TRUE)

x = getImgSlice(xx, patientNum = 2, sliceNum = 70)
y = getRoi(x, 50, 350, 50, 350)
image( t(df_to_2dmat(y[])), col = gray.colors(256))

# histogram for 1 image
sl1 = getImgSlice(imgDB, patientNum = 2, sliceNum = 70)
# sl1 = subset(sl1, "val>=0")
data = as.vector(df_to_2dmat(sl1[]))
hist1 = hist(data)

hist2 = histscidb(sl1, breaks = hist1$breaks)
hist2$counts
hist2$breaks

barplot(hist2$counts)

# histogram for all images

