rm(list=ls())
## Need dicom related functions
source('~/p4scratch/dicom-scidb/functions.R')

library(oro.dicom)
library(scidb)
scidbconnect()
# # Start from scratch
if (FALSE)
{
  iquery("remove(IMGDB)")
  iquery("create array IMGDB <val:int32> [patient_id=0:*,1,0, i=0:511,512,0,j=0:511,512,0,k=0:*,10,0]")
  iquery("remove(IMGDB_KEY)")
  iquery("create array IMGDB_KEY <folderpath:string>[patient_id=0:*,1,0]")
}

imgDB = scidb("IMGDB")
imgDB_Key = scidb("IMGDB_KEY")

BASEFOLDER='/datastore/dicomdata'
SUBPATHS= c(
  'PRCA-20090113/CA0101/1.2.826.0.1.3680043.2.656.4.1.6.1/S505A01/', 
  'PRCA-20090113/CA0102/1.2.826.0.1.3680043.2.656.4.1.6.201/S502A01/', 
  'PRCA-20090113/CA0103/1.2.826.0.1.3680043.2.656.4.1.6.210/S504A01/',
  'PRSM-20090113/SM0052/1.2.826.0.1.3680043.2.656.4.1.1.894/S02A01/', 
  'PRSM-20090113/SM0053/1.2.826.0.1.3680043.2.656.4.1.1.897/S02A01/', 
  'PRSM-20090113/SM0054/1.2.826.0.1.3680043.2.656.4.1.1.900/S02A01/',
  'PRSM-20090113/SM0055/1.2.826.0.1.3680043.2.656.4.1.1.911/S02A01/',
  'PRSM-20090113/SM0057/1.2.826.0.1.3680043.2.656.4.1.1.929/S02A01/',
  'PRSM-20090113/SM0060/1.2.826.0.1.3680043.2.656.4.1.1.955/S02A01/',
  'PRSM-20090113/SM0061/1.2.826.0.1.3680043.2.656.4.1.1.959/S02A01/',
  'PRSM-20090113/SM0063/1.2.826.0.1.3680043.2.656.4.1.1.965/S02A01/',
  'PRSM-20090113/SM0065/1.2.826.0.1.3680043.2.656.4.1.1.978/S02A01/',
  'PRSM-20090113/SM0066/1.2.826.0.1.3680043.2.656.4.1.1.981/S02A01/',
  'PRSM-20090113/SM0067/1.2.826.0.1.3680043.2.656.4.1.1.984/S02A01/',
  'PRSM-20090113/SM0068/1.2.826.0.1.3680043.2.656.4.1.1.1003/S02A01/',
  'PRSM-20090113/SM0070/1.2.826.0.1.3680043.2.656.4.1.1.1010/S02A01/',
  'PRSM-20090113/SM0073/1.2.826.0.1.3680043.2.656.4.1.1.1019/S02A01/',
  'PRSM-20090113/SM0077/1.2.826.0.1.3680043.2.656.4.1.1.1038/S02A01/',
  'PRSL-20090113/SL0058/1.2.826.0.1.3680043.2.656.4.1.1.932/S02A01/',
  'PRSL-20090113/SL0059/1.2.826.0.1.3680043.2.656.4.1.1.935/S02A01/',
  'PRSL-20090113/SL0062/1.2.826.0.1.3680043.2.656.4.1.1.962/S02A01/',
  'PRSL-20090113/SL0064/1.2.826.0.1.3680043.2.656.4.1.1.971/S02A01/',
  'PRSL-20090113/SL0072/1.2.826.0.1.3680043.2.656.4.1.1.1016/S02A01/',
  'PRSL-20090113/SL0074/1.2.826.0.1.3680043.2.656.4.1.1.1025/S02A01/',
  'PRSL-20090113/SL0078/1.2.826.0.1.3680043.2.656.4.1.1.1041/S02A01/',
  'PRSL-20090113/SL0079/1.2.826.0.1.3680043.2.656.4.1.1.1044/S02A01/',
  'PRSL-20090113/SL0080/1.2.826.0.1.3680043.2.656.4.1.1.1054/S02A01/',
  'PRSL-20090113/SL0081/1.2.826.0.1.3680043.2.656.4.1.1.1057/S03A01/',
  'PRSL-20090113/SL0083/1.2.826.0.1.3680043.2.656.4.1.1.1069/S02A01/',
  'PRSL-20090113/SL0084/1.2.826.0.1.3680043.2.656.4.1.1.1076/S02A01/'
  )
IMGFOLDERS= paste(BASEFOLDER, SUBPATHS, sep="/")


# The actual loop to load data
for (path in IMGFOLDERS)
{
  cat("#################\n")
  cat(path)
  cat("\n")
  cat("Checking if this DICOM folder path is already existing in SciDB\n")
  if (count(subset(imgDB_Key, sprintf("folderpath='%s'", path))) > 0) # path exists already
  { 
    cat("already exists. going to next\n")
    next
  }
  else {
    cat("Does not exist. Loading \n")
  }
    
  
  cat("reading Dicom \n")
  t1 = proc.time()
  hk40 = readDICOM(path)
  vol = create3D(hk40)
  print(proc.time()-t1)
  cat("finished reading Dicom in R\n ---- \n")
  
  cat("loading DICOM image into SciDB temporary array\n")
  t1 = proc.time()
  y = ThreeDarray2scidb(vol, asImgStack = TRUE)
  print(proc.time()-t1)
  cat("finished loading one DICOM image into SciDB \n ----\n")
  
  patient_id_ = getNextPatientID()$count
  cat(sprintf("Inserting DICOM folder name into Key database at position %.0f\n", patient_id_))
  
  cat("inserting DICOM stack into SciDB IMGDB array\n")
  t1 = proc.time()
  y2 = transform(y, patient_id = patient_id_)
  y3 = redimension(y2, sprintf("<val:int32> %s", scidb:::build_dim_schema(imgDB)))
  y4 = scidb(sprintf("insert(%s, IMGDB)", y3@name))
  scidbeval(y4)
  print(proc.time()-t1)
  cat("finished insertion into master SciDB array \n ---- \n")

  insertKey(path)
}

