# dicom-scidb
SciDB for managing DICOM data

## Notes

Lung imaging datasets from the following portal: Public Lung Database to Assess Drug Response
http://www.via.cornell.edu/crpf.html

Access data here: https://veet.via.cornell.edu/crpf.html, Key: khv9qiu

Ingesting the image portion of DICOM into SciDB is easy. There is a CRAN package for reading DICOM-s (https://cran.r-project.org/web/packages/oro.dicom/vignettes/dicom.pdf), and we have SciDBR to upload 2d matrices one z-slice at a time into SciDB. Later we can optimize for uploading the entire z-stack as a full 3D matrix into SciDB. 

The tricky part is working with the header info. In DICOMs there is a specific structure of storing headers, and lots and lots of info in the headers. We can store that as strings in SciDB for now. We have to figure out a strategy for this in the long run. 
