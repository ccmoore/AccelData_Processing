library(devtools)
library(roxygen2)

### To Update R Package ###

# 1) Update function documentation
document("./Accelerometer")

# 2) Rebuild package
build("./Accelerometer")

### Test Functions ###
load_all()
?acceldata_prep
?sleep_filter
