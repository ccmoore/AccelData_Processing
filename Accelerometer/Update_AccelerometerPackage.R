library(devtools)
library(roxygen2)

### To Update R Package ###

# 1) Update function documentation
document()

# 2) Rebuild package
build()

### Test Functions ###
load_all()
?acceldata_prep
?sleep_filter
