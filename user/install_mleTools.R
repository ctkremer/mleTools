
# Need to install devtools, if not already there
library(devtools)

# remove any old versions of the package before installing
remove.packages('mleTools')

# Install the growthTools package!
devtools::install_github("ctkremer/mleTools")

# Load package!
library(mleTools)
