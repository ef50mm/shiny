setwd('/Users/yichen/github/R/Correctness/')

install.packages("jsonlite")

library("jsonlite")
install.packages("curl")
library("curl")

#knownBuilds <- fromJSON("https://avocadobn1.blob.core.windows.net/avocado-cache-1/reports/77554/templates/214830/executions/13246310?sv=2014-02-14&sr=b&sig=%2FpsImNvTJYXIkGkjCfv1zvhJ76XJMk1egb0erxYng7U%3D&se=2015-04-06T05%3A54%3A10Z&sp=r")
#reliability <- fromJSON("https://avocadobn1.blob.core.windows.net/avocado-cache-1/reports/77554/templates/214834/executions/13246311?sv=2014-02-14&sr=b&sig=6CcKx9Z0CVBytvoenqpzM3qGfcW7CR7lzPd2haNhk8k%3D&se=2015-04-06T05%3A54%3A10Z&sp=r")
#saveRDS(releases, "knownBuilds.rds")
#saveRDS(reliability, "reliability.rds")

knownBuilds <- readRDS("knownBuilds.rds")
reliability <- readRDS("reliability.rds")

# 0: coverged
# 1: actively syncing
# 5: idle
correctDevices <- reliability[reliability$Name %in% c(0, 1, 5), ]
incorrectDevices <- reliability[reliability$Name %in% c(2, 3), ]

