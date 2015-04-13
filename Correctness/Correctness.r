setwd('/Users/yichen/github/R/Correctness/')

install.packages("jsonlite")

library("jsonlite")
install.packages("curl")
library("curl")

#knownBuilds <- fromJSON("https://avocadobn1.blob.core.windows.net/avocado-cache-1/reports/77554/templates/214830/executions/13246310?sv=2014-02-14&sr=b&sig=%2FpsImNvTJYXIkGkjCfv1zvhJ76XJMk1egb0erxYng7U%3D&se=2015-04-06T05%3A54%3A10Z&sp=r")
#reliability <- fromJSON("https://avocadobn1.blob.core.windows.net/avocado-cache-1/reports/77554/templates/214834/executions/13246311?sv=2014-02-14&sr=b&sig=6CcKx9Z0CVBytvoenqpzM3qGfcW7CR7lzPd2haNhk8k%3D&se=2015-04-06T05%3A54%3A10Z&sp=r")
#saveRDS(releases, "knownBuilds.rds")
#saveRDS(reliability, "reliability.rds")

cutoffDate = as.Date("2014-10-25", "%Y-%m-%d")
populationThreshold = 100

knownBuilds <- readRDS("knownBuilds.rds")
# convert from string to date
knownBuilds$ReleaseDate <- as.Date(knownBuilds$ReleaseDate, "%Y-%m-%d")
# only consider the builds we are monitoring
knownBuilds <- knownBuilds[knownBuilds$Monitor == 1, ]
# remove old builds
releases <- knownBuilds[knownBuilds$ReleaseDate > cutoffDate, ]

reliability <- readRDS("reliability.rds")
reliability$Date <- as.Date(reliability$Date, "%Y-%m-%d")

# 0: coverged
# 1: actively syncing
# 5: idle
correctDevices <- reliability[reliability$Name %in% c(0, 1, 5), ]
# group by builds and dates
correctCount <- aggregate(correctDevices$DeviceCount, list(build=correctDevices$AppVersion, date=correctDevices$Date), sum)

# 2: diverged
# 3: fatal assert
incorrectDevices <- reliability[reliability$Name %in% c(2, 3), ]
incorrectCount <- aggregate(incorrectDevices$DeviceCount, list(build=incorrectDevices$AppVersion, date=incorrectDevices$Date), sum)

appVersions <- sort(unique(knownBuilds$AppVersion))
dates <- sort(unique(reliability$Date))

# matrix (build, date) -> correctness rate

"
incorrectnessRate <- function(build, date) {
  correct <- correctCount[(correctCount$build == build) & (correctCount$date == date), ]
  incorrect <- incorrectCount[(incorrectCount$build == build) & (incorrectCount$date == date), ]
  rate <- (incorrect$x / (correct$x + incorrect$x))
  return(rate)
}

very slow
results <- matrix(, nrow = length(appVersions), ncol = length(dates))
for (r in 1:length(appVersions)) {
  for (c in 1:length(dates)) {
    results[r, c] <- incorrectnessRate(appVersions[r], dates[c])
  }
}
saveRDS(results, 'results.rds')
results <- readRDS('results.rds')
"

incorrectnessRate <- function(dateWithBuild) {
  # left outer join
  correct <- merge(x = dateWithBuild, y = correctCount, by = c("date", "build"), all.x=TRUE)
  incorrect <- merge(x = dateWithBuild, y = incorrectCount, by = c("date", "build"), all.x=TRUE)
  rate <- (incorrect$x / (correct$x + incorrect$x))
  return(rate)
}

# all combonations of builds and dates, cartesian product
combos <- expand.grid(build=appVersions, date=dates)
# same output dimension as the input
rates <- incorrectnessRate(combos)
# reshape to (build, date) matrix
results <- matrix(rates, nrow=length(appVersions))

# all.equal(slow, fast)

results <- t(results)
rownames(results) <- appVersions
colnames(results) <- format(dates, format="%Y-%m-%d")
rownames(results) <- format(dates, format="%Y-%m-%d")
colnames(results) <- appVersions
df <- data.frame(results)

df$Date <- dates
ggplot(data = df, aes(Date, X17.3.1229.0918)) + geom_line() 

library("reshape2")

ggplot(aes(x = row.names, y = X17.3.1229.0918), data = df) + 
  geom_point() 
