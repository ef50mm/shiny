setwd('/Users/yichen/github/R/Correctness/')

"
install.packages('jsonlite')
library('jsonlite')
install.packages('curl')
library('curl')

knownBuilds <- fromJSON('https://avocadobn1.blob.core.windows.net/avocado-cache-1/reports/77554/templates/214830/executions/13246310?sv=2014-02-14&sr=b&sig=%2FpsImNvTJYXIkGkjCfv1zvhJ76XJMk1egb0erxYng7U%3D&se=2015-04-06T05%3A54%3A10Z&sp=r')
reliability <- fromJSON('https://avocadobn1.blob.core.windows.net/avocado-cache-1/reports/77554/templates/214834/executions/13246311?sv=2014-02-14&sr=b&sig=6CcKx9Z0CVBytvoenqpzM3qGfcW7CR7lzPd2haNhk8k%3D&se=2015-04-06T05%3A54%3A10Z&sp=r')
saveRDS(releases, 'knownBuilds.rds')
saveRDS(reliability, 'reliability.rds')
"

cutoffDate <- as.Date("2014-10-25", "%Y-%m-%d")
populationThreshold <- 100

knownBuilds <- readRDS("knownBuilds.rds")
# convert from string to date
knownBuilds$ReleaseDate <- as.Date(knownBuilds$ReleaseDate, "%Y-%m-%d")
# only consider the builds we are monitoring
knownBuilds <- knownBuilds[knownBuilds$Monitor == 1, ]
# remove old builds
releases <- knownBuilds[knownBuilds$ReleaseDate > cutoffDate, ]

reliability <- readRDS("reliability.rds")
reliability$Date <- as.Date(reliability$Date, "%Y-%m-%d")

# get the sorted list of all unique builds and dates
appVersions <- sort(unique(knownBuilds$AppVersion))
dates <- sort(unique(reliability$Date))

# matrix (date, build) -> incorrectness rate

"
results <- matrix(, nrow = length(dates), ncol = length(appVersions))

incorrectnessRate <- function(date, build) {
  correctDevices <- subset(reliability, Name %in% c(0, 1, 5) & (AppVersion == build) & (Date == date))
  incorrectDevices <- subset(reliability, Name %in% c(2, 3) & (AppVersion == build) & (Date == date))
  correctCount <- sum(correctDevices$DeviceCount)
  incorrectCount <- sum(incorrectDevices$DeviceCount)
  rate <- incorrectCount / (correctCount + incorrectCount)
  return(rate)
}

# very slow
for (r in 1:length(dates)) {
  for (c in 1:length(appVersions)) {
    cat('iteration = ', r, ', ', c, '\n')
    results[r, c] <- incorrectnessRate(dates[r], appVersions[c])
  }
}

saveRDS(results, 'results.rds')
results <- readRDS('results.rds')
"

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

# outer join of correct and incorrect
totalCount <- merge(x = correctCount, y = incorrectCount, by = c("build", "date"), all = TRUE)
# remove rows that don't meet the daily 100 active devices threshold
totalCount <- totalCount[(totalCount$x.x + totalCount$x.y) > populationThreshold, ]

# all combonations of builds and dates, cartesian product
combos <- expand.grid(date=dates, build=appVersions)
# left outer join
datesWithBuilds <- merge(x = combos, y = totalCount, by = c("build", "date"), all.x=TRUE)
# calculate the incorrect rate
rates <- datesWithBuilds$x.y / (datesWithBuilds$x.x + datesWithBuilds$x.y)
# reshape to (date, build) matrix
results <- matrix(rates, nrow=length(dates))

# all.equal(slow, fast)

# add column names
colnames(results) <- appVersions
# transform from matrix to data frame
df <- data.frame(results)
# add a new column
df$Date <- dates

# convert the data to a "tall" format.
allBuilds <- melt(df, id.vars="Date")

# import the library for plotting
library(ggplot2)

# draw one build
ggplot(data = df, aes(Date, X17.3.1229.0918)) + geom_line() 

# draw all builds in seperate plots
ggplot(allBuilds, aes(Date, value)) + 
  geom_line() + 
  facet_wrap(~variable)

# draw all builds in one plot
ggplot(allBuilds, aes(Date, value)) + 
  geom_line(aes(color = variable))

# draw serveral plots in one
p1 <- ggplot(allBuilds, aes(Date, value)) + 
  geom_line(aes(color = variable))

p2 <- ggplot(allBuilds, aes(Date, value)) +
  geom_smooth(method = 'lm', aes(color = variable))

p3 <- ggplot(aes(x = value), data = allBuilds) + 
  geom_histogram(aes(fill = variable)) + 
  scale_fill_brewer(type = 'qual')

library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 1)
