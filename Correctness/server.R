library(shiny)
library(curl)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  
  output$distPlot <- renderPlot({
    
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
    
    totalCount <- merge(x = correctCount, y = incorrectCount, by = c("build", "date"), all = TRUE)
    totalCount <- totalCount[(totalCount$x.x + totalCount$x.y) > populationThreshold, ]
    
    # all combonations of builds and dates, cartesian product
    combos <- expand.grid(date=dates, build=appVersions)
    # left outer join
    datesWithBuilds <- merge(x = combos, y = totalCount, by = c("build", "date"), all.x=TRUE)
    rates <- datesWithBuilds$x.y /(datesWithBuilds$x.x + datesWithBuilds$x.y)
    # reshape to (date, build) matrix
    results <- matrix(rates, nrow=length(dates))
    
    # add column names
    colnames(results) <- appVersions
    # transform from matrix to data frame
    df <- data.frame(results)
    # add a new column
    df$Date <- dates
    
    # convert the data to a "tall" format.
    allBuilds <- melt(df, id.vars="Date")
    
    # draw all builds in one plot
    ggplot(allBuilds, aes(Date, value)) + 
      geom_line(aes(color = variable))
  })
})
