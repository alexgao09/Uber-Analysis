library(plyr)
library(ggplot2)

#############################################################################

apr_dat <- read.csv("./data/uber-raw-data-apr14.csv", stringsAsFactors = FALSE)
may_dat <- read.csv("./data/uber-raw-data-may14.csv", stringsAsFactors = FALSE)
jun_dat <- read.csv("./data/uber-raw-data-jun14.csv", stringsAsFactors = FALSE)
jul_dat <- read.csv("./data/uber-raw-data-jul14.csv", stringsAsFactors = FALSE)
aug_dat <- read.csv("./data/uber-raw-data-aug14.csv", stringsAsFactors = FALSE)
sep_dat <- read.csv("./data/uber-raw-data-sep14.csv", stringsAsFactors = FALSE)

apr_to_sep_dat <- rbind(apr_dat, may_dat, jun_dat, jul_dat, aug_dat, sep_dat)
apr_to_sep_dat$Date.Time <- as.POSIXct(apr_to_sep_dat$Date.Time, format = "%m/%d/%Y %H:%M:%S") # convert character to time

# feature extraction
print("Data preprocessing ...")

# Function to get rides each day of the week
apr_to_sep_dat$Date <- as.Date(apr_to_sep_dat$Date.Time)
apr_to_sep_dat$hr <- format(apr_to_sep_dat$Date.Time, "%H") # gives hour of ride in string
apr_to_sep_dat$hr_map_r <- factor(floor(as.numeric(apr_to_sep_dat$hr)/24*8))
apr_to_sep_dat$hr_map <- mapvalues(apr_to_sep_dat$hr_map_r,
                                     from = 0:7,
                                     to = c("0AM-3AM",
                                            "3AM-6AM",
                                            "6AM-9AM",
                                            "9AM-12AM",
                                            "12AM-15AM",
                                            "15AM-18AM",
                                            "18AM-21AM",
                                            "21AM-24AM"))
  
  
apr_to_sep_dat$wd <- weekdays(apr_to_sep_dat$Date) # gives weekday

tt <- table(apr_to_sep_dat$Date)
tt_names <- names(tt) # already ordered
first_indices <- rep(0,length(tt_names)) # first indices contain the changepoints
ll=length(tt_names)



i=1
for (t in tt_names){
  first_indices[i] <- as.numeric(rownames(apr_to_sep_dat[apr_to_sep_dat$Date==t,][1,]))
  print(paste(i/ll*100, "percent of data preprocessing done ..."))
  i=i+1
}
