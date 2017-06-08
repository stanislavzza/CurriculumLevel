# this variation takes graduates from a program and looks at their registration history within certain disciplines
# the model for this is NSC

# we need to say where the data file is--
setwd("C:/Users/deubanks/Box Sync/GitHub/CurriculumLevel") #change to your directory. Note FORWARD slashes

# load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

#read data from csv, select only the three fields we care about
data <- read.csv("Grades.csv", as.is = TRUE) %>% select(Subject, Number, YearTaken)

# rounds the number to 100s. May need to adjust, e.g. if you have four-digit course numbers change 100 to 1000
data$Number <- as.integer(substr(data$Number,1,1)) * 100 #assumes a 1xx, 2xx, etc. number system

# this eliminates oddities in the data. Use accordingly
data <- data %>% filter(YearTaken >= 0, Number < 500) # you can filter the ranges here

# if you want to inspect first, this creates a box plot of the total average
ggplot(data,aes(factor(Number),YearTaken)) + geom_boxplot()

# institutional average YearTaken by course number, for reference
total <- data %>% group_by(Number) %>% summarize(Years = mean(YearTaken))

# create data for each plot, summarizing avg year taken by subject and number
plots <- data %>% group_by(Subject,Number) %>% summarize(Years = mean(YearTaken))

# fill in missing rows for plots that have missing data
# http://stackoverflow.com/questions/9996452/r-find-and-add-missing-non-existing-rows-in-time-related-data-frame
vals <- expand.grid(Number = unique(plots$Number),
                    Subject = unique(plots$Subject))
plots <- merge(vals,plots,all = TRUE)

# setwd("CHANGE TO FOLDER YOU WANT THE GRAPHS TO GO TO")

########## the loop before runs through each subject and creates a .png graph

# get the list of unique subjects 
subjects <- as.vector(unlist(plots %>% select(Subject) %>% unique()))

for (subj in subjects){

  # file names, e.g. BIO.png
  fname <- paste0(subj,".png")
  
  # you can change the size of the graph here
  png(filename = fname, width = 480, height = 480, units = "px", pointsize = 12, bg = "white")
  
  # select the one we're looking at
  t <- plots %>% filter(Subject == subj)
  
  # generate the graph
  print(plot(total$Number,total$Years, col = "red",main= subj, xlim = c(100,400), ylim = c(0,4), xlab = "Course Level",ylab ="Avg Year Taken"))
  print(lines(total$Number,total$Years, lwd = .5, col = "red", lty = 2))
  print(points(t$Number,t$Years))
  print(lines(t$Number,t$Years, lwd = 2))
  dev.off()
}

