#' Create a folder of plots that show the relationship between course levels and timing:
#' when do students take courses? We might assume that lower level courses are most commonly
#' taken by first-year students, and upper-level courses by seniors, but is that the case?

# load libraries
library(tidyverse)

#' Read data from csv, which should be in the /data folder. 
#' Select only the fields we care about
courses <- read_csv("data/sample.csv") %>% 
            select(Subject, Number, YearTaken) %>% 
            # assumes three-digit course numbers here
            mutate(Number = as.integer(str_sub(Number, 1, 1))*100) %>% 
            # filter to a usable range
            filter(YearTaken >= 0, Number < 500) 

# institutional average YearTaken by course number, for reference
inst_avg <- courses %>% 
            group_by(Number) %>% 
            summarize(YearTaken = mean(YearTaken))

inst_props <- courses %>% 
  count(Number, YearTaken) %>% 
  group_by(Number) %>% 
  mutate(p = n/sum(n),
         YearTaken = str_c("Year ", YearTaken)) %>% 
  ungroup()

# create summaries for each subject plot
sub_avg <- courses %>% 
           group_by(Subject,Number) %>% 
           summarize(YearTaken = mean(YearTaken))

subj_props <- courses %>% 
  count(Subject, Number, YearTaken) %>% 
  group_by(Subject, Number) %>% 
  mutate(p = n/sum(n),
         YearTaken = str_c("Year ", YearTaken)) %>% 
  ungroup()

########## the loop before runs through each subject and creates a .png graph

# get the list of unique subjects 
subjects <- unique(sub_avg$Subject)

for (subject in subjects){
  
  # file names, e.g. BIO.png
  fname <- str_c("images/",subject,".png")
  
  # filter the data to the subject we will plot
  plot_data <- sub_avg %>% 
    filter(Subject == subject) 
  
  # you can change the size of the graph here
  #png(filename = fname, width = 480, height = 480, units = "px", pointsize = 12, bg = "white")
  
  # plot averages
  plot_data %>% 
    ggplot(aes(x = Number, y = YearTaken)) +
    geom_line(color = "steelblue") +
    geom_point(color = "steelblue") +
    geom_line(aes(x = Number, y = YearTaken), data = inst_avg, linetype = "dashed") +
    ggtitle(subject) +
    coord_cartesian(ylim = c(1,4)) +
    theme_bw()
  
  # save to disk
  ggsave(fname)
  
  ##################### Proportions ##########################
  
  # file names, e.g. BIO.png
  fname <- str_c("images/",subject,"_props.png")
  
  # filter the data to the subject we will plot
  plot_data <- subj_props %>% 
    filter(Subject == subject) 
  
  # you can change the size of the graph here
  #png(filename = fname, width = 480, height = 480, units = "px", pointsize = 12, bg = "white")
  
  # plot averages
  plot_data %>% 
    ggplot(aes(x = Number, y = p)) +
    geom_line(color = "steelblue") +
    geom_point(color = "steelblue") +
    geom_line(aes(x = Number, y = p), data = inst_props, linetype = "dashed") +
    ggtitle(subject) +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    facet_grid(. ~ YearTaken) +
    ylab("Proportion taking courses")
  
  # save to disk
  ggsave(fname)
  
}
