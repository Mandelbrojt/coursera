# Load packages
library(tidyverse)

# Read CSV file and store it in df
df <- read_csv("/Users/luism/Documents/R/ggplot-data-vis/data/week 1/cel-volden-wiseman.csv")

# Dataset inspection
names(df)
dim(df)
table(df$year)
summary(df$all_bills)

# Subset and retain by condition, then select variables by name
fig115 <- df %>% 
     filter(congress == 115) %>% 
     select("seniority", "all_pass")

head(fig115)

# Set up the data for x and y axis
ggplot(fig115,
       aes(x = seniority,
           y = all_pass))

# Add marks to the chart
ggplot(fig115,
       aes(x = seniority,
           y = all_pass)) + 
     geom_point()

# Add random noise to the data to avoid over-plotting
ggplot(fig115,
       aes(x = seniority,
           y = all_pass)) +
     geom_jitter()

# Add labels and a title
ggplot(fig115,
       aes(x = seniority,
           y = all_pass)) +
     geom_jitter() + 
     labs(x = "Seniority",
          y = "Bills Passed",
          title = "Seniority and Bills Passed in the 115th Congress")

# Add "dem" variable to the subset and selected data
fig115 <- df %>% 
     filter(congress == 115) %>% 
     select("seniority","all_pass","dem")

head(fig115$dem)

# Set the color by using a variable
ggplot(fig115,
       aes(x = seniority,
           y = all_pass,
           color = dem)) + 
     geom_jitter() + 
     labs(x = "Seniority",
          y = "Bills Passed",
          title = "Seniority and Bills Passed in the 115th Congress")

# Set a variable as a categorical variable
party <- recode(fig115$dem,
                `1`="Democrat",
                `0`="Republican")

# Add the new variable into the dataframe
fig115 <- add_column(fig115,party)

# Show the first rows of the new variable
head(fig115$party)

# Show the plot with the new variable
ggplot(fig115,
       aes(x = seniority,
           y = all_pass,
           color = party)) +
     geom_jitter() + 
     labs(x = "Seniority",
          y = "Bills Passed",
          title = "Seniority and Bills Passed in the 115th Congress")

# Change colors of the marks 
ggplot(fig115,
       aes(x=seniority,
           y=all_pass,
           color=party)) + 
     geom_jitter() + 
     labs(x="Seniority",
          y="Bills Passed",
          title="Seniority and Bills Passed in the 115th Congress") + 
     scale_color_manual(values = c("blue",
                                   "red"))

# Make two separate plots using facet_wrap
ggplot(fig115,
       aes(x=seniority,
           y=all_pass,
           color=party)) + 
     geom_jitter()+
     labs(x="Seniority",
          y="Bills Passed",
          title="Seniority and Bills Passed in the 115th Congress") + 
     scale_color_manual(values = c("blue",
                                   "red")) + 
     facet_wrap(~party)