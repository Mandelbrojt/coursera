#### PACKAGES ####
library(tidyverse)

#### DATASET ####
datos <- read_csv("./data/week 3/population_and_gdp.csv")
# NOTE: By default, GDP and Population values are in millions

# Convert GDP into "billions"
datos$gdp_usd <- datos$gdp_usd / 1000

#### BAR PLOT DATA ####
bar_data <- datos %>% 
        # Filter data were "year" value is "2019"
        # Drop missing values
        filter(year == "2019") %>% 
        # Group results by "continent" variable
        group_by(continent) %>% 
        # For each combination of grouping variables calculate the average of "gdp"
        summarise(avg_gdp = mean(gdp_usd, na.rm = TRUE)) %>%
        # Delete row with NA calculations
        drop_na()

# Use "continent" variable as factor and...
bar_data$continent <- factor(bar_data$continent, 
                             # Set "continent" values as levels by using...
                             levels = bar_data$continent[
                                     # "avg_gdp" values in ascending order
                                     order(bar_data$avg_gdp)])

#### BASIC BAR PLOT ####
# Create a Bar Plot with "bar_data"
bar_plot <- ggplot(bar_data, 
                   aes(x = continent, 
                       y = avg_gdp,
                       # Fill bar color based on "continent" variable
                       fill = continent)) + 
        # Set "stat" to "identity" to disable aggregation and provide "y" manually
        geom_bar(stat = "identity") 

#### CUSTOMIZED BAR PLOT ####
customized_bar_plot <- bar_plot + 
        
        labs(title = "Average GDP by Continent* in 2019",
             # Set the x-axis title to blank
             x = element_blank(),
             # Set the y-axis title
             y = "Current US$ (billions)",
             # Set the caption and use a line break
             caption = "Source: World Bank (2020). 
          \n * U.N. M49 Standard methodology for continent classification."
        ) +
        
        theme(
                # Set the axis line to black
                axis.line = element_line(colour = "royalblue"),
                # Set the x-axis text to blank
                axis.text.x = element_blank(),
                # Set x-axis ticks to blank
                axis.ticks.x = element_blank(),
                
                # Set legend title to blank
                legend.title=element_blank(),
                
                # Set the panel background to blank
                panel.background = element_blank(),
                # Set x grid to blank
                panel.grid.major.x = element_blank(),
                # Set the y grid color
                panel.grid.major.y = element_line(colour = "royalblue"),
                
                # Align title to the left and set its color to "blue"
                plot.title = element_text(color = "royalblue", hjust = 0.5),          
                # Align caption to the left
                plot.caption = element_text(hjust = 0)
        ) +
        
        # Use "comma" function to format y-axis scale...
        # to insert commas every 3 digits
        scale_y_continuous(labels = scales::comma)

#### SCATTER PLOT DATA ####
scatter_data <- datos %>% 
        # Select "year" where values are equal to "2019"
        filter(year == "2019") %>% 
        # Group by "continent" variable
        group_by(continent) %>% 
        # For each grouping variable calculate the average for all numeric variables
        summarise(avg_gdp = mean(gdp_usd, na.rm = TRUE), 
                  avg_pop = mean(total_pop, na.rm = TRUE)) %>%
        # Delete row with NA calculations
        drop_na()

# Use "continent" values as factors
scatter_data$continent <- factor(scatter_data$continent,
                                 # Set "continent" values as levels by using...
                                 levels = scatter_data$continent[
                                         # "avg_pop" values in ascending order
                                         order(scatter_data$avg_pop)])

#### BASIC SCATTER PLOT ####
scatter_plot <- ggplot(scatter_data,
                       aes(x = avg_gdp, 
                           y = avg_pop)) + 
        
        # Insert point values into the plot
        geom_point(colour = c("red", "green", "blue", 
                              "purple", "orange", "brown")) + 
        
        # Fit text labels smartly with "ggrepel" package
        ggrepel::geom_text_repel(scatter_data,
                        mapping = aes(x = avg_gdp,
                                      y = avg_pop,
                                      # Set "continent" variable values as labels
                                      label = continent))

#### CUSTOMIZED SCATTER PLOT ####
customized_scatter_plot <- scatter_plot +
        
        labs(
                # Set the plot title
                title = "Average GDP and Population by Continent in 2019",
                # Set the x-axis title
                x = "Current US$ (billions)",
                # Set the y-axis title
                y = "Total Population (millions)",
                # Add a caption
                caption = "Source: World Bank (2020).
          \n * U.N. M49 Standard methodology for continent classification."
        ) + 
        
        theme(
                # Set all axis line color to black
                axis.line = element_line(colour = "black"),
                # Do not show legend
                #legend.position = "none",
                # Set plot background to blank
                panel.background = element_blank(),
                # Set major grids properties
                panel.grid.major = element_line(
                        # Set line color to grey
                        colour = "grey90",
                        # Set line type to "dashed"
                        linetype = 2),
                
                # Adjust caption text to the left
                plot.caption = element_text(hjust = 0),
                # Adjust title properties
                plot.title = element_text(
                        # Set color to blue
                        color = "royalblue", 
                        # Set horizontal justification to the center
                        hjust = 0.5)
                
                # Set panel border properties
                #panel.border = element_rect(
                        # Set border color to blue
                        #colour = "royalblue", 
                        # Do not fill border
                        #fill = FALSE,
                        # Set border line to "dashed"
                        #linetype = 2)
        ) +
        
        # Use "comma" function to format x-axis scale...
        # to insert commas every 3 digits
        scale_x_continuous(labels = scales::comma)


#### BOX PLOT DATA ####
box_plot_data <- datos %>%
        # Filter data were year is equal to "2019"
        filter(year == "2019") %>%
        # Delete NA row values
        drop_na()
#### BASIC BOX PLOT ####
basic_box_plot <- box_plot_data %>%
        ggplot(aes(x = factor(continent, 
                              # Set levels order manually...
                              # based on median value of each continent
                              levels = c("Oceania", "North America", "Europe", 
                                         "Asia", "Africa", "South America")),
                   y = total_pop)) + 
        # Apply the Edward Tuftle Boxplot theme
        ggthemes::theme_tufte(ticks = FALSE)

#### CUSTOMIZED BOX PLOT ####
customized_box_plot <- basic_box_plot + 
        ggthemes::geom_tufteboxplot() + 
        
        # Set labels values
        labs(
                title = "Total Population by Continent in 2019",
                subtitle = "Tufte's Box Plot",
                caption = "Source: World Bank (2020).
                \n * U.N. M49 Standard methodology for continent classification.",
                x = "Continent",
                y = "Total Population (millions)"
        ) +
        
        # Customize plot objects style
        theme(
                panel.grid.major.y = element_line(colour = "grey80",
                                                  linetype = 2),
                plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5, 
                                             color = "royalblue"),
                plot.caption = element_text(hjust = 0)
        ) +
        # Set y-axis scale to logarithmic
        scale_y_log10(labels = scales::trans_format("log10",
                                                    scales::math_format(10^.x)))


#### LINE PLOT DATA ####
line_plot_data <- datos %>% 
        filter(year == c(2009:2019)) %>% 
        select(year, country_name, 
               continent, total_pop) %>% 
        drop_na() %>%
        group_by(continent, year) %>% 
        summarise(avg_pop = mean(total_pop))
#### BASIC LINE PLOT ####
basic_line_plot <- datos %>% 
        filter(between(year,2015,2019) & 
                       continent == "North America" &
                       total_pop > 30) %>% 
        arrange(year) %>%
        ggplot(mapping = aes(x = year,
                             y = gdp_usd,
                             group = 1)) +
        geom_line() +
        geom_point() +
        facet_wrap(~country_name)

### CUSTOMIZED LINE PLOT ####
customized_line_plot <- basic_line_plot +
        labs(
                title = "GDP by Country",
                subtitle = "Biggest Countries in North America by Total Population",
                caption = "Source: World Bank (2020).
                \n * U.N. M49 Standard methodology for continent classification.",
                x = "Year",
                y = "Current US$ (billions)"
        ) + 
        
        theme(
             plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             plot.caption = element_text(hjust = 0),
             panel.spacing = unit(1, "cm")
        ) + 
        scale_y_log10() +
        scale_x_continuous(breaks = c(2015, 2017, 2019))
