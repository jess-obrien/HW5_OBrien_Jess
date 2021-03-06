# HW5_OBrien_Jess
Homework 5 - Shiny Dashboard

Deploy locally using this code:


shiny::runGitHub("HW5_OBrien_Jess","staa566-csu-sp2022")



**Data Sources:** Asthma data - https://ephtracking.cdc.gov/DataExplorer/ (Select Asthma > Asthma Prevalence Among Children > Percent of Children Ever Diagnosed with Asthma > Michigan )
Ozone data - https://www.epa.gov/outdoor-air-quality-data/download-daily-data (use filters to pull Ozone levels from Michigan during 2020)

**Background:** This Shiny dashboard explores air quality and associated health conditions in the state of Michigan. 

The first tab displays the daily maximum ozone levels in 4 Michigan counties (2 rural and 2 urban) during 2020. I used a plotly graph with a rangeslider so I could zoom in on specific date ranges. For example, pinpointing the summer peak for Benzie counties shows that the ozone was very close to the danger zone of 0.1 ppm on June 19th. Including different colored lines for each county allows for the comparison between them, while hovering over a point gives the exact ozone concentration value for each county on that date (x-value). Double-clicking one of the counties on the legend will cause the graph to display only that county, making it easier to examine it more closely without noise from the other three counties.

The second tab features a plot of the prevalence of childhood asthma in Michigan from 2011 to 2019. Studies have shown an association between exposure to air pollutants and the triggering or worsening of asthma symptoms in children. Public health officials may use childhood asthma rates in conjunction with data on air pollution levels to identify regions to target for outreach on asthma treatment and prevention. There are 4 age groups in the data, displayed on the plot as separate lines. The range slider allows for a closer look at a particular time frame, while the hover function displays the value for each age group during a particular year. 

The third tab features a table of the 25 highest instances of ozone levels in Michigan for 2020. This table could be helpful in identifying counties that experience high ozone levels and explore pollution mitigation options for those areas. 
