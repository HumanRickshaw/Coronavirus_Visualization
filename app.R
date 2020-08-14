##############PACKAGES##############

library(DT)
library(ggplot2)
library(ggthemes)
library(grid)
library(growthcurver)
library(knitr)
library(scales)
library(shiny)
library(tidyr)
library(tidyverse)
library(urbnmapr)
library(viridis)


##############FUNCTIONS##############

#Distribute Kansas City amongs Cass, Clay, Jackson, and Platte Counties.
kcEdit <- function(df) {
  
  temp_counties <- c("Cass", "Clay", "Jackson", "Platte")
  
  clay_df <- data.frame("date" = seq(as.Date("2020-03-20"), by = "day", length.out = 3),
                        "county" = "Clay",
                        "state" = "Missouri",
                        "fips" = 29047,
                        "cases" = 0,
                        "deaths" = 0)
  
  platte_df <- data.frame("date" = seq(as.Date("2020-03-20"), by = "day", length.out = 6),
                          "county" = "Platte",
                          "state" = "Missouri",
                          "fips" = 29165,
                          "cases" = 0,
                          "deaths" = 0)

  df <- rbind(df, clay_df)
  df <- rbind(df, platte_df)
  kc_df <- df[df$state == "Missouri" & df$county == "Kansas City",]
  df <- df[!(df$state == "Missouri" & df$county == "Kansas City"),]
  df <- df[order(df$date),]
  
  for (x in kc_df$date) {
    df[df$state == "Missouri" & df$county %in% temp_counties & df$date == x,]$cases <-
      df[df$state == "Missouri" & df$county %in% temp_counties & df$date == x,]$cases + as.integer(kc_df[kc_df$date == x,]$cases / 4)

    df[df$state == "Missouri" & df$county %in% temp_counties & df$date == x,]$deaths <-
      df[df$state == "Missouri" & df$county %in% temp_counties & df$date == x,]$deaths + as.integer(kc_df[kc_df$date == x,]$deaths / 4)
  }
  df
}

#State, Territory, or DC?
division <- function(state_i) {
  if (state_i == "District of Columbia") {
    "Capital"
  } else if (state_i %in% territory.name){
    "Territory"
  } else {
    "State"
  }
}

#Alaska has Boroughs, Louisiana has Parishes, all else have Counties.
subdivision <- function(state_i) {
  if (state_i == "Alaska") {
    "Borough"
  } else if (state_i == "Louisiana"){
    "Parish"
  } else {
    "County"
  }
}

#Helper functions for dataframes for graphing.
getDate <- function(df, value) {
  df %>%  filter(date == value)
}
getState <- function(df, value) {
  df %>% filter(state_name == value)
}

getCounty <- function(df, value) {
  df %>% filter(county_name == value)
}

getType <- function(df, value) {
  df %>% filter(type == value)
}

#Helper functions for sidePanel options
#for State Graph and County Graph.

#Show or hide points on the graph.
#showZeroes is also used in State Map.
showZeroes <- function(df, boolean, thresh, cd) {
  #For State Graph and County Graph.
  #Show Cases greater than threshold.
  if (boolean == FALSE & cd == 1) {
    df %>% filter(Cases >= thresh)
    
  #Show Deaths greater than threshold.
  } else if (boolean == FALSE & cd  == 2) {
    df %>% filter(Deaths >= thresh)
  } else if (cd %in% c(1:5)) {
    df
    
  #For Chloropleth or Bubble Plot.  
  } else {
    if (boolean == FALSE) {
      df %>% filter(people >= thresh)
    } else {
      df %>% filter(people >= 1)
    }
  }
}

#Show State of Emergency Declaration or Stay at Home.
createVertical <- function(ggobject, boolean, temp_data, es, linecolor) {
  #Default no lines.
  if (boolean == FALSE) {
    return(ggobject)
  } else {
    #State of Emergency
    if (es == "e") {
      linetype = "dotted"
      size = 2
    } else {
      #Stay at Home
      if (is.na(temp_data$date)) {
        return(ggobject)
      } else {
        linetype = "dashed"
        size = 1.5
      }
    }
    ggobject + geom_vline(xintercept = temp_data$date,
                          color = linecolor,
                          linetype = linetype, size = size)
  }
}

#Points and Line(s) for State Graphs and County Graphs.
pointsInfo <- function(con_p, data_c, gg_object, plot_color, df, date_i) {
  df <- df %>% filter(people > 0)
  #Initial setup of points.
  gg_object <- gg_object + geom_point(color = plot_color[2], size = 6)
  #No lines or no points if switching from cases to deaths quickly
  if (con_p == 0 || length(df$date) == 0) {
    gg_object
  #Connect initial points.  
  } else if (con_p == 1) {
    gg_object + geom_line(color = plot_color[2], size = 1.1)
  #If Cases or Deaths  
  } else if (data_c %in% c(1, 2)) {
    if (tail(df[,2], 1) > 0) {
      dfx <- df %>% mutate(date = as.integer(date))
      logistic_model <- logisticModel(dfx)
      h_asym <- as.integer(logistic_model$vals[[1]])
      df.pred <-  data.frame(date = as.Date(min(dfx$date):(max(dfx$date) + 30), origin = "1970-01-01"),
                             Cases = predict(logistic_model$model,
                                             newdata = list(t = 0:(max(dfx$date) - min(dfx$date) + 30))))
      #Color the points used for the regression.
      gg_object <- gg_object + geom_point(data = df, mapping = aes(y = df[,2]), color = plot_color[7], size = 6)
      #Draw the regression line.
      gg_object <- gg_object + geom_line(df.pred, mapping = aes(y = df.pred[, 2]), color = plot_color[7], size = 1.5)
      #Draw the horizontal asymptote.
      gg_object <- gg_object + geom_hline(yintercept = h_asym, color = plot_color[7], linetype = "longdash")
      gg_object  
    }
  }
}

#Create logistic model.
logisticModel <- function(df) {
  SummarizeGrowth(df$date - min(df$date), df[, 2])
}



#Sets x-axis for graphs.
xDate <- function(gg_object, date_min, date_max, linetype) {

  temp_date <- as.Date("2020-04-15") + 15 * (as.numeric(linetype) - 2)
  if (temp_date > date_max) {
    date_max <- temp_date
  }
    gg_object + scale_x_date(name = "Date",
                             limits = c(date_min, date_max),
                             date_breaks = "7 days",
                             date_labels = "%B %d",
                             expand = c(0, 0))
}

#Sets y-axis for graphs.
yScale <- function(gg_object, d_c, g_s) {
  if (d_c == 5) {
    gg_object + scale_y_continuous(labels = percent)  
  } else {
    #Linear/Logarithmic
    if (g_s == "Linear") {
      gg_object + scale_y_continuous(labels = comma)
    } else {
      gg_object + scale_y_log10(labels = comma)
    }
  }
}

themeDetails <- function(gg_object) {

  gg_object + theme(plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
                    axis.text.x = element_text(hjust = 1, size = 18, angle = 45),
                    axis.text.y = element_text(size = 18),
                    axis.title = element_text(size = 22, face = "bold"))
}

#Helper function for states_Data_Table and counties_Data_Table for
#State Comparisons and County Comparisons Tabs, respectively.
createMetrics <- function(df, state_i) {
  
  df %>%
    rename(Population = population) %>%
    mutate(pr = Cases * 1000 / Population,
           mr = Deaths * 100000 / Population,
           crid = round(Deaths / Cases, 5))
}



#Helper functions for Bubble Plot Scale.
getScale <- function(df) {
  scale_200 <- c(200, 2000, 20000, 200000)
  scale_100 <- c(100, 1000, 10000, 100000)
  scale_50 <- c(50, 500, 5000, 50000)
  scale_20 <- c(20, 200, 2000, 20000)
  scale_10 <- c(10, 100, 1000, 10000)
  scale_5 <- c(5, 50, 500, 5000)
  scale_2 <- c(2, 20, 200, 2000)
  scale_1 <- c(1, 10, 100, 1000)
  temp_max <- max(df$people)
  test_max <- c(200000, 100000, 50000, 20000, 10000, 5000, 2000, 1000, 500, 200, 100, 50, 20, 10, 5, 2)
  scale_out <- list(scale_200, scale_100, scale_50, scale_20, scale_10, scale_5, scale_2, scale_1,
                    scale_5, scale_2, scale_1, scale_5, scale_2, scale_1, scale_5, scale_2)
  for (x in 1:length(test_max)) {
    if (temp_max >= test_max[x]) {
      return(scale_out[[x]])
    }
  }
}



#Helper function for growths.  
buildGrowthDF <- function(df, cd) {
  
  df <- getType(df, cd)
  
  xs <- c(5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000,
          10000, 20000, 50000, 100000, 200000, 500000,
          1000000, 2000000, 5000000)
  
  people_x <- NULL
  diff_x <- NULL
  #Collect the first date when each value in xs is reached.
  for (x in xs) {
    temp_x <- df[df$people >= x,]$date[1]
    people_x <- c(people_x, temp_x)
  }
  #Subtract pairs of every other value.
  for (x in 1:(length(xs) - 3)) {
    temp_x <- people_x[x+3] - people_x[x]
    diff_x <- c(diff_x,temp_x)
  }
  #Some states/territories have not yet reached 50 cases/deaths.
  if (is.na(diff_x[1])) {
    temp_time <- "NA"
  } else {
    temp_time <- paste("~", diff_x[1], "days")
  }
  #Initialize output dataframe.
  growth = data.frame(Growth = paste("From 5 to 50", cd),
                      Time = temp_time)
  #Bind new rows of additional differences with the number of days 
  #if the cases/deaths have been reached.
  for (x in 2:(length(diff_x))) {
    if (!is.na(diff_x[x])) {
      temp_df = data.frame(Growth = paste("From", prettyNum(as.integer(xs[x]), big.mark = ","),
                                          "to", prettyNum(as.integer(xs[x+3]), big.mark = ","), cd),
                           Time = paste("~", diff_x[x], "days"))
      growth <- rbind(growth, temp_df)
    }
  }
  growth
}



#Helper function for State Comparisons and County Comparison.
#Inputs two dataframes(US and States, or State and Counties) state or county, a date, and data choice
#(Cases, Deaths, Prevalence Rate, Mortality Rate, or Cases Resulting in Deaths).
#
#Using the us_Data_Table and states_Data_Table, it will output the US value, the state
#value, and a Table comparing states to the chosen state.
#
#or
#
#Using the state_Data_Table and counties_Data_Table, it will output the state value, the county
#value, and a Table comparing counties to the chosen state.
comparisonsData <- function(big_df, small_df, big_name, small_name, sc, date_i, var1) {

  #Combine us_Data_Table and states_Data_Table,
  #or state_Data_Table and counties_Data_Table.
  mini_Table <- rbind(getDate(big_df, date_i),
                      getDate(small_df, date_i))
  mini_Table <- mini_Table %>%
    mutate(pr = round(pr, 2),
           mr = round(mr, 2)) %>%
    rename(Prevalence_Rate = pr,
           Mortality_Rate = mr,
           Cases_Resulting_in_Death = crid)
  
  #Display full table.
  if (var1 == 6) {
    mini_Table <- mini_Table %>%
      mutate(Cases_Resulting_in_Death = paste(round(Cases_Resulting_in_Death * 100, 2), "%", sep="")) %>%
      select(1,2,4:8)
    if (sc == "s") {
      mini_Table <- mini_Table %>%
        rename(State = state_name)
    } else {
      if (big_name == "Alaska") {
        mini_Table <- mini_Table %>%
          rename(Borough = county_name)  
      } else if (big_name == "Louisiana"){
        mini_Table <- mini_Table %>%
          rename(Parish = county_name)
      } else {
        mini_Table <- mini_Table %>%
          rename(County = county_name)
      }
    }
    
    #Individual options.      
  } else {
    #Retrieve Data for output.
    if (sc == "s") {
      temp_big_value <- getState(mini_Table, big_name)[,(var1 + 3)]
      temp_small_value <- getState(mini_Table, small_name)[,(var1 + 3)]
    } else {
      temp_big_value <- getCounty(mini_Table, big_name)[,(var1 + 3)]
      temp_small_value <- getCounty(mini_Table, small_name)[,(var1 + 3)]
    }
    
    #Make mini_Table a mini table. Order by option of choice.
    if (sc == "s") {
      mini_Table <- mini_Table %>%
        rename(State = state_name)
    } else {
      mini_Table <- mini_Table %>%
        rename(County = county_name)
    }
    mini_Table <- mini_Table %>%
      select(c(1, (var1 + 3)))
    mini_Table <- mini_Table[order(-mini_Table[, 2]),]
    
    #Cases or Deaths.  They have to be formatted after order.
    if (var1 %in% c(1,2)) {
      temp_perc <- paste(round(temp_small_value * 100 / temp_big_value, 2), "%", sep="")
      temp_big_value <- prettyNum(temp_big_value, big.mark = ",")
      temp_small_value <- paste(prettyNum(temp_small_value, big.mark = ","),
                                "or", temp_perc, "of", big_name, y_labels[var1])
      mini_Table[,2] <- prettyNum(mini_Table[,2], big.mark = ",")
      
      #CRID has to be formatted after ordering.
    } else if (var1 == 5) {
      mini_Table <- mini_Table %>%
        mutate(Cases_Resulting_in_Death = paste(round(Cases_Resulting_in_Death * 100, 2), "%", sep=""))
      temp_big_value <- paste(round(temp_big_value * 100, 2), "%", sep = "")
      temp_small_value <- paste(round(temp_small_value * 100, 2), "%", sep = "")
    }
    
    #Final edit of mini_Table.  Clear rownames, get rid of US, create Rank, place Rank as first
    #column
    if (sc == "s") {
      mini_Table <- mini_Table %>%
        filter(State != big_name)
    } else {
      mini_Table <- mini_Table %>%
        filter(County != big_name)
    }
    
    rownames(mini_Table) <- NULL
    mini_Table <- mini_Table %>%
      mutate(Rank = rownames(mini_Table)) %>%
      select(3,1,2)
    
    #Delaware has only 3 counties, Hawaii and Rhode Island have 5 counties.
    if (!(big_name %in% c("Delaware", "Hawaii", "Rhode Island"))) {
      #mini_Table displays the first 5 rows, last 5 rows, or 2 rows above and below selected state. 
      if (sc == "s") {
        list_index <- as.integer(row.names(mini_Table[mini_Table$State == small_name,]))
      } else {
        list_index <- as.integer(row.names(mini_Table[mini_Table$County == small_name,]))
      }
      
      if (list_index %in% c(1, 2)) {
        grouping <- c(1:5)
      } else if (list_index %in% c(nrow(mini_Table) - 1, nrow(mini_Table))) {
        grouping <- c((nrow(mini_Table) - 4):nrow(mini_Table))
      } else {
        grouping <- c((list_index - 2):(list_index + 2))
      }
      
      if (big_name == "Alaska") {
        mini_Table <- mini_Table[grouping,] %>%
          rename(Borough = County)  
      } else if (big_name == "Louisiana"){
        mini_Table <- mini_Table[grouping,] %>%
          rename(Parish = County)
      } else {
        mini_Table <- mini_Table[grouping,]
      }

    }
    #Ouput US data info, state data info, and mini_Table.
    list(paste(big_name, y_labels[var1], ":", temp_big_value),
         paste(small_name, y_labels[var1], ":", temp_small_value),
         mini_Table )
  }
}

#Helper function for County Graph, Comparisons, and Growth Info.
#Creates a list of counties for a state.
countyList <- function(state_i) {
  if (state_i == "New York") {
    ny_df <- unique(counties_Mapping %>%
                      filter(state_name == state_i) %>%
                      select(county_name))
    ny_df <- ny_df %>%
      add_row(county_name = "New York City")
    ny_df[order(ny_df$county_name),]
  } else {
    df <- unique(counties_Mapping %>%
                   filter(state_name == state_i) %>%
                   select(county_name))
    df[order(df$county_name),]
  }
}



#Helper function for County Graph, Comparisons, and Growth Info.
noCasesText <- function(state_i, county_i) {
  #The five counties of New York City are empty because all data is in New York City.
  #All other counties are simply empty.
  if (!(state_i == "New York" & county_i %in% nyc_counties)) {
    paste(county_i, "currently has no confirmed/probable cases.")
  }
}

#Outputs a footnote for specific county exceptions.
countyExceptions <- function(state_i, county_i) {
  #Wait for county input to load.
  if (is.null(county_i)) {
  } else {
    #Alameda County, CA.
    if (state_i == "California" & county_i == "Alameda County") {
      california_exception
      
      #Cook and Dupage Counties, IL.
    } else if (state_i == "Illinois" & county_i %in% c("Cook County", "DuPage County")) {
      illinois_exception
      
      #Douglas County, NE.
    } else if (state_i == "Nebraska" & county_i == "Douglas County") {
      nebraska_exception
      
      #Kansas City, MO.
    } else if (state_i == "Missouri" & county_i %in% kc_counties) {
      missouri_exception
      
      #New York City, NY.
    } else if (state_i == "New York" & county_i %in% c(nyc_counties, "New York City")) {
      nyc_exception
    } 
  } 
}



stateExceptions <- function(state_i) {
  #Alameda County, CA.
  if (state_i == "California") {
    california_exception
    
    #Cook and Dupage Counties, IL.
  } else if (state_i == "Illinois") {
    illinois_exception
    
    #Douglas County, NE.
  } else if (state_i == "Nebraska") {
    nebraska_exception
    
    #Kansas City, MO.
  } else if (state_i == "Missouri") {
    missouri_exception
    
    #New York City, NY.
  } else if (state_i == "New York") {
    nyc_exception
  } 
} 



#Helper function for Links.  Creates HTML.
createHTML <- function(description, title, link, end) {
  tagList(description, a(title, href = link), end)
}



##############LISTS AND CONSTANTS##############

#States, Territories, and Abbreviations Lists.
territory.abb <- c("AS", "GU", "PR", "MP", "VI")
territory.name <- c("American Samoa", "Guam", "Puerto Rico", "Northern Mariana Islands", "Virgin Islands")
abb.56 <- c(state.abb, "DC", territory.abb)
state.list <- sort(c(state.name, "District of Columbia", territory.name))

cd_list <- c("confirmed/probable cases", "confirmed/probable deaths")

reg_dates <- as.Date(c("2020-03-15",
                       "2020-03-31",
                       "2020-04-15",
                       "2020-04-30",
                       "2020-05-15",
                       "2020-05-31",
                       "2020-06-15",
                       "2020-06-30",
                       "2020-07-15",
                       "2020-07-30",
                       as.character(Sys.Date() - 2)))
regression_list <- c("None" = 0,
                     "Connect Points" = 1,
                     "March 15th Regression" = 2,
                     "March 31st Regression" = 3,
                     "April 15th Regression" = 4,
                     "April 30th Regression" = 5,
                     "May 15th Regression" = 6,
                     "May 31st Regression" = 7,
                     "June 15th Regression" = 8,
                     "June 30th Regression" = 9,
                     "July 15th Regression" = 10,
                     "July 31st Regression" = 11,
                     "Current Regression" = 12)

#Kansas City Counties
kc_counties <- c("Cass County", "Clay County", "Jackson County", "Platte County")

#New York City Counties.
nyc_counties <- c("Bronx County", "Kings County", "New York County", "Richmond County", "Queens County")

#Independent Cities
independent_cities <- c("Baltimore (city)", "Carson City", "St. Louis (city)", "New York City")

#Graph y-labels
y_labels = c("Confirmed/Probable Cases",
             "Confirmed/Probable Deaths",
             "Prevalence Rate (Cases per 1,000)",
             "Mortality Rate (Deaths per 100,000)",
             "Cases Resulting in Deaths")


california_exception <- "Alameda County also includes data from the Grand Princess cruise ship."
illinois_exception <- "All data for Chicago are included in Cook County.  None are included in DuPage County."
guam_exception <- "Guam also includes data from the USS Theodore Roosevelt."
nebraska_exception <- "Douglas County also includes data from the Diamond Princess cruise ship."
missouri_exception <- "Data for Kansas City was separate, and has been divided evenly and added to Cass, Clay, Jackson, and Platte Counties."
nyc_exception <- "Data for Bronx, Kings, Queens, New York, and Richmond Counties are contained in New York City."

##############DATASETS##############

#Population - contains all 50 States, DC, and 5 Territories 
states_Population <- read.csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv?#")[c(6:57), c(5, 17)]
colnames(states_Population) <- c("state_name", "population")
states_Population <- states_Population %>%
  mutate(state_name = as.character(state_name))
temp <- data.frame("state_name" = c("American Samoa", "Guam", "Northern Mariana Islands", "Virgin Islands"),
                   "population" = c(55191, 168775, 57559, 104425))
states_Population <- rbind(states_Population, temp)
states_Population <- states_Population[order(states_Population$state_name),]
rownames(states_Population) <- NULL



#Population of Counties
counties_Population <- read.csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv")
counties_Population <- counties_Population %>%
  select(7, 6, 19) %>%
  `colnames<-`(c("county_name", "state_name", "population")) %>%
  mutate(county_name = as.character(county_name),
         state_name = as.character(state_name))

#Dona Ana's Spanish 'n' causes a UTF8 error, change to 'Dona Ana'.
counties_Population[counties_Population$population == 218195,]$county_name <- "Dona Ana County"

#Baltimore and St. Louis have adjacent independent cities to counties of same name.
counties_Population[counties_Population$county_name == "Baltimore city",]$county_name <- "Baltimore (city)"
counties_Population[counties_Population$county_name == "St. Louis city",]$county_name <- "St. Louis (city)"


#New York City Edit.  Eliminate five counties and add their populations for New York.
counties_Population <- rbind(counties_Population[!(counties_Population$state_name == "New York" & counties_Population$county_name %in% nyc_counties),],
                             data.frame("county_name" = "New York City",
                                        "state_name" = "New York",
                                        "population" = counties_Population %>%
                                          filter(state_name == "New York",
                                                 county_name %in% nyc_counties) %>%
                                          select(population) %>%
                                          sum()))



#States - contains all 50 States, DC, and 5 Territories.  American Samoa still has no confirmed cases.
#Columns:
#date, state, state_fips, type (Cases or Deaths), people
states_Corona <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
states_Corona <- states_Corona %>%
  mutate(date = as.Date(date),
         state = as.character(state)) %>%
  `colnames<-`(c("date", "state_name", "state_fips", "Cases", "Deaths"))

states_Data_Table <- createMetrics(full_join(states_Population,
                                             states_Corona[,c(1,2,4,5)],
                                             by = "state_name"), "NULL")
states_Corona <- states_Corona %>%
  gather(type, people, c(Cases, Deaths))  
#Some are territories_Corona.
territories_Corona <- states_Corona %>%
  filter(state_name %in% territory.name)


#Counties - contains all 50 States, DC, and 5 Territories.  American Samoa still has no confirmed cases.
#Columns:
#date, county, state, county_fips, type (Cases or Deaths), people
#county is unknown and county_fips is NA for 5 Territories.
counties_Corona <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
counties_Corona<- counties_Corona %>%
  mutate(date = as.Date(date),
         county = as.character(county),
         state = as.character(state))

counties_Corona <- kcEdit(counties_Corona)
#Rename.
counties_Corona[counties_Corona$county == "Anchorage",]$county <- "Anchorage Municipality"
counties_Corona[counties_Corona$county == "Baltimore city",]$county <- "Baltimore (city)"
counties_Corona[counties_Corona$county == "St. Louis city",]$county <- "St. Louis (city)"
counties_Corona[counties_Corona$fips == 35013 & !(is.na(counties_Corona$fips)),]$county <- "Dona Ana"

#Append 'County' to the end of each County.  Eliminate Unknown, independent cities, Alaska, DC, and Louisiana first.
counties_Corona[!(counties_Corona$county %in% c(independent_cities, "Unknown")) & !(counties_Corona$state %in% c("Alaska", "District of Columbia", "Louisiana")),]$county <-
  paste(counties_Corona[!(counties_Corona$county %in% c(independent_cities, "Unknown")) & !(counties_Corona$state %in% c("Alaska", "District of Columbia", "Louisiana")),]$county, "County")
#Append 'Parish' to the end of each Louisiana Parish. 
counties_Corona[counties_Corona$state  == "Louisiana",]$county <- paste(counties_Corona[counties_Corona$state  == "Louisiana",]$county, "Parish")

counties_Corona <- counties_Corona %>%
  `colnames<-`(c("date", "county_name", "state_name", "county_fips", "Cases", "Deaths"))

counties_Corona_Data <- data.frame(counties_Corona)

counties_Corona <- counties_Corona %>%
  gather(type, people, c(Cases, Deaths))


#For US Comparisons Tab.
us_Data_Table <- counties_Corona %>% select(1,5,6)
us_Data_Table <- aggregate(us_Data_Table$people, list(us_Data_Table$date, us_Data_Table$type), FUN = sum)
us_Data_Table <- us_Data_Table %>%
  spread(Group.2, x) %>%
  rename (date = Group.1) %>%
  mutate(state_name = "United States",
         Population = 331883986,
         pr = Cases * 1000 / Population,
         mr = Deaths * 100000 / Population,
         crid = Deaths / Cases)
us_Data_Table <- us_Data_Table[,c(4,5,1,2,3,6,7,8)]



#Data with 'Unknown' in county column.
counties_Corona_Unknown <- counties_Corona %>%
  filter(county_name == "Unknown")
#Others are states.
counties_Corona_Unknown <- counties_Corona_Unknown %>%
  filter(!state_name %in% territory.name)
#Complete county and state data.
counties_Corona <- counties_Corona %>%
  filter(county_name != "Unknown")
#New York City.
new_York_City <- counties_Corona %>%
  filter(county_name == "New York City")


#County Outline mapping Data.
counties_Mapping <- as.data.frame(counties)
counties_Mapping <- counties_Mapping %>%
  mutate(county_fips = as.integer(county_fips),
         county_name = as.character(county_name))

#Rename
counties_Mapping[counties_Mapping$county_name == "Petersburg Census Area",]$county_name <- "Petersburg Borough"

#Baltimore and St. Louis have adjacent independent cities to counties of same name.
counties_Mapping[counties_Mapping$county_name == "Baltimore city",]$county_name <- "Baltimore (city)"
counties_Mapping[counties_Mapping$county_name == "St. Louis city",]$county_name <- "St. Louis (city)"

#Territory outline mapping data.
territories_Mapping <- get_urbn_map(map = "ccdf")
territories_Mapping <- (territories_Mapping %>%
  filter(state_abbv %in% territory.abb)) %>%
  mutate(state_name = ifelse(state_abbv == "VI", "Virgin Islands",
                             ifelse(state_abbv == "MP", "Northern Mariana Islands",
                                    state_name)))



#ccdf is not a global location.  Averaging map values to approximate centers of territories.
temp_lats <- NULL
temp_longs <- NULL
for (name in territory.name) {
  temp_lats <- c(temp_lats, mean(territories_Mapping[territories_Mapping$state_name == name,]$lat))
  temp_longs <- c(temp_longs, mean(territories_Mapping[territories_Mapping$state_name == name,]$long))
}
territories_Lats_Longs <- data.frame(state_name = territory.name,
                          lat = temp_lats,
                          long = temp_longs)
territories_Lats_Longs <- territories_Lats_Longs %>%
  mutate(state_name = as.character(state_name))



#Center Lat and Long of Counties.
counties_Lats_Longs <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")[,c(5,6,7,9,10)]
counties_Lats_Longs <- counties_Lats_Longs %>%
  `colnames<-`(c("county_fips", "county_name", "state_name", "lat", "long"))
counties_Lats_Longs <- counties_Lats_Longs %>%
  mutate(county_fips = as.integer(county_fips),
         county_name = as.character(county_name),
         state_name = as.character(state_name))
counties_Lats_Longs <- counties_Lats_Longs[c(6:nrow(counties_Lats_Longs)), ]
#Baltimore and St. Louis have adjacent independent cities to counties of same name.
counties_Lats_Longs[counties_Lats_Longs$county_name == "Baltimore City",]$county_name <- "Baltimore (city)"
counties_Lats_Longs[counties_Lats_Longs$county_name == "St. Louis City",]$county_name <- "St. Louis (city)"

counties_alaska <- (counties_Lats_Longs %>%
                      filter(state_name == "Alaska") %>%
                      select(c(county_name, county_fips)))[c(1:29),]
#counties_alaska[counties_alaska$county_name == "Petersburg Census Area",]$county_name <- "Petersburg Borough"
counties_hawaii <- (counties_Lats_Longs %>%
                      filter(state_name == "Hawaii") %>%
                      select(c(county_name, county_fips)))[c(1:5),]

for (temp_fips in c(counties_alaska$county_fips,counties_hawaii$county_fips)) {
  counties_Lats_Longs[!(is.na(counties_Lats_Longs$county_fips)) & counties_Lats_Longs$county_fips == temp_fips,]$lat <-
    mean(counties_Mapping[counties_Mapping$county_fips == temp_fips,]$lat)
  counties_Lats_Longs[!(is.na(counties_Lats_Longs$county_fips)) & counties_Lats_Longs$county_fips == temp_fips,]$long <-
    mean(counties_Mapping[counties_Mapping$county_fips == temp_fips,]$long)
}



#Date states declared State of Emergency.
emergency <- read.csv("./StatesEmergency.csv") %>% mutate(date = as.Date(date))



##############UI##############
ui <- fluidPage(
  titlePanel("US Coronavirus Dashboard"),
  sidebarLayout(
    ##############SIDEBAR PANEL##############
    sidebarPanel(width = 3,
      #Used in at least 2 of the tabs.
      h6("Testing in progress...still debugging, adding functionality, including Oxford commas, and learning R."),
      h3("Main Settings"),
      selectInput("state_i", "State or Territory :", state.list),
      conditionalPanel(condition = "(input.mytabs == 5 || input.mytabs == 6 || input.mytabs == 7)",
                       selectInput("county_i", "County", c(""))),
      sliderInput("date_in",
                  "Date range :",
                  as.Date("2020-01-22"),
                  as.Date(Sys.Date() - 1),
                  value = c(as.Date("2020-03-01"), as.Date(Sys.Date() - 2))),
      h6("(Data is generally complete until one or two days before current date.)"), 
      #Used exclusively in Graph and Map Tabs.
      conditionalPanel(condition = "(input.mytabs == 1 && (input.data_choice == 1 || input.data_choice == 2)) || input.mytabs == 4 || (input.mytabs == 5 && (input.data_choice == 1 || input.data_choice == 2))",
                       checkboxInput("show_hide",
                                     "Show all cases/deaths",
                                     value = FALSE),
                       conditionalPanel(
                         condition = "input.show_hide == false",
                         sliderInput("threshold",
                                     "Hide cases/deaths less than :",
                                     1,100,2))),
      conditionalPanel(condition = "input.mytabs == 1 || input.mytabs == 4 || input.mytabs == 5",
                       selectInput("col_sch",
                                   "Color Gradient :",
                                   c("Blues", "Greens", "Heat", "Purples", "Reds", "Viridis"))),
      
      #Used exclusively in Graph Tabs.
      conditionalPanel(condition = "input.mytabs == 1 || input.mytabs == 5",
                       h3("Graph Settings"),
                       selectInput("data_choice",
                                   "Data to Display :",
                                   c("Confirmed/Probable Cases" = 1,
                                     "Confirmed/Probable Deaths" = 2,
                                     "Prevalance Rate" = 3,
                                     "Mortality Rate" = 4,
                                     "Cases Resulting in Deaths" = 5)),
                       conditionalPanel(condition = "input.data_choice != 5",
                                        selectInput("graph_scale",
                                                    "Scale :",
                                                    c("Linear", "Logarithmic"))),
                       selectInput("con_pts",
                                   "Display Line :",
                                   regression_list[c(1, 2)])),

      conditionalPanel(condition = "input.mytabs == 1",
                       checkboxInput("states_emer",
                                     "Show/Hide State of Emergency Declaration",
                                     value = FALSE),
                       checkboxInput("states_stay",
                                     "Show/Hide Stay at Home Order",
                                     value = FALSE)),
      
      #Used exclusively in Map Tab.
      conditionalPanel(condition = "input.mytabs == 4",
                       h3("Map Settings"),
                       selectInput("map_display",
                                   "Display Style :",
                                   c("Chloropleth", "Bubble Plot")),
                       selectInput("map_data",
                                   "Data to Display :",
                                   c("Cases", "Deaths"))),
      
      #Used exclusively in Comparisons Tab.
      conditionalPanel(condition = "input.mytabs == 2 || input.mytabs == 6",
                       h3("Comparison Settings"),
                       selectInput("table_choice",
                                   "Data to Display :",
                                   c("Cases" = 1,
                                     "Deaths" = 2,
                                     "Prevalance Rate" = 3,
                                     "Mortality Rate" = 4,
                                     "Cases Resulting in Deaths" = 5,
                                     "Show Complete Table" = 6)))),

    
        
    ##############MAIN PANEL##############
    mainPanel(width = 9,
      navbarPage(uiOutput("main_panel"), id = "mytabs",
                 
                 tabPanel(uiOutput("tab1_title"), value = 1,
                          br(),
                          plotOutput("state_plot", height = 600),
                          conditionalPanel(condition = "input.data_choice == 1 || input.data_choice == 2",
                                           strong(span(uiOutput("tab1_regression"), style = "font-size: 12px"))),
                          br(),
                          conditionalPanel(condition = "input.states_emer == true",
                                           strong(span(uiOutput("tab1_emer"), style = "font-size: 12px"))),
                          conditionalPanel(condition = "input.states_stay == true",
                                           strong(span(uiOutput("tab1_stay"), style = "font-size: 12px")))),
                 
                 tabPanel("State Comparisons", value = 2,
                          br(),
                          strong(span(textOutput("tab2_date"), style = "text-decoration: underline")),
                          br(),
                          conditionalPanel(condition = "input.table_choice != 6",
                                           textOutput("tab2_us_data"),
                                           textOutput("tab2_state_data"),
                                           br(),
                                           tableOutput("tab2_mini_table"),
                                           #US and State/Territory Population
                                           conditionalPanel(condition = "input.table_choice == 3 || input.table_choice == 4",
                                                            textOutput("tab2_us_pop"),
                                                            textOutput("tab2_state_pop"))),
                          
                          conditionalPanel(condition = "input.table_choice == 6",
                                           DTOutput("tab2_full_table"))),
                  
                  tabPanel(uiOutput("tab3_title"), value = 3,
                           br(),
                           strong(span(textOutput("tab3_date"), style = "text-decoration: underline")),
                           br(),
                           textOutput("tab3_first_case"),
                           br(),
                           tableOutput("tab3_case_table"),
                           textOutput("tab3_first_death"),
                           br(),
                           tableOutput("tab3_death_table")),

                  tabPanel(uiOutput("tab4_title"), value = 4,
                           br(),
                           plotOutput("county_plot", height = 600, width = "100%"),
                           br(),
                           textOutput("tab4_exceptions")),
                  
                  tabPanel(uiOutput("tab5_title"), value = 5,
                           br(),
                           uiOutput("tab5_plot_or_not"),
                           br(),
                           uiOutput("tab5_exceptions")),
                 
                  tabPanel(uiOutput("tab6_title"), value = 6,
                           br(),
                           strong(span(textOutput("tab6_date"), style = "text-decoration: underline")),
                           br(),
                           uiOutput("tab6_info_or_not"),
                           br(),
                           conditionalPanel(condition = "input.table_choice == 6",
                                            DTOutput("tab6_full_table")),
                           br(),
                           uiOutput("tab6_exceptions")),
                 
                 tabPanel(uiOutput("tab7_title"), value = 7,
                          br(),
                          strong(span(textOutput("tab7_date"), style = "text-decoration: underline")),
                          br(),
                          uiOutput("tab7_info_or_not"),
                          br(),
                          textOutput("tab7_exceptions")),
                 
                 tabPanel("Testing Info", value = 8,
                           br(),
                           strong(span(textOutput("tab8_title"), style = "text-decoration: underline")),
                           br(),
                           textOutput("tab8out1"),
                           br(),
                           textOutput("tab8out2"),
                           br(),
                           textOutput("tab8out3")),
                  
                 navbarMenu("More",
                            tabPanel("General Information",
                                     strong(span(textOutput("tab9_title"), style = "text-decoration: underline")),
                                     br(),
                                     uiOutput("tab9_info")),
                            "------",
                            tabPanel("Sources",
                                     strong(span(textOutput("tab10_title"), style = "text-decoration: underline")),
                                     br(),
                                     uiOutput("tab10_links1"),
                                     br(),
                                     uiOutput("tab10_links2"),
                                     br(),
                                     uiOutput("tab10_links3"),
                                     br(),
                                     uiOutput("tab10_links4"),
                                     br(),
                                     uiOutput("tab10_links5")),
                            "------",
                            tabPanel("Related Links",
                                     strong(span(textOutput("tab11_links_title1"), style = "text-decoration: underline")),
                                     br(),
                                     uiOutput("tab11_links_title2"),
                                     br(),
                                     uiOutput("tab11_link1"),
                                     br(),
                                     uiOutput("tab11_link2"),
                                     br(),
                                     uiOutput("tab11_link3"),
                                     br(),
                                     uiOutput("tab11_link4"),
                                     br(),
                                     uiOutput("tab11_link5"),
                                     br(),
                                     uiOutput("tab11_link6"),
                                     br(),
                                     uiOutput("tab11_link7"),
                                     br(),
                                     uiOutput("tab11_link8")),
                            "------",
                            tabPanel("Email", 
                                     textOutput("tab12_email1"),
                                     br(),
                                     textOutput("tab12_email2")))))))



##############SERVER##############
server <- function(input, output, session) {
  
  output$main_panel <- renderUI(paste(input$state_i, "Data, Graphs, Maps, and More!"))

  
  
  ##############FUNCTIONS##############  
  
  #UpdateSliderInput for Date.
  observeEvent({
    confirmedValues()
    input$mytabs
    input$data_choice
    input$state_i
    input$county_i},
    {
      first_case <- (confirmedValues()[[1]])$date[1]
      first_death <- (confirmedValues()[[2]])$date[1]
      if (input$mytabs %in% c(1, 5)) {
        if (input$data_choice %in% c(1, 3, 5)) {
          updateSliderInput(session, "date_in",
                            value = c(first_case, as.Date(Sys.Date() - 2)))
        } else {
          updateSliderInput(session, "date_in",
                            value = c(first_death, as.Date(Sys.Date() - 2)))
        }
        
      } else if(input$mytabs %in% c(2, 3, 4, 6, 7)) {
        updateSliderInput(session, "date_in",
                          value = c(first_case, as.Date(Sys.Date() - 2)))
      }
    })
  
  
  
  #Outputs two, two column data frames containing the Date and all Cases or Date and all Deaths for a state or county.
  confirmedValues <- reactive({
    if (input$mytabs %in% 1:4) {
      currentGeo <- getState(states_Corona, input$state_i)
    } else {
      currentGeo <- getCounty(getState(counties_Corona, input$state_i), input$county_i)
    }
    list(currentGeo %>% filter(type == "Cases") %>% select(date, people),
         currentGeo %>% filter(type == "Deaths") %>% select(date, people)) 
  })
  
  
  
  #Helper function for Server.  Defines all colors.
  getColors <- reactive ({
    if (input$col_sch == "Blues") {
      c("#CFE8F3", "#A2D4EC", "#73BFE2", "#46ABDB", "#1696D2", "#12719E", "#0A4C6A", "#062635")
    } else if (input$col_sch == "Greens") {
      c("#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B")
    } else if (input$col_sch == "Heat") {
      c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026")
    } else if (input$col_sch == "Purples") {
      c("#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
    } else if (input$col_sch == "Reds") {
      c("#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D")
    } else {
      c("#FDE725FF", "#9FDA3AFF", "#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF", "#440154FF")
    }
  })
  
  
  
  #Updates regression line options depending on number of cases or date of cases.
  observeEvent({
    input$mytabs
    input$data_choice
    input$state_i
    input$county_i
    confirmedValues()
    },
    { #Removes regression line options if not Cases or Deaths.
      if ((input$mytabs %in% c(1,5)) & (input$data_choice %in% c(1, 2)) & (input$state_i != "American Samoa")) {
        temp_df <- confirmedValues()[[as.integer(input$data_choice)]] %>%
          filter(people > 0)
        temp_date <- temp_df$date[1]
        if (is.na(temp_date)) {
          updateSelectInput(session, "con_pts",
                            choices = regression_list[c(1, 2)])
        } else {
          #Not enough people for a regression.
          if (tail(confirmedValues()[[as.integer(input$data_choice)]]$people, 1) < 10) {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2)])
          #First Case or Death is after Sys.Date() - 17.
          } else if (temp_date > Sys.Date() - 17) {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2)])
          #First Case or Death is after July 15th.  
          } else if (temp_date > "2020-06-15") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 13)])
          #First Case or Death is after June 30th.  
          } else if (temp_date > "2020-06-30") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 12, 13)])
          #First Case or Death is after June 14th.
          } else if (temp_date > "2020-06-14") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 11:13)])
          #First Case or Death is after May 31st.
          } else if (temp_date > "2020-05-31") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 10:13)])
          #First Case or Death is after May 14th.
          } else if (temp_date > "2020-05-14") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 9:13)])
          #First Case or Death is after April 30th.
          } else if (temp_date > "2020-04-30") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 8:13)])
          #First Case or Death is after April 14th.
          } else if (temp_date > "2020-04-14") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 7:13)])
          #First Case or Death is after March 31st.
          } else if (temp_date > "2020-03-31") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 6:13)])
          #First Case or Death is after March 14th.
          } else if (temp_date > "2020-03-14") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 5:13)])
          #First Case or Death is after February 29th.   
          } else if (temp_date > "2020-02-29") {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[c(1, 2, 4:13)])
          #First Case or Death is before February 29th      
          } else {
            updateSelectInput(session, "con_pts",
                              choices = regression_list[1:13]) 
          } 
        }
      } else {
        updateSelectInput(session, "con_pts",
                          choices = regression_list[c(1, 2)])
      }
    })
  
  
  
  #Dataframe for State and County Graphs.
  graphDF <- reactive({
    #Confirmed Cases, Prevalence Rate, and Deaths/Cases are dated by first Case.
    if (input$data_choice %in% c(1, 3, 5)) {
      graphDF <- confirmedValues()[[1]]
    #Confirmed Deaths and Mortality Rate are dated by first Death.
    } else {
      graphDF <- confirmedValues()[[2]]
    }
    #All points are relevant for None and Connect Points.   
    if (input$con_pts %in% c(0, 1)) {
      graphDF
    #All points are relevant up to selected Regression Date.   
    } else {
      graphDF %>%
        filter(date <= (reg_dates[as.integer(input$con_pts) - 1]))
    }
    
  }) 
  
  
  
  #Regression Info for State Graph and County Graph.
  regressionOutput <- reactive({
    if (input$con_pts > 1  & input$data_choice %in% c(1, 2)) {
      cd_index <- as.integer(input$data_choice)
      p(paste("Using a logistic regression model from",
              format((confirmedValues()[[cd_index]])$date[1], "%B %d"),
              "to",
              format(reg_dates[as.integer(input$con_pts) - 1], "%B %d"),
              "projects",
              prettyNum(as.integer(logisticModel(graphDF() %>% mutate(date = as.integer(date)))$vals[[1]]), big.mark = ","),
              paste(cd_list[cd_index], ".", sep = "")),
        style = paste("color:", getColors()[7], sep = ""))
    }
  })
  
  
  
  #Update Subdivision name and list depending on State.
  observeEvent(input$state_i,
               { 
                 updateSelectInput(session,
                                 "county_i",
                                 paste(subdivision(input$state_i), ":"),
                                 choices = countyList(input$state_i))}
               )
  
  
  
  #Helper function for County Graph, Comparisons, and Growth Info.
  isCountyNull <- reactive({
    #Wait for county input to load.
    if (is.null(input$county_i)) {
    } else {
      #County could simply not exist in data.
      if ((input$county_i %in% unique((getState(counties_Corona, input$state_i))$county_name)) == FALSE) {
        TRUE
        #County could exist in data, but have only 0s.
      } else if (sum((getCounty(getState(counties_Corona, input$state_i), input$county_i))$people) == 0) {
        TRUE
        #County has at least one case.  
      } else {
        FALSE
      }
    }
  })
  
  
   
  ##############STATE GRAPH##############
  output$tab1_title <- renderUI({
    paste(division(input$state_i), "Graph")
  })

  emerInfo <- reactive({
    states_Emer <- emergency %>%
      filter(type == 1) %>%
      select(1, 5, 6, 8)
    
    getState(states_Emer, input$state_i)
  })
  
  stayInfo <- reactive({
    states_Stay <- emergency %>%
      filter(level == "state",
             type %in% c(2, 2.5)) %>%
      select(1, 5, 6, 7, 8)
    
    getState(states_Stay, input$state_i)
  })
  
  output$state_plot <- renderPlot({
    
    #Graph

    current_State <- getState(states_Data_Table,input$state_i) %>%
      filter(date >= input$date_in[1], date <= input$date_in[2])
    
    #Show Zeroes
    current_State <- showZeroes(current_State, input$show_hide, input$threshold, input$data_choice)
    
    g1 <- ggplot(current_State, aes(x = date, y = current_State[,(as.integer(input$data_choice) + 3)]))
    
    #Vertical Lines.
    g1 <- createVertical(g1, input$states_emer, emerInfo(), "e", getColors()[5])
    g1 <- createVertical(g1, input$states_stay, stayInfo(), "s", getColors()[5])
    
    g1 <- g1 + labs(title = paste("Coronavirus in", input$state_i),
                    y = y_labels[as.integer(input$data_choice)])
    
    #Plot Points and Connect Points.
    g1 <- pointsInfo(input$con_pts,
                     input$data_choice,
                     g1,
                     getColors(),
                     graphDF(),
                     input$date_in[1])

    #scale_x_date
    g1 <- xDate(g1, input$date_in[1], input$date_in[2], input$con_pts)
    
    #Theme Details
    g1 <- themeDetails(g1)      
  
    #For all graphs
    #Cases Resulting in Deaths should be a %. The rest can have the 
    #option of Linear or Logarithmic, and should have commas when 
    #applicable.
    g1 <- yScale(g1, input$data_choice, input$graph_scale)
    g1
    })
  
  output$tab1_regression <- renderUI({regressionOutput()})
  
  #Create Footnote with State, Date, Name, and Link.
  output$tab1_emer <- renderUI({
    
    p(createHTML(paste("...", input$state_i, "declared a"),
                       emerInfo()$type_name,
                       emerInfo()$link,
                       paste("on",
                             paste(format(emerInfo()$date, "%B %d"),
                                   ".", sep = ""))),
      style = paste("color:", getColors()[5], sep = ""))
  })
  
  #Create Footnote with State, Date, Name, and Link.
  output$tab1_stay <- renderUI({
    #Never issued.
    if (is.na(stayInfo()$date)) {
      p(paste("As of",
            paste(format(Sys.Date(), "%B %d"), ",", sep = ""),
            input$state_i,
            "has yet to implement a statewide stay at home order."),
        style = paste("color:", getColors()[5], sep = ""))
    #Advisories.  
    } else if (stayInfo()$type == 2.5) {
      p(createHTML(paste("---", input$state_i, "issued an advisory"),
                   stayInfo()$type_name,
                   stayInfo()$link,
                   paste("on",
                         paste(format(stayInfo()$date, "%B %d"),
                               ".", sep = ""))),
        style = paste("color:", getColors()[5], sep = ""))
      
    #Orders.  
    } else {
      p(createHTML(paste("---", input$state_i, "implemented"),
                         stayInfo()$type_name,
                         stayInfo()$link,
                         paste("on",
                               paste(format(stayInfo()$date, "%B %d"),
                                     ".", sep = ""))),
        style = paste("color:", getColors()[5], sep = ""))
    }
  })

  
    
  ##############STATE COMPARISONS##############
  output$tab2_date <- renderText(paste("As of", format(input$date_in[2], "%B %d")))
  
  # No confirmed cases or the three part - US data, State data, and Mini Table.
  tempStateData <- reactive({
    if (input$table_choice != 6 & input$state_i == "American Samoa") {
      c("American Samoa currently has no confirmed cases", "", NULL)
    } else {
      comparisonsData(us_Data_Table, states_Data_Table, "United States", input$state_i, "s", input$date_in[2], as.integer(input$table_choice))
    }
  })
  
  #For us_pop and state_pop.
  tempStatePop1 <- reactive({
    paste(input$state_i, " Population (",
          ifelse(input$state_i %in% c("American Samoa", "Guam",
                                      "Northern Mariana Islands",
                                      "Virgin Islands"),
                 2020, 2019), 
          " estimate) : ",
          prettyNum((getState(states_Population, input$state_i))$population,
                    big.mark = ","), sep = "")
  })

  output$tab2_us_data <- renderText(tempStateData()[[1]])
  output$tab2_state_data <- renderText(tempStateData()[[2]])
  output$tab2_mini_table <- renderTable({tempStateData()[3]})
  output$tab2_us_pop <- renderText(paste("US Population (2020 estimate) : ", prettyNum(331883986, big.mark = ",")))
  output$tab2_state_pop <- renderText(tempStatePop1())
  output$tab2_full_table <- renderDT(datatable(tempStateData(),
                                               rownames = FALSE,
                                               options = list(autoWidth = TRUE)) %>%
                                       formatCurrency(c("Population", "Cases", "Deaths"), currency = '',
                                                      interval = 3, mark = ',', digits = 0, before = FALSE))

  
  
  ##############STATE GROWTH INFO##############
  output$tab3_title <- renderUI({
    paste(division(input$state_i), "Growth Info")
  })
  
  output$tab3_date <- renderText(paste("As of", format(input$date_in[2], "%B %d")))
  
  state_Growth_DF <- reactive({
    getState(states_Corona, input$state_i) %>%
               filter(date <= input$date_in[2])
  })
  
  output$tab3_first_case <- renderText(paste("First case in",
                                             input$state_i,
                                             "reported on :",
                                             format((confirmedValues()[[1]] %>% filter(people > 0))$date[1], "%B %d")))
  
  output$tab3_case_table <- renderTable({
    buildGrowthDF(state_Growth_DF(), "Cases")
  })
  
  
  output$tab3_first_death <- renderText(paste("First death in",
                                              input$state_i,
                                              "reported on :",
                                              format((confirmedValues()[[2]] %>% filter(people > 0))$date[1], "%B %d")))    
  
  output$tab3_death_table <- renderTable({
    buildGrowthDF(state_Growth_DF(), "Deaths")
  })
  
    
    
  ##############STATE MAP##############
  output$tab4_title <- renderUI({
    paste(division(input$state_i), "Map")
  })
  
  output$county_plot <- renderPlot({
    
    #Unknown states for the date and type.
    unknown_County_Date_Search <- getType(getDate(counties_Corona_Unknown, input$date_in[2]), input$map_data)
    #State people and type for the date
    people_value <- getType(getState(getDate(states_Corona, input$date_in[2]), input$state_i), input$map_data) %>%
      select(people)
    #Unknown people and type for the date
    unknown_value <- getState(unknown_County_Date_Search, input$state_i) %>%
      select(people)
    
    #Displays the total number of Cases or Deaths for a state.  Will also display
    #The number of unknown county Cases or Deaths if they exist.
    temp_people <- prettyNum(people_value[1, 1], big.mark = ",")
    if (input$state_i %in% unique(unknown_County_Date_Search$state) && unknown_value >= input$threshold) {
      caption <- paste(paste("Unknown County Confirmed/Probable", input$map_data, ":", prettyNum(unknown_value[1, 1], big.mark = ",")),
                       paste("Total Confirmed/Probable", input$map_data, ":", temp_people),
                       sep = "\n\n")
    } else {
      caption <- paste("Total Confirmed/Probable", input$map_data, ":", temp_people)
    }
    
    #Map
    ##Chloropleth
    if (input$map_display == "Chloropleth") {  
      #State/DC outline and fill.
      if (input$state_i %in% c(state.name, "District of Columbia")) {
        
        blank_Mapping <- getState(counties_Mapping, input$state_i)
        current_State_Mapping_Date <- right_join(getDate(getType(getState(counties_Corona,
                                                                          input$state_i),
                                                                 input$map_data),
                                                         input$date_in[2]),
                                                 blank_Mapping,
                                                 by = "county_fips")
        #Territory?
      } else {
        
        blank_Mapping <- getState(territories_Mapping, input$state_i)
        current_State_Mapping_Date <- right_join(getDate(getType(getState(territories_Corona,
                                                                          input$state_i),
                                                                 input$map_data),
                                                         input$date_in[2]),
                                                 blank_Mapping,
                                                 by = "state_name")
      }
      #Distribute NYC number evenly between 5 counties.
      if  (input$state_i == "New York") {
        nyc_cd <- getType(getDate(new_York_City, input$date_in[2]), input$map_data)$people
        current_State_Mapping_Date[current_State_Mapping_Date$county_name.y %in% nyc_counties,]$people <- nyc_cd / 5
      }
      #Eliminate counties less than threshold.

 
      current_State_Mapping_Date <- showZeroes(current_State_Mapping_Date, input$show_hide, input$threshold, "map")

      g2 <- ggplot() + labs(title = paste(input$state_i, "as of", format(input$date_in[2], "%B %d")),
                            caption = caption)
      #Fill all counties with grey.
      g2 <- g2 + geom_polygon(blank_Mapping,
                              mapping = aes(x = long, y = lat, group = group),
                              fill = "lightgrey")
      #Fill counties with people values.
      g2 <- g2 + geom_polygon(current_State_Mapping_Date,
                              mapping = aes(x = long, y = lat, group = group, fill = people))
      #Fill Gradient.
      g2 <- g2 + scale_fill_gradientn(name = paste("Confirmed/Probable", input$map_data),
                                      colors = getColors(),
                                      labels = comma)
      #Outline all counties, after filling.
      g2 <- g2 + geom_polygon(blank_Mapping,
                              mapping = aes(x = long, y = lat, group = group),
                              fill = NA, color = "white", size = 0.25)
      
      g2 <- g2 + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + theme_map()
      #Deets
      g2 <- g2 + theme(plot.title = element_text(face = "bold", size = 24),
                       plot.caption = element_text(hjust = 0, size = 15),
                       legend.position = "bottom",
                       legend.title = element_text(face = "bold", size = 15),
                       legend.key.height = unit(1, "cm"),
                       legend.key.width = unit(1.5, "cm"),
                       legend.text = element_text(size = 12))
      
      

      
      #Bubble Plot
    } else {
      #State/DC
      if (input$state_i %in% c(state.name, "District of Columbia")) {
        
        #Set NYC number to NY County.
        if (input$state_i == "New York") {
          counties_Corona[counties_Corona$county_name == "New York City",]$county_fips <- 36061
        }
        
        blank_Mapping <- getState(counties_Mapping, input$state_i)
        current_State_Mapping_Date <- left_join(getDate(getType(getState(counties_Corona,
                                                                          input$state_i),
                                                                 input$map_data),
                                                         input$date_in[2]),
                                                 getState(counties_Lats_Longs, input$state_i),
                                                 by = "county_fips")                      
        #Territory?
      } else {
        
        blank_Mapping <- getState(territories_Mapping, input$state_i)
        current_State_Mapping_Date <- left_join(getDate(getType(getState(territories_Corona,
                                                                          input$state_i),
                                                                 input$map_data),
                                                         input$date_in[2]),
                                                 territories_Lats_Longs,
                                                 by = "state_name")
      }
      
      #Eliminate counties less than threshold.
      current_State_Mapping_Date <- showZeroes(current_State_Mapping_Date, input$show_hide, input$threshold, "map")
      print(getState(territories_Corona,
                     input$state_i))
      g2 <- ggplot(current_State_Mapping_Date,
                   aes(x = long, y = lat, size = people))
      g2 <- g2 + labs(title = paste(input$state_i, "as of", format(input$date_in[2], "%B %d")),
                      size = paste("Confirmed/Probable", input$map_data),
                      caption = caption)
      
      #Fill all counties with grey and outline, before dotting.
      g2 <- g2 + geom_polygon(blank_Mapping,
                              mapping = aes(x = long, y = lat, group = group),
                              fill = "lightgrey", color = "white", size = 0.25)

      g2 <- g2 + geom_point(color = getColors()[6], alpha = 0.4) + scale_size_area(breaks = getScale(current_State_Mapping_Date), labels = comma, max_size = 70)

      g2 <- g2 + coord_map(projection = "albers", lat0 = 39, lat1 = 45) + theme_map()

      g2 <- g2 + theme(plot.title = element_text(face = "bold", size = 24),
                       plot.caption = element_text(hjust = 0, size = 15),
                       legend.position = "right",
                       legend.title = element_text(hjust = 0.5, face = "bold", size = 15),
                       legend.text = element_text(size = 12),
                       legend.background = element_rect(fill="transparent"))
    }
    g2
  })
  
    output$tab4_exceptions <- renderText({
      if (input$map_display == "Chloropleth") {
        if (input$state_i == "Missouri") {
          missouri_exception
        } else if (input$state_i == "New York") {
          "Data for New York City has been divided evenly amongst Bronx, Kings, New York, Queens, and Richmond Counties."
        }
      } else {
        if (input$state_i == "Missouri") {
          missouri_exception
        } else if (input$state_i == "New York") {
          nyc_exception          
        }
      }
    })
    
    ##############COUNTY GRAPH##############
    output$tab5_title <- renderUI({
      paste(subdivision(input$state_i), "Graph")
    })
    
    observeEvent(input$state_i, {
      if (!(input$state_i %in% state.name)) {
        hideTab(inputId = "mytabs", target = '5')
      }
    })
    
    observeEvent(input$state_i, {
      if (input$state_i %in% state.name) {
        showTab(inputId = "mytabs", target = '5')  
      }
    })
    
    #Data Table for County Graph and County Comparisons.
    counties_Data_Table <- reactive({
      createMetrics(left_join(getState(counties_Population, input$state_i)[, c(1, 3)], 
                              getState(counties_Corona_Data, input$state_i)[, c(1, 2, 5, 6)],
                              by = "county_name"), input$state_i)
    })
    
    #Initially, the county name is not read.  Wait.  Then see if there are cases/deaths in the county.
    #If there are not, then text saying that is output, else, a graph is output.
    output$tab5_plot_or_not <- renderUI({
      #Wait for county input to load.  No county data or data = 0.
      if (isCountyNull()) {
        textOutput("tab5_null_text")
        #There is at least one case.  
      } else {
        observeEvent(input$county_i,
                     {updateSliderInput(session, "date_in",
                                        value = c((getCounty(counties_Data_Table(),input$county_i) %>%
                                                     select(date))[1,],
                                                  as.Date(Sys.Date() - 2)))
                     })
        conditionalPanel(condition = "true",
                         plotOutput("tab5_plot", height = 600),
                         conditionalPanel(condition = "input.data_choice == 1 || input.data_choice == 2",
                                          strong(span(uiOutput("tab5_regression"), style = "font-size: 12px"))))
      } 
    })
    
    #If no cases in county, say it so!
    output$tab5_null_text <- renderText(noCasesText(input$state_i, input$county_i))
   
   #Graph
    output$tab5_plot <- renderPlot({

      current_County <- getCounty(counties_Data_Table(), input$county_i) %>%
        filter(date >= input$date_in[1], date <= input$date_in[2])
      
      #Show Zeroes
      current_County <- showZeroes(current_County, input$show_hide, input$threshold, input$data_choice)
      g3 <- ggplot(current_County, aes(x = date, y = current_County[,(as.integer(input$data_choice) + 3)]))
      
      g3 <- g3 + labs(title = paste("Coronavirus in", paste(input$county_i, ",", sep = ""),
                                    state.abb[grep(input$state_i, state.name)]),
                      y = y_labels[as.integer(input$data_choice)])
      
      #Plot Points and Connect Points.
      g3 <- pointsInfo(input$con_pts,
                       input$data_choice,
                       g3,
                       getColors(),
                       graphDF(),
                       input$date_in[1])
      
      #scale_x_date
      g3 <- xDate(g3, input$date_in[1], input$date_in[2], input$con_pts)
      
      #Theme Details.
      g3 <- themeDetails(g3)
      
      #For all graphs
      #Cases Resulting in Deaths should be a %. The rest can have the 
      #option of Linear or Logarithmic, and should have commas when 
      #applicable
      g3 <- yScale(g3, input$data_choice, input$graph_scale)
      g3
    })
    
    output$tab5_regression <- renderUI({regressionOutput()})
    
    output$tab5_exceptions <- renderUI(countyExceptions(input$state_i, input$county_i))

    
    
    ##############COUNTY COMPARISONS##############
    output$tab6_title <- renderUI({
      paste(subdivision(input$state_i), "Comparisons")
    })
    
    observeEvent(input$state_i, {
      if (!(input$state_i %in% state.name)) {
        hideTab(inputId = "mytabs", target = '6')
      }
    })
    
    observeEvent(input$state_i, {
      if (input$state_i %in% state.name) {
        showTab(inputId = "mytabs", target = '6')  
      }
    })
    
    output$tab6_date <- renderText(paste("As of", format(input$date_in[2], "%B %d")))
    
    # No confirmed cases or the three part - US data, State data, and Mini Table.
    output$tab6_info_or_not <- renderUI({
      #Wait for county input to load.
      if (isCountyNull()) {
        textOutput("tab6_null_text")
        
      } else {
        conditionalPanel(condition = "true",
                         conditionalPanel(condition = "input.table_choice != 6",
                                          textOutput("tab6_state_data"),
                                          textOutput("tab6_county_data"),
                                          br(),
                                          tableOutput("tab6_mini_table")),
                         conditionalPanel(condition = "input.table_choice == 3 || input.table_choice == 4",
                                          textOutput("tab6_state_pop"),
                                          textOutput("tab6_county_pop")))
      }
    })
    
    output$tab6_null_text <- renderText(noCasesText(input$state_i, input$county_i))
    
    countyComparisons <- reactive({
      if ((input$table_choice == 6) || (isCountyNull() == FALSE)) {
        state_Data_Table <- getState(states_Data_Table, input$state_i) %>%
          rename(county_name = state_name)
        
        comparisonsData(state_Data_Table,
                        counties_Data_Table(),
                        input$state_i,
                        input$county_i, 
                        "c",
                        input$date_in[2],
                        as.integer(input$table_choice))
      }
    })
    
    output$tab6_state_data <- renderText(countyComparisons()[[1]])
    output$tab6_county_data <- renderText(countyComparisons()[[2]])
    output$tab6_mini_table <- renderTable({
      #if (!(is.null(comparisonsData()[3]))) {
      countyComparisons()[3]
      #}
    })
    
    #For state_pop and county_pop.
    tempStatePop2 <- reactive({
      paste(input$state_i, " Population (2019 estimate) : ",
            prettyNum((getCounty(getState(counties_Population, input$state_i), input$state_i))$population,
                      big.mark = ","), sep = "")
    })
    tempCountyPop <- reactive({
      paste(input$county_i, " Population (2019 estimate) : ",
            prettyNum((getCounty(getState(counties_Population, input$state_i), input$county_i))$population,
                      big.mark = ","), sep = "")
    })
    
    output$tab6_state_pop <- renderText(tempStatePop2())
    output$tab6_county_pop <- renderText(tempCountyPop())
    
    output$tab6_full_table <- renderDT({
      if (input$table_choice == 6) {
        datatable(countyComparisons(),
                  rownames = FALSE,
                  options = list(autoWidth = TRUE)) %>%
          formatCurrency(c("Population", "Cases", "Deaths"), currency = '',
                         interval = 3, mark = ',', digits = 0, before = FALSE)
      }
    })
    
    output$tab6_exceptions <- renderText({
      if (input$table_choice == 6) {
        stateExceptions(input$state_i)
        
      } else {
        countyExceptions(input$state_i, input$county_i)
      }
    })

    
    
    ##############COUNTY GROWTH INFO##############
    output$tab7_title <- renderUI({
      paste(subdivision(input$state_i), "Growth Info")
    })
    
    observeEvent(input$state_i, {
      if (!(input$state_i %in% state.name)) {
        hideTab(inputId = "mytabs", target = '7')
      }
    })
    
    observeEvent(input$state_i, {
      if (input$state_i %in% state.name) {
        showTab(inputId = "mytabs", target = '7')  
      }
    })
    
    output$tab7_date <- renderText(paste("As of", format(input$date_in[2], "%B %d")))
    
    output$tab7_info_or_not <- renderUI({
      #Wait for county input to load.
      if (isCountyNull()) {
        textOutput("tab7_null_text")
        
      } else {
        conditionalPanel(condition = "true",
                         textOutput("tab7_first_case"),
                         br(),
                         tableOutput("tab7_case_table"),
                         textOutput("tab7_first_death"),
                         br(),
                         tableOutput("tab7_death_table"))
      }
      
    })
    
    output$tab7_null_text <- renderText(noCasesText(input$state_i, input$county_i))
    
    countyGrowthDF <- reactive({
      getCounty(getState(counties_Corona, input$state_i), input$county_i) %>%
        filter(date <= input$date_in[2])
      })
    
    output$tab7_first_case <- renderText({
      paste("First case in",
            input$county_i,
            "reported on :",
            format((confirmedValues()[[1]] %>% filter(people > 0))$date[1], "%B %d"))
    })
    
    output$tab7_case_table <- renderTable(buildGrowthDF(countyGrowthDF(), "Cases"))
    
    output$tab7_first_death <- renderText({
      paste("First death in",
            input$county_i,
            "reported on :",
            format((confirmedValues()[[2]] %>% filter(people > 0))$date[1], "%B %d"))
    })    
    
    output$tab7_death_table <- renderTable(buildGrowthDF(countyGrowthDF(), "Deaths"))
  
    output$tab7_exceptions <- renderText(countyExceptions(input$state_i, input$county_i))
    
    
    
    ##############TESTING INFO##############
    output$tab8_title <- renderText(paste("Tab is under construction..."))
    
    
    
    ##############MORE MENU############## 
    output$tab10_title <- renderText("Sources")
    
    output$tab10_links1 <- renderUI({
      createHTML("USA, County, State, and Territory Data : ",
                 "Data from The New York Times, based on reports from state and local health agencies.",
                 "https://github.com/nytimes/covid-19-data",
                 ".")
      })
  
    output$tab10_links2 <- renderUI({
      createHTML("State, DC, and Puerto Rico Population : ",
                 "State Population Totals: 2010-2019",
                 "https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html",
                 ".")
      })
    
    output$tab10_links3 <- renderUI({
      createHTML("USA, American Samoa, Guam, Northern Mariana Islands, and Virgin Islands Population : ",
                 "Worldometer - real time world statistics",
                 "https://www.worldometers.info/world-population/population-by-country/",
                 ".")
      })
    
    output$tab10_links4 <- renderUI({
      createHTML("US Counties Population : ",
                 "County Population Totals: 2010-2019",
                 "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv",
                 ".")
    })
        
    output$tab10_links5 <- renderUI({
      createHTML("State Testing Data : ",
                 "The COVID Tracking Project",
                 "https://covidtracking.com/api/",
                 ".")
      })
    
    output$tab11_links_title1 <- renderText("Related Links")
    output$tab11_links_title2 <- renderText("Some other COVID-19 related pages that I found interesting, but did not use directly on my page.")
    
    output$tab11_link1 <- renderUI({
      createHTML("Probably the best page I have seen for worlwide COVID-19 information by country : ",
                 "COVID-19 Dashboard by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University (JHU)",
                 "https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6",
                 ".")
    })
  
    output$tab11_link2 <- renderUI({
      createHTML("Identifying populations at risk : ",
                 "COVID Community Vulnerability Map",
                 "https://covid19.jvion.com/",
                 ".")
      })
    
    output$tab11_link3 <- renderUI({
      createHTML("US Coronavirus Data : ",
                 "Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE",
                 "https://github.com/CSSEGISandData/COVID-19",
                 ".")
    })
    
    output$tab11_link4 <- renderUI({
      createHTML("An exploration of hospital bed availability. :",
                 "Understanding Hospital Bed Capacities Nationwide amid COVID-19",
                 "https://www.urban.org/policy-centers/health-policy-center/projects/understanding-hospital-bed-capacities-nationwide-amid-covid-19",
                 ".")
    })
    
    output$tab11_link5 <- renderUI({
      createHTML("A more interactive and professional COVID-19 Page (No maps tho. ;)) : ",
                 "COVID-19.direct",
                 "https://covid-19.direct/",
                 ".")
    })
    
    output$tab11_link6 <- renderUI({
      createHTML("A buddy of mine is on a team that created this page. :",
                 "COVID-19 Health Workforce Recruiting Dashboard",
                 "https://nudge4.org/wp-content/uploads/2020/04/COVID19.html",
                 ".")
    })
    
    output$tab11_link7 <- renderUI({
      createHTML("An international tracking of COVID-19.  A great resource. :",
                 "Genomic epidemiology of novel coronavirus - Global subsampling",
                 "https://nextstrain.org/ncov/global",
                 ".")
    })
    
    output$tab11_link8 <- renderUI({
      createHTML("A website looking at Rt values as an indicator of spread rate. :",
                 "Rt Covid-19",
                 "https://rt.live/",
                 ".")
    })
  
    output$tab12_email1 <- renderText({
      "Questions?  Comments?"
    })
  
    output$tab12_email2 <- renderText({
      "Email : rohan.lewis@gmail.com"
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

