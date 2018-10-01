# Looking at Minneapolis Weather data

source('~/R/R_startup.R')

# Load data
# file <- 'C:\\Users\\z001c9v\\Documents\\Data\\Weather\\Minneapolis Weather.xlsx'
file <- 'Minneapolis Weather.xlsx'

library(readxl)
library(readr)
library(stringr)
library(tidyverse)
library(dplyr)
library(ggplot2)

base_data <- read_csv('Minneapolis_Weather_percip_in_inches.csv')
months <- c("Jan","Feb","Mar", "Apr","May","Jun",
			"Jul","Aug","Sep", "Oct","Nov","Dec")

base_data$Precip <- as.numeric( ifelse(base_data$Precip == 'T' , 0, base_data$Precip) )
base_data$SnowFall <- as.numeric( ifelse(base_data$SnowFall == 'T' , 0, base_data$SnowFall) )
base_data$Snowdepth <- as.numeric( ifelse(base_data$Snowdepth == 'T' , 0, base_data$Snowdepth))
base_data$yr_mo <- paste0(base_data$Year, '_' , base_data$Month)
base_data$yr_mo_dd <- paste0(base_data$Year,'_',base_data$Month , '_', str_pad(base_data$Day, 2, side = 'left' , pad = "0") )

Mthly_avg <- base_data %>%
	#select(Year, Month, yr_mo, High, Low, Precip, SnowFall, Snowdepth) %>%
	group_by(Year, Month, yr_mo) %>%
	mutate(avg_High = mean(High), 
		   avg_Low = mean(Low),
		   avg_Precip = mean(Precip),
		   avg_Snowfall = mean(SnowFall),
		   avg_Snowdepth = mean(Snowdepth)) %>%
	select(Year,
		   Month, 
		   yr_mo, 
		   avg_High, 
		   avg_Low, 
		   avg_Precip, 
		   avg_Snowfall , 
		   avg_Snowdepth) %>%
	distinct()


# Graph data
# Average Snow Depth by Month
Mthly_avg %>% ggplot(aes(x = as.factor(Month), 
						 y = avg_Snowdepth, 
						 group = Year, 
						 col = as.factor(Year))) +
	geom_line() + scale_x_discrete(labels = months) +
	labs(title = "Average Snow Depth by Month",
		 x = "Month",
		 y = "Average Snow Depth (inches)",
		 col = "Year")

# Average Snowfall by Month
Mthly_avg %>% ggplot(aes(x = as.factor(Month), 
						 y = avg_Snowfall, 
						 group = Year, 
						 col = as.factor(Year))) +
	geom_line() + scale_x_discrete(labels = months) +
	labs(title = "Average Snowfall by Month",
		 x = "Month",
		 y = "Average Snowfall (inches)",
		 col = "Year")

# Average Precipitation by Month
Mthly_avg %>% ggplot(aes(x = as.factor(Month), 
						 y = avg_Precip, 
						 group = Year, 
						 col = as.factor(Year))) +
	geom_line() + scale_x_discrete(labels = months) +
	labs(title = "Average Precipitation by Month",
		 x = "Month",
		 y = "Average Precipitation (inches)",
		 col = "Year")