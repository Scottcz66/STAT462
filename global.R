library(shiny)

library(vcd)
library(bslib)
library(shinyWidgets)
library(plotly)
library(ggplot2)
library(GGally)
library(corrgram)
library(visdat)
library(DT)
library(car)
library(plotly)

library(devtools)
#install_github("edwindj/ffbase", subdir="pkg") 
library(ffbase) 
#install_github("mtennekes/tabplot")

ff_dir <- file.path(getwd(), "ff_temp")

if (!dir.exists(ff_dir)) {
  dir.create(ff_dir, recursive = TRUE)
}

options(fftempdir = ff_dir)


library(tabplot)

data <- read.csv("Ass1Data.csv", header = TRUE, stringsAsFactors = TRUE)

#eg. 2018-01-12
data$Date <- as.Date(data$Date)

if ("Date" %in% names(data)) data$Date <- as.Date(data$Date)

num_vars <- names(data)[sapply(data, is.numeric)]
cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

#oder factors
data$Priority <- ordered(data$Priority, levels = c("Low", "Medium", "High"))
data$Price    <- ordered(data$Price,    levels = c("Cheap", "Fair", "Expensive"))
data$Speed    <- ordered(data$Speed,    levels = c("Slow", "Medium", "Fast"))
data$Duration <- ordered(data$Duration, levels = c("Short", "Long", "Very Long"))
data$Temp     <- ordered(data$Temp,     levels = c("Cold", "Warm", "Hot"))
data$Agreed   <- factor(data$Agreed, levels = c("No", "Yes"))

num_vars <- names(data)[sapply(data, is.numeric)]
cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
allVars  <- colnames(data)
factVars <- colnames(data)[sapply(data, is.factor)]
numVars  <- colnames(data)[sapply(data, is.numeric)]

id_cols <- intersect(allVars, c("ID", "Id", "id"))
ggpairsVars <- setdiff(allVars, c(id_cols, "Date"))

date_min <- min(data$Date, na.rm = TRUE)
date_max <- max(data$Date, na.rm = TRUE)

tabplot_default_vars <- c("Y", "Priority", "Price", "Speed", "Duration",
                          "sensor1", "sensor2", "sensor3", "sensor4")
tabplot_default_vars <- intersect(tabplot_default_vars, names(data))