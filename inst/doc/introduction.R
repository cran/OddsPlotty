## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----inv_odds_plot------------------------------------------------------------
library(OddsPlotty)


## ----load_pack----------------------------------------------------------------
#install.packages("mlbench")
#install.packages("caret")
library(mlbench)
library(caret)
library(tibble)
library(ggplot2)
library(OddsPlotty)
library(e1071)
library(ggthemes)


## ----data---------------------------------------------------------------------
data("BreastCancer", package = "mlbench")
#Use complete cases of breast cancer
breast <- BreastCancer[complete.cases(BreastCancer), ] #Create a copy
breast <- breast[, -1]
head(breast, 10)
#Convert the class to a factor - Beningn (0) and Malignant (1)
breast$Class <- factor(breast$Class)
str(breast)


## ----data_loop----------------------------------------------------------------
for(i in 1:9) {
  breast[, i] <- as.numeric(as.character(breast[, i]))
}
#Loops through the first columns - 1 to 9 and changes them from factors to a numerical representation
str(breast)


## ----train_glm----------------------------------------------------------------
library(caret)
glm_model <- train(Class ~ .,
                   data = breast,
                   method = "glm",
                   family = "binomial")


## ----odds_plot, echo = FALSE--------------------------------------------------
plotty <- OddsPlotty::odds_plot(glm_model$finalModel,
                      title = "Odds Plot",
                      subtitle = "Showing odds of cancer based on various factors")


plotty$odds_plot

## ----oddstibble, echo = FALSE-------------------------------------------------
plotty$odds_data

## ----odds_plot_two, echo = FALSE----------------------------------------------
library(OddsPlotty)
library(ggthemes)
plotty <- OddsPlotty::odds_plot(glm_model$finalModel, 
                      title = "Odds Plot with ggthemes economist",
                      subtitle = "Showing odds of cancer based on various factors",
                      point_col = "#00f2ff",
                      error_bar_colour = "black",
                      point_size = .5,
                      error_bar_width = .8,
                      h_line_color = "red") 

plot <- plotty$odds_plot 
plot <- plot + ggthemes::theme_economist() + theme(legend.position = "NULL")

# Add odds ratios to labels by calling the data list element
# The round function is used to return 2 decimal place values
plot + geom_text(label=round(plotty$odds_plot$data$OR, digits=2), 
                             hjust=0.1, vjust=2)


## ----themes-------------------------------------------------------------------
library(OddsPlotty)
library(ggthemes)
plotty <- OddsPlotty::odds_plot(glm_model$finalModel, 
                      title = "Odds Plot with ggthemes Tufte Theme",
                      subtitle = "Showing odds of cancer based on various factors",
                      point_col = "#00f2ff",
                      error_bar_colour = "black",
                      point_size = .5,
                      error_bar_width = .8,
                      h_line_color = "red")

plotty$odds_plot + ggthemes::theme_tufte()


