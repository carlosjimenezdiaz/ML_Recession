# Libraries that we need
suppressPackageStartupMessages({
  if (!require("Quandl")) install.packages("Quandl"); library(Quandl)
  if (!require("forecast")) install.packages("forecast"); library(forecast)
  if (!require("e1071")) install.packages("e1071"); library(e1071)
  if (!require("cowplot")) install.packages("cowplot"); library(cowplot)
  if (!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer)
  if (!require("tseries")) install.packages("tseries"); library(tseries)
  if (!require("tidyquant")) install.packages("tidyquant"); library(tidyquant)
  if (!require("tidyverse")) install.packages("tidyverse"); library(tidyverse)
  if (!require("rpart")) install.packages("rpart"); library(rpart)
})

# Local Variables
symbols           <- c("ISM/MAN_PMI", "FRED/GS10", "FRED/GS2", "FRED/UNRATE", "FRED/USREC") # Macro Data from Quandl
nPeriods_Forecast <- 24 # How many months into the future you want to go
Quandl_API        <- "XXXXXXXXXXXXXXXXXXXXXXX" # Put your Quandl API
laging_Periods    <- 3 # How many months you want to consider for training the model

# Feeling (what do you think will happen with the values in the future)
Slope_Feeling     <- "Lower"
PMI_Feeling       <- "Lower"
UnEmpRate_Feeling <- "Higher"

# Activating Quandl Connection
Quandl::Quandl.api_key(Quandl_API)

# Creating the DB with all the Macro Variables
data.Set <- symbols %>%
  tq_get(get      = "quandl",
         from     = Sys.Date() - lubridate::years(100), # As much historical data as possible
         to       = Sys.Date(),
         collapse = "monthly") %>%
  dplyr::mutate(Final_Value = ifelse(is.na(pmi), value, pmi)) %>% # Dont know why with this tickers Quandl return an inconsistent DB.. so, this will fix that
  dplyr::select(symbol, date, Final_Value) %>%
  spread(symbol, Final_Value) %>%
  na.omit() %>%
  purrr::set_names(c("Date", "Yield_10Y", "Yield_2Y", "UnEmpRate", "CRISIS", "PMI")) %>%  # Crisis will be our label variable... so, let rearrange the DF
  dplyr::select("Date",  "Yield_10Y", "Yield_2Y", "UnEmpRate", "PMI", "CRISIS") %>%
  dplyr::mutate(CRISIS = dplyr::lead(CRISIS, n = laging_Periods)) %>%
  na.omit()

# Creating the Forecasted Dates
Forecasted_Dates <- seq((data.Set$Date %>% tail(n= 1)) + 30, by = "month", length.out = nPeriods_Forecast)

# Forecasting PMI
PMI_Forecasted <- forecast::forecast(tbats(data.Set %>% 
                                             dplyr::select(Date, PMI) %>% 
                                             column_to_rownames(var = "Date") %>% 
                                             as.ts()), h = nPeriods_Forecast)

if(PMI_Feeling == "Lower"){
  PMI_Forecasted_Final <- PMI_Forecasted %>%
    pluck("lower") %>%
    as.data.frame() %>%
    purrr::set_names(c("Low", "High")) %>%
    dplyr::mutate(PMI = (Low + High)/2,
                  Date  = Forecasted_Dates %>% as.Date()) %>%  # A naive forecast... taking the mean of both values
    dplyr::select(Date, PMI)
}else{
  PMI_Forecasted_Final <- PMI_Forecasted %>%
    pluck("upper") %>%
    as.data.frame() %>%
    purrr::set_names(c("Low", "High")) %>%
    dplyr::mutate(PMI = (Low + High)/2,
                  Date  = Forecasted_Dates %>% as.Date()) %>%  # A naive forecast... taking the mean of both values
    dplyr::select(Date, PMI)
}

# Forecasting Yield Curve Slope
Slope_Forecasted <- forecast::forecast(tbats(data.Set %>% 
                                               dplyr::select(Date, Yield_10Y, Yield_2Y) %>% 
                                               dplyr::mutate(Slope = Yield_10Y - Yield_2Y) %>%
                                               dplyr::select(Date, Slope) %>%
                                               column_to_rownames(var = "Date") %>% 
                                               as.ts()), h = nPeriods_Forecast)

if(Slope_Feeling == "Lower"){
  Slope_Forecasted_Final <- Slope_Forecasted %>%
    pluck("lower") %>%
    as.data.frame() %>%
    purrr::set_names(c("Low", "High")) %>%
    dplyr::mutate(Slope = (Low + High)/2,
                  Date  = Forecasted_Dates %>% as.Date()) %>%  # A naive forecast... taking the mean of both values
    dplyr::select(Date, Slope)
}else{
  Slope_Forecasted_Final <- Slope_Forecasted %>%
    pluck("upper") %>%
    as.data.frame() %>%
    purrr::set_names(c("Low", "High")) %>%
    dplyr::mutate(Slope = (Low + High)/2,
                  Date  = Forecasted_Dates %>% as.Date()) %>%  # A naive forecast... taking the mean of both values
    dplyr::select(Date, Slope)
}

# Forecasting Unemployment
UnEmpRate_Forecasted <- forecast::forecast(tbats(data.Set %>% 
                                                   dplyr::select(Date, UnEmpRate) %>% 
                                                   column_to_rownames(var = "Date") %>% 
                                                   as.ts()), h = nPeriods_Forecast)

if(UnEmpRate_Feeling == "Lower"){
  UnEmpRate_Forecasted_Final <- UnEmpRate_Forecasted %>%
    pluck("lower") %>%
    as.data.frame() %>%
    purrr::set_names(c("Low", "High")) %>%
    dplyr::mutate(UnEmpRate = (Low + High)/2,
                  Date      = Forecasted_Dates %>% as.Date()) %>%  # A naive forecast... taking the mean of both values
    dplyr::select(Date, UnEmpRate)
}else{
  UnEmpRate_Forecasted_Final <- UnEmpRate_Forecasted %>%
    pluck("upper") %>%
    as.data.frame() %>%
    purrr::set_names(c("Low", "High")) %>%
    dplyr::mutate(UnEmpRate = (Low + High)/2,
                  Date      = Forecasted_Dates %>% as.Date()) %>%  # A naive forecast... taking the mean of both values
    dplyr::select(Date, UnEmpRate)
}

# Let's see graphically our forecasts
recessions.df = read.table(textConnection(
  "Peak, Trough
  1973-11-01, 1975-03-01
  1980-01-01, 1980-07-01
  1981-07-01, 1982-11-01
  1990-07-01, 1991-03-01
  2001-03-01, 2001-11-01
  2007-12-01, 2009-06-01
  2020-02-01, 2020-03-30"), sep=',',
  colClasses=c('Date', 'Date'), header=TRUE)

forecast.df <- data.frame(Trough = Forecasted_Dates %>% as.Date()) %>%
  dplyr::mutate(Peak = dplyr::lag(Trough)) %>%
  replace(is.na(.), data.Set$Date %>% tail(n= 1)) %>%
  dplyr::select("Peak", "Trough")

db_PMI <- data.Set %>% 
  dplyr::select(Date, PMI) %>%
  rbind(PMI_Forecasted_Final)

db_Slope <- data.Set %>% 
  dplyr::select(Date, Yield_10Y, Yield_2Y) %>% 
  dplyr::mutate(Slope = Yield_10Y - Yield_2Y) %>%
  dplyr::select(Date, Slope) %>%
  rbind(Slope_Forecasted_Final)

db_UNEMPL <- data.Set %>% 
  dplyr::select(Date, UnEmpRate) %>%
  rbind(UnEmpRate_Forecasted_Final)

g.emp <- ggplot(data = db_UNEMPL, aes(x = Date, y = UnEmpRate/100))+
  geom_line(color = "royalblue") +
  geom_rect(data = recessions.df, inherit.aes = FALSE,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill = 'lightblue', alpha=0.5) +
  geom_rect(data = forecast.df, inherit.aes=FALSE,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill='yellow', alpha=0.5) +
  theme_minimal() +
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic",size=9),
        plot.title=element_text(face="bold",size=14)) +
  labs(x = "",
       y = "",
       title = "Unemployment",
       subtitle = "Evolution of the unemployment in USA") + 
  scale_y_continuous(labels = scales::percent)

g.pmi<- ggplot(data = db_PMI, aes(x = Date, y = PMI))+
  geom_line(color = "royalblue") +
  geom_hline(yintercept = 50, col = "red") +
  geom_rect(data = recessions.df, inherit.aes = FALSE,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill = 'lightblue', alpha=0.5) +
  geom_rect(data = forecast.df, inherit.aes=FALSE,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill='yellow', alpha=0.5) +
  theme_minimal() +
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic",size=9),
        plot.title=element_text(face="bold",size=14)) +
  labs(x = "",
       y = "",
       title    = "PMI",
       subtitle = "Purchasing Managers' Index (PMI)")

g.slope<- ggplot(data = db_Slope, aes(x = Date,y = Slope))+
  geom_rect(data=recessions.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='lightblue', alpha=0.5) +
  geom_rect(data=forecast.df, inherit.aes=FALSE,
            aes(xmin=Peak, xmax=Trough, ymin=-Inf, ymax=+Inf), 
            fill='yellow', alpha=0.5) +
  theme_minimal() +
  geom_line(color="royalblue") +
  labs(x="",y="",title="Yield curve slope",
       subtitle="10-year minus 2-year U.S. Treasury rates") +
  geom_hline(yintercept=0,color="black") +
  theme(plot.caption=element_text(hjust=0),
        plot.subtitle=element_text(face="italic",size=9),
        plot.title=element_text(face="bold",size=14))

plot_grid(g.slope,g.pmi,g.emp, nrow=3, ncol = 1)

# Training and Test Set
Training.Set <- data.Set %>% # We will traing the model with the total dataset and we will test it with the forecast (so we can create the indicator)
  dplyr::select(-Date) %>%  # Dont need the date for training the model
  dplyr::mutate(Slope = Yield_10Y - Yield_2Y) %>%
  dplyr::select("PMI", "Slope", "UnEmpRate", "CRISIS")

Test.Set <- data.frame(PMI_Forecasted_Final$PMI,
                       Slope_Forecasted_Final$Slope,
                       UnEmpRate_Forecasted_Final$UnEmpRate) %>%
  purrr::set_names(c("PMI", "Slope", "UnEmpRate"))

# Training a Logistic Regression Model
# Creating the Training and testing
Training_set_LG <- Training.Set %>%
  dplyr::mutate(PMI       = scale(PMI),
                Slope     = scale(Slope),
                UnEmpRate = scale(UnEmpRate))

Test_set_LG <- Test.Set %>%
  dplyr::mutate(PMI       = scale(PMI),
                Slope     = scale(Slope),
                UnEmpRate = scale(UnEmpRate))

# training the model
classifier_glm <- glm(formula = CRISIS ~ ., family = binomial, data = Training_set_LG)

# Getting the fitted values and combining those with the predictions to create the Crisis Indicator 
prob      <- classifier_glm[["fitted.values"]]
prob_pred <- predict(classifier_glm, type = 'response', newdata = Test_set_LG) # Prediction

# Crisis Indicator Logistic Regression
total_Predictions_LG <- data.frame(Predictions  = c(prob, prob_pred),
                                   `Model Type` = "Logistic Regression",
                                   Dates        = c(data.Set$Date, Forecasted_Dates) %>% as.Date())

# Training a Decision Tree Model
# Creating the Training and testing (No scaling is need it)
Training_set_DT <- Training.Set
Test_set_DT     <- Test.Set

# training the model
classifier_DT <- rpart(formula = CRISIS ~ ., data = Training_set_DT)

# Getting the fitted values and combining those with the predictions to create the Crisis Indicator 
prob      <- predict(classifier_DT, Training_set_DT[,-ncol(Training_set_DT)]) # Historical Fitted Values
prob_pred <- predict(classifier_DT, newdata = Test_set_DT) # Prediction

# Crisis Indicator Decision Tree
total_Predictions_DT <- data.frame(Predictions  = c(prob, prob_pred),
                                   `Model Type` = "Decision Tree",
                                   Dates        = c(data.Set$Date, Forecasted_Dates) %>% as.Date())


# Training a GBM Model
# Creating the Training and testing (No scaling is need it)
Training_set_GBM <- Training.Set
Test_set_GBM     <- Test.Set

# training the model
classifier_GBM <- gbm(CRISIS ~ ., data = Training_set_GBM, distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)

# Getting the fitted values and combining those with the predictions to create the Crisis Indicator 
prob      <- predict(classifier_GBM, newdata = Training_set_GBM[,-ncol(Training_set_GBM)], n.trees = 1) # Historical Fitted Values
prob_pred <- predict(classifier_GBM, newdata = Test_set_GBM) # Prediction

# Crisis Indicator GBM
# Normalization of the output (between 0 and 100)
fitted_values <- data.frame(fitted_values = prob) %>%
  dplyr::mutate(Normalization = (fitted_values - min(fitted_values))/(max(fitted_values) - min(fitted_values))) %>%
  dplyr::select(Normalization) %>%
  pull(1)

Predictions <- data.frame(fitted_values = prob_pred) %>%
  dplyr::mutate(Normalization = (fitted_values - min(fitted_values))/(max(fitted_values) - min(fitted_values))) %>%
  dplyr::select(Normalization) %>%
  pull(1)

total_Predictions_GBM <- data.frame(Predictions  = c(fitted_values, Predictions), 
                                    `Model Type` = "GBM",
                                    Dates        = c(data.Set$Date, Forecasted_Dates) %>% as.Date())

# Creating the chart
total_Predictions_LG %>%
  rbind(total_Predictions_DT) %>%
  rbind(total_Predictions_GBM) %>%
  purrr::set_names(c("RecessionOdds", "Model Type", "Dates")) %>%
  ggplot(aes(x = Dates, y = RecessionOdds, colour = `Model Type`)) + 
  geom_line(size = 0.9) +
  geom_rect(data = recessions.df, inherit.aes = FALSE,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill = 'lightblue', alpha = 0.4) +
  theme_minimal() +
  labs(x = "", y = "", title = "Probability of a Recession in the United States",
       subtitle = str_glue("Decision Tree, Logistic Regression and Gradient Boosted Modeling - Forecast: {nPeriods_Forecast} Months"),
       caption = "The variables analyzed in this study are: PMI, the Slope of the Yield Curve and the Unemployment Rate") +
  geom_hline(yintercept = 0,color = "black") +
  theme(plot.caption = element_text(hjust = 0),
        plot.subtitle = element_text(face = "italic",size = 9),
        plot.title = element_text(face = "bold", size = 14)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::percent)

total_Predictions_LG %>%
  rbind(total_Predictions_DT) %>%
  rbind(total_Predictions_GBM) %>%
  purrr::set_names(c("RecessionOdds", "Model Type", "Dates")) %>%
  dplyr::group_by(Dates) %>%
  dplyr::summarise(Assemble = mean(RecessionOdds)) %>%
  dplyr::mutate(Assemble = (Assemble - min(Assemble))/(max(Assemble) - min(Assemble))) %>%
  ggplot(aes(x = Dates, y = Assemble)) + 
  geom_line(size = 0.9) +
  geom_rect(data = recessions.df, inherit.aes = FALSE,
            aes(xmin = Peak, xmax = Trough, ymin = -Inf, ymax = +Inf), 
            fill = 'lightblue', alpha = 0.4) +
  theme_minimal() +
  labs(x = "", y = "", title = "Probability of a Recession in the United States",
       subtitle = str_glue("Assemble Model - Forecast: {nPeriods_Forecast} Months"),
       caption = "The variables analyzed in this study are: PMI, the Slope of the Yield Curve and the Unemployment Rate") +
  geom_hline(yintercept = 0,color = "black") +
  theme(plot.caption = element_text(hjust = 0),
        plot.subtitle = element_text(face = "italic",size = 9),
        plot.title = element_text(face = "bold", size = 14)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::percent)