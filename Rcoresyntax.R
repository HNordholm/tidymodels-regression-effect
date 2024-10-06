
 #Raw syntax for this model#
## Made by Hampus Nordholm ##
    ### 2024-10-26 ###
#### FEAT TIDYMODELS ####



#CORE LIBRARIES 

#Data analysis 
library(tidyverse)
library(correlationfunnel)
library(skimr)
library(janitor)

#Machine learning
library(tidymodels)

#Loading data -- 
electric_tbl <- read_csv("data.csv")

#Data exploration --

electric_tbl %>% glimpse()

electric_tbl %>% sample_n(20)

#Cleaning var names -- 
electric_tbl <- electric_tbl %>% clean_names()

#EXPLORATORY DATA ANALYSIS --

# Count n vehicles where distance = 0  --
electric_tbl %>% 
  filter(electric_range==0) %>% 
  count()

# Basic mean values for  electric range by EV type -- 

electric_tbl %>% 
  filter(electric_range!=0) %>% 
  group_by(e_v_type) %>% 
  summarise(avg_electric_range=mean(electric_range)) %>% 
  ggplot(aes(e_v_type,avg_electric_range,fill=e_v_type))+
  geom_col()+
  geom_text(aes(label=round(avg_electric_range,1)),
            vjust=-0.5)+
  labs(title="Avg range for respective EV type",
       x="Type",y="Miles")

#Histogram for respective group -- 
electric_tbl %>% 
  filter(electric_range!=0) %>% 
  ggplot(aes(electric_range,fill=e_v_type))+
  geom_histogram(alpha=0.7,bins=25)+
  labs(title="Range distribution for EV-type",
       x="Range(miles)")

#Correlation analysis -- 

electric_tbl %>% 
  filter(electric_range!=0) %>% 
  na.omit() %>% 
  binarize() %>% 
  correlate(electric_range__215_Inf) %>% 
  plot_correlation_funnel()

# Simple linear reg feature selection -- avg. electric range effect of EV-type**

ev_type_tbl <- electric_tbl %>%
  filter(electric_range!=0) %>% 
  select(electric_range,e_v_type) %>% 
  mutate(e_v_type=as.factor(e_v_type))

# Train / test split 
set.seed(123)
simple_lm_split <- initial_split(data=ev_type_tbl,prop=0.8)
lm_training <- training(simple_lm_split)
lm_testing <- testing(simple_lm_split)

#Regression recipe -- 
lm_model_rec  <- recipe(electric_range~e_v_type,data=lm_training)

# Linear model spec -- 
lm_model_spec<-linear_reg() %>% 
  set_engine("lm")

#Combind into workflow -- 

lm_wf <- workflow() %>% 
  add_recipe(lm_model_rec) %>% 
  add_model(lm_model_spec)

# Training linear model -- 

lm_model_fit <- fit(lm_wf,data=lm_training)

#Results -- 
lm_model_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()

#Model evaluation -- (Examined on testing data)
ev_predict <- predict(lm_model_fit,new_data=lm_testing)

#Combinding actual vs. predicted values -- 
actvspred_lm<- lm_testing %>% select(electric_range) %>% 
  bind_cols(ev_predict)

lm_evaluation <-metrics(data=actvspred_lm,truth=electric_range,estimate=.pred)

#Final linear reg. model metrics -- 
lm_evaluation