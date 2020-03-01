library(pacman)
p_load(dplyr, readr, janitor, ggplot2, caret, naniar)
getwd()
# importing by-year tuition data
t11 = read_csv("tui_11.csv")
t12 = read.csv("tui_12.csv")
t13 = read.csv("tui_13.csv")
t14 = read.csv("tui_14.csv")
t15 = read.csv("tui_15.csv")
t16 = read.csv("tui_16.csv")


# combining to make overall tuition df
tui_clean <- as.data.frame(rbind(t11, t12, t13, t14, t15, t16))

write.csv(tui_clean, file =
            "C:/Users/Nina/Documents/College Classes/Winter 2020/Econ/424-524-project/tuition_clean1.csv")               

#checking for missing data then replacing missing values with the median of each variable usinf preProcess function

gg_miss_var(tui_clean) # visual of the missing data 

tui_clean1 <- preProcess(
  x = tui_clean,
  method = c("medianImpute")
) %>% predict(tui_clean)

summary(tui_clean1)
sum(is.na(tui_clean1))

# split data into test and training data  20% 80%

set.seed(0726)
tui_clean_test <- sample_frac(tui_clean1, size = 0.2)
tui_clean_train <- setdiff(tui_clean, tui_clean_test)


