# load packages & data ----
install.packages("pacman")
library(pacman)

p_load(dplyr, readr, ggplot2, magrittr)
p_load(caret, tidymodels, readr)

set.seed(0726)
# importing by-year tuition data
t11 = read.csv("tui_11.csv")
t12 = read.csv("tui_12.csv")
t13 = read.csv("tui_13.csv")
t14 = read.csv("tui_14.csv")
t15 = read.csv("tui_15.csv")
t16 = read.csv("tui_16.csv")


# combining to make overall tuition df
tui_clean <- as.data.frame(rbind(t11, t12, t13, t14, t15, t16))

#replacing missing values with the median of each variable using preProcess function

p_load(naniar)
gg_miss_var(tui_clean) # visual of the missing data 

tui_clean <- preProcess(
  x = tui_clean[,4:22],
  method = c("medianImpute")
) %>% predict(tui_clean)

colSums(is.na(tui_clean))
tui_clean %<>% na.omit() #remove rows with NA in the outcome (1120 out of 14772)

# create a reference table for school name to unit id match...
## need centered, scaled, numeric matrix for modeling.

tui_reference <- cbind.data.frame(tui_clean[,1:3], tui_clean[,23])
write.csv(tui_reference, file = "tuition_reference_table.csv") 


# preProcess to center and scale tui_clean[,3:22]

# must be matrix
tui_clean %<>% select(-a_institution)
tui_mat <- as.matrix(tui_clean, rownames.force = NA)


tui_stnd <- preProcess(
  # Do not process the outcome variable
  x = tui_mat[, 2:21],
  # Standardize using methods: 'center' and 'scale'
  method = c("center", "scale")
) # We have to pass the 'preProcess' object to 'predict' to get new data
tui_stnd %<>% predict(newdata = tui_mat)

# Save preProcessed matrix as csv ----

write.csv(tui_stnd, file = "tuition_stnd_mat.csv")
