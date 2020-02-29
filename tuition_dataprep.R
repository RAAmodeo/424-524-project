library(dplyr, readr)

# importing by-year tuition data
t11 = read_csv("C:/My_R/524.proj/tui_11.csv")
t12 = read.csv("C:/My_R/524.proj/tui_12.csv")
t13 = read.csv("C:/My_R/524.proj/tui_13.csv")
t14 = read.csv("C:/My_R/524.proj/tui_14.csv")
t15 = read.csv("C:/My_R/524.proj/tui_15.csv")
t16 = read.csv("C:/My_R/524.proj/tui_16.csv")


# combining to make overall tuition df
tui_clean <- as.data.frame(rbind(t11, t12, t13, t14, t15, t16))

write.csv(tui_clean, file = "C:/My_R/524.proj/tuition_clean.csv")               
