################################################################################
# Filename: DataPrep.R

################################################################################

#full_shoot <- read.csv("C:/Users/cal14e/Git/RemovalEstimation/Data/full_shoot_data.csv")
full_shoot <- read.csv("./Data/full_shoot_data.csv", header=TRUE)
grouped_shoot <- read.csv("./Data/grouped_shoot_data.csv", header=TRUE)


# Plot all data
par(mfrow=c(1,1))
with(full_shoot, {
  plot(Dhat, kills_hour, xlim=c(0, 40), ylim=c(0,100), pch=16, col=year)
})


# Generate list 
shoot_lst <- split(full_shoot, list(full_shoot$site))


par(mfrow=c(3,3))
lapply(shoot_lst,
       function(x) {
         with(x,{
          plot(Dhat, kills_hour, xlim=c(0, 40), ylim=c(0,100), pch=16, col=year)
          title(unique(x$site)) 
         })
           }
       )


test <- shoot_lst[["ACT"]]


################################################################################
# Pasted 
library(tidyverse)
# Full data set, one row for each day's shooting
datFull <- read.csv("./Data/full_shoot_data.csv", header=T)
datFull$key <- gsub(" ", "_", datFull$key)

datFull <- datFull %>% mutate(key=factor(key))

# Grouped by operation
datFullGp <- read.csv("./Data/grouped_shoot_data.csv", header=T) 

# Replace space in field "key" with underbar
datFullGp$key <- gsub(" ", "_", datFullGp$key)

# add se for dhat
dhat_se <- data.frame(key = levels(factor(datFull$key))) %>%
  mutate(se_dhat = c(1.68, 5.68, 2.17, 5.13, 0.22, 11.34, 0.91, 3.35, 2.88, 2.01, 15.23, 10.58))

datFullGp <- datFullGp %>%
  left_join(dhat_se)

################################################################################
# Put together data matrices

dhat <- datFullGp$dhat
names(dhat) <- datFullGp$key

se_dhat <- datFullGp$se_dhat
names(se_dhat) <- datFullGp$key
