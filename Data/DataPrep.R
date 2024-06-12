################################################################################
# Filename: DataPrep.R

################################################################################

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

################################################################################