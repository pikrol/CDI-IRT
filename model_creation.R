library(ltm) #For creation of model
library(mirt) #For creation of model
library(beepr) #For sound signal after model creation
library(rstudioapi)#For set of current working directory

#Set proper current working directory
setwd(dirname(getActiveDocumentContext()$path)) 

#If not done after 1DataPreparation load ready csvs & prepare responses matrix:
responses_demo <- read.csv("Data/responses_demo.csv", encoding = "UTF-8") 
cdi <- read.csv("Data/cdi.csv", encoding = "UTF-8")
responses <- as.matrix(responses_demo[,5:ncol(responses_demo)])

#----
#mirt
#----

#Create mirt model (it takes some time)
mirt_model <- mirt(responses, 1, itemtype = "2PL")
beep(sound=8)
save(mirt_model, file = "Data/mirt_model")

#Calculate items fit
mirtCluster() #Speeds up computation
mirt_items_fit <- itemfit(mirt_model) #Use default S_X2 statistic
beep(sound=8)
write.csv(mirt_items_fit, file = "Data/mirt_items_fit.csv", fileEncoding = "utf-8", row.names = F)

#Calculate items fit with Yen's (1981) Q1 variant of the X2 statistic (default ltm method)
mirtCluster() #Speeds up computation
mirt_items_fit_x2 <- itemfit(mirt_model, fit_stats = 'X2') #Use X2 statistic
write.csv(mirt_items_fit, file = "Data/mirt_items_fit_x2.csv", fileEncoding = "utf-8", row.names = F)

#---
#ltm
#---

#Create ltm model
ltm_model <- ltm(responses ~ z1)
beep(sound=8)
save(ltm_model, file = "Data/ltm_model")

#Calculate items fit
ltm_items_fit <- item.fit(ltm_model)
ltm_items_fit <- cbind(ltm_items_fit$Tobs, ltm_items_fit$p.values)
colnames(ltm_items_fit) <- c("Tobs", "p.values")
ltm_items_fit <- as.data.frame(ltm_items_fit)
ltm_items_fit$item <- rownames(ltm_items_fit)
write.csv(ltm_items_fit, "Data/ltm_items_fit.csv", row.names = F)