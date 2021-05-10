library(rstudioapi)#For setting current working directory
library(mirt) #For creation of model
library(ggpubr) #For making plots
source("Functions/plotSave.R") #For saving ggplots
options(max.print=999999)

#Set proper current working directory
setwd(dirname(getActiveDocumentContext()$path))

#If not done after 1DataPreparation load ready csvs:
responsesDemo <- read.csv("Data/responsesDemo.csv", encoding = "UTF-8") 
cdi <- read.csv("Data/cdi.csv", encoding = "UTF-8")

#--------------
#MODEL CREATION 
#--------------

#Create mirt model (it takes some time)
# library(beepr)
# responses <- as.matrix(responsesDemo[,5:ncol(responsesDemo)])
# model <- mirt(responses, 1)
# beep(sound=8)
# save(model, file = "Data/model")

#Load saved model
load("Data/model")

#Plot expected total score
plot(model)

#----------------
#ITEMS PARAMETERS
#----------------

#Get items parameters
# params <- as.data.frame(coef(model, IRTpars = TRUE, simplify = TRUE)$items)
# params$g <- NULL
# params$u <- NULL

#Plot difficulty vs. number of selections (to check if params makes sense)
# plot(params$b, colSums(responses), xlab = "Item difficulty", ylab = "Number of selections")

#Prepare params df with positions and categories
# params$position <- cdi$Pozycja
# params$category <- cdi$Kategoria
# params$number.ws <- cdi$Numer
# write.csv(params, file = "Data/params.csv", fileEncoding = "utf-8", row.names = F)

#Read already prepared items params
params <- read.csv("Data/params.csv", encoding = "UTF-8")

#Plot items params
#plot(params$b, params$a, xlab = "Difficulty", ylab = "Discrimination", main = "Items parameters")
ggplot(params, aes(b, a, label = position, colour = category)) +
  geom_text(check_overlap = TRUE) +
  xlab("Difficulty") +
  ylab("Discrimination") + 
  labs(colour = "Category", title = "Items parameters")

#-----------
#FULL THETAS
#-----------

#Speeds up computations
mirtCluster()

#Obtain full thetas
method <- "EAP"
fullThetas <- as.data.frame(fscores(model, method = method, full.scores.SE = TRUE))

#Prepare full thetas df
fullThetasDf <- responsesDemo[,1:4]
fullThetasDf$fullTheta <- fullThetas$F1
fullThetasDf$SE <- fullThetas$SE_F1
fullThetasDf$weeks <- ceiling(fullThetasDf$days/7)

#Set plots path
plotsPath <- "Results/Plots/"
if(!dir.exists(file.path(plotsPath, method))) dir.create(file.path(plotsPath, method))
plotsPath <- paste0(plotsPath, method, "/")

#Plot full thetas histogram
png(paste0(plotsPath, "Full thetas histogram.png"), width = 853, pointsize = 16)
hist(fullThetasDf$fullTheta, xlab = "Theta", main = paste0("Histogram of full thetas - ", method, " estimation"))
dev.off()

#Plot full theta vs. SE
title <- paste0("Full theta vs. SE - ", method, " estimation")
plot <- ggscatter(fullThetasDf, y = "SE", x = "fullTheta",
                  xlab = "Theta", ylab = "SE", title = title)
print(plot)
plotSave(plot, filename = paste0(plotsPath, "Full theta vs. SE.png"))

#Plot full thetas distribution in age groups
fullThetasAge <- data.frame(age = responsesDemo$months, fullTheta = fullThetasDf$fullTheta)
gghistogram(fullThetasAge, x = "fullTheta", bins = 10) + 
  facet_wrap(~age) + 
  labs(title = paste0("Full thetas distribution in particular age groups - ", method, " estimation")) + 
  xlab("Full theta") + 
  ylab("Count")

#Save full thetas to file
write.csv(fullThetasDf, file = "Data/fullThetasDf.csv", fileEncoding = "utf-8", row.names = F)

#---------
#ITEMS FIT 
#---------

#Speeds up computation
mirtCluster() 

#Check items fit (it could take some time)
itemsFit <- itemfit(model)

#Check number of misfits (p < 0.05) without correction - 35
nrow(itemsFit[itemsFit$p.S_X2 < 0.05, ])

#Adjust p-values with fdr correction (less conservative than bonferonni thus
#it is more probable that some misfit will occur)
itemsFit$p.S_X2_fdr <- p.adjust(itemsFit$p.S_X2, 'fdr')

#Check number of misfits after correction - no misfits even at p = 0.1
nrow(itemsFit[itemsFit$p.S_X2_fdr < 0.1, ])