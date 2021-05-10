library(rstudioapi) #For setting current working directory
library(mirtCAT) #For CAT simulations
library(parallel) #For making many simulations in parallel
library(beepr) #For sound signal when simulations are done
library(ggpubr) #For making plots
source("Functions/plotSave.R") #For saving ggplots

#Set proper current working directory
setwd(dirname(getActiveDocumentContext()$path))

#Prepare results datapath
resultsPath <- paste0(getwd(), "/Results")
plotsPath <- paste0(resultsPath, "/Plots/")

#------------
#PREPARE DATA
#------------

#Load responses and params
params <- read.csv("Data/params.csv", encoding = "UTF-8")
responsesDemo <- read.csv("Data/responsesDemo.csv", encoding = "UTF-8")

#Prepare params df (convert difficulty from classic IRT to mirt format)
paramsToMirt <- data.frame(a1 = params$a, d = params$b * params$a * (-1))

#Generate mirt object from item parameters
mirtObject <- generate.mirt_object(paramsToMirt, '2PL') 

#For computation in parallel (speeds up simulations)
cl <- makeCluster(detectCores())

#Get pure responses matrix
responses <- as.matrix(responsesDemo[,5:ncol(responsesDemo)])

#Prepare full thetas
fullThetasDf <- read.csv("Data/fullThetasDf.csv", encoding = "UTF-8")

#-------------------------------
#PREPARE START THETAS [OPTIONAL]
#-------------------------------

#Get full thetas aggregated by days or weeks or months (put proper below - days, weeks, months)
fullThetasAggr <- aggregate(fullTheta ~ months + gender , data = fullThetasDf, mean)
dateUnit <- colnames(fullThetasAggr)[1]

#Plot full theta vs. date unit
# title <- paste0("Relation between full theta and age in ", dateUnit)
# plot <- ggscatter(fullThetasAggr, y = "fullTheta", x = dateUnit,
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = dateUnit, ylab = "theta", title = title)
# print(plot)
# plotSave(plot, filename = paste0(plotsPath, "Theta vs. ", dateUnit, ".png"))

#Plot full theta vs. gender
# title <- "Relation between theta and gender"
# plot <- ggerrorplot(fullThetasAggr, y = "fullTheta", x = "gender",
#                     add = "mean", error.plot = "errorbar", xlab = "Gender", ylab = "Theta") +
#   scale_x_discrete(labels = c('Boy','Girl'))
# print(plot)
# plotSave(plot, filename = paste0(plotsPath, "Theta vs. gender", ".png"))

#Get initial theta for each row (subject) by age and gender
obtainStartThetas <- function(age, gender){
  return(fullThetasAggr[fullThetasAggr[[dateUnit]] == age & fullThetasAggr$gender == gender, "fullTheta"])
}
startThetas <- mapply(obtainStartThetas, fullThetasDemo[[dateUnit]], fullThetasDemo$Płeć)
startThetas <- as.matrix(startThetas)

#------------
#SIMULATE CAT 
#------------

method <- "EAP"
criteria <- "MI"
startItem <- "MI"
folderSufix <- paste(method, criteria, startItem)

# 2. Fixed number of items
itemsNr <- 50

# 2.1. No start theta given
titleSufix <- paste0("no start theta given (", folderSufix, ")")
folder <- paste0("No start theta ", folderSufix)
design <- list(min_items = itemsNr, max_items = itemsNr)
results <- mirtCAT(mo = mirtObject, method = method, criteria = criteria, start_item = startItem, local_pattern = responses, cl = cl, design = design)

# 2.2. Initial thetas estimates given
# titleSufix <- paste0("start theta given by gender and age in ", tolower(xlab))
# folder <- paste0("Start theta by ", tolower(xlab))
# design <- list(min_items = itemsNr, max_items = itemsNr, thetas.start = as.vector(4))
# results <- mirtCAT(mo = mirtObject, method = 'ML', criteria = "MI", start_item = "MI", local_pattern = responses, cl = cl, design = design)

#Prepare simulation folder
if(!dir.exists(file.path(resultsPath, folder))) dir.create(file.path(resultsPath, folder))
simFolder <- paste0(resultsPath, "/", folder, "/")

#Prepare mean SE csv
meanSEfile <- paste0(simFolder, "meanSE.csv")
if (file.exists(meanSEfile)){
  meanSE <- read.csv(meanSEfile)
  if (!is.element(itemsNr, meanSE$items))   meanSE <- rbind(meanSE, c(itemsNr, 0))
} else {
  meanSE <- data.frame(items = itemsNr, meanSE = 0)
}

beep(sound=8)