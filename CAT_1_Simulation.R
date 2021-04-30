library(mirtCAT) #For CAT simulations
library(parallel) #For making many simulations in parallel
library(beepr) #For sound signal when simulations are done
library(ggpubr) #For making plots
source("Functions/plotSave.R") #For saving ggplots

#------------
#PREPARE DATA
#------------

#Load responses and params
params <- read.csv("Data/params.csv", encoding = "UTF-8")
responsesDemo <- read.csv("Data/responsesDemo.csv", encoding = "UTF-8")
folder0 <- "NotRestricted/"

#Adjust data for restricted set of items (remove not fitting items)
load("Data/notFitting")
params <- params[!is.element(params$Numer, notFitting), ]
responsesDemo <- responsesDemo[, -which(names(responsesDemo) %in% paste0("item", notFitting))]
folder0 <- "RestrictedP05"

#Prepare params df
params <- data.frame(a1 = params$a1, d = params$d)

#Generate mirt object from item parameters
mirtObject <- generate.mirt_object(params, '2PL') 

#For computation in parallel (speeds up any simulation)
cl <- makeCluster(detectCores())

#Get pure responses matrix
responses <- responsesDemo
responses <- responses[,5:ncol(responses)]
responses <- as.matrix(responses)

#Prepare full thetas
fullThetas <- read.csv("Data/fullThetas.csv", encoding = "UTF-8")

#Prepare full thetas with demographic variables
fullThetasDemo <- merge(responsesDemo[,1:4], fullThetas, by="KOD")
fullThetasDemo$WiekTygodnie <- ceiling(fullThetasDemo$WiekDni/7)

#Get full thetas aggregated by days or weeks or months (put proper below - WiekDni, WiekTygodnie, WiekMiesiące)
fullThetasAggr <- aggregate(fullTheta ~ WiekDni + Płeć , data = fullThetasDemo, mean)
dateUnit <- colnames(fullThetasAggr)[1]

#Plot full theta vs. date unit
if (dateUnit == "WiekDni"){
  xlab <- "Days"
} else if (dateUnit == "WiekTygodnie"){
  xlab <- "Weeks"
} else if (dateUnit == "WiekMiesiące"){
  xlab <- "Months"
}
# title <- paste0("Relation between full theta and age in ", tolower(xlab))
# plot <- ggscatter(fullThetasAggr, y = "fullTheta", x = dateUnit,
#           add = "reg.line", conf.int = TRUE,
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = xlab, ylab = "Theta", title = title)
# print(plot)
# plotSave(plot, filename = paste0("Plots/Demographic/", tolower(xlab), ".png"))

#Plot full theta vs. gender
# title <- "Relation between theta and gender"
# plot <- ggerrorplot(fullThetasAggr, y = "fullTheta", x = "Płeć",
#                     add = "mean", error.plot = "errorbar", xlab = "Gender", ylab = "Theta") +
#   scale_x_discrete(labels = c('Boy','Girl'))
# print(plot)
# plotSave(plot, filename = paste0("Plots/Demographic/gender", ".png"))

#Get initial theta for each row (subject) by age and gender
obtainStartThetas <- function(age, gender){
  return(fullThetasAggr[fullThetasAggr[[dateUnit]] == age & fullThetasAggr$Płeć == gender, "fullTheta"])
}
startThetas <- mapply(obtainStartThetas, fullThetasDemo[[dateUnit]], fullThetasDemo$Płeć)
startThetas <- as.matrix(startThetas)

#------------
#SIMULATE CAT 
#------------

# 2. Fixed number of items
itemsNr <- 15

# 2.1. No start theta given
titleSufix <- "no start theta given"
folder <- "No start theta"
design <- list(min_items = itemsNr, max_items = itemsNr)
results1 <- mirtCAT(mo = mirtObject, method = 'ML', criteria = "MI", start_item = "MI", local_pattern = responses, cl = cl, design = design)

# 2.2. Initial thetas estimates given
# titleSufix <- paste0("start theta given by gender and age in ", tolower(xlab))
# folder <- paste0("Start theta by ", tolower(xlab))
# design <- list(min_items = itemsNr, max_items = itemsNr, thetas.start = startThetas)
# results1 <- mirtCAT(mo = mirtObject, method = 'ML', criteria = "MI", start_item = "MI", local_pattern = responses, cl = cl, design = design)

beep(sound=8)