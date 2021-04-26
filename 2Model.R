library(mirt)

#--------------
#MODEL CREATION 
#--------------

#Create mirt model (it takes some time)
# library(beepr)
# model <- mirt(answers, 1)
# beep(sound=8)
# save(model, file = "Data/model")

#Load saved model
load("Data/model")

#-----------
#FULL THETAS
#-----------

#Obtain full thetas
fullThetas <- fscores(model)

#Save full thetas histogram
png("Plots/fullThetas.png", width = 853, pointsize = 16)
hist(fullThetas, xlab = "Theta", main = "Histogram of full thetas")
dev.off()

#Save full thetas to file
fullThetas <- data.frame(KOD = responsesDemo$KOD, fullTheta = fullThetas)
colnames(fullThetas) <- c("KOD", "fullTheta")
# write.csv(fullThetas, file = "Data/fullThetas.csv", fileEncoding = "utf-8", row.names = F)

png("Plots/PointsVsTheta.png", width = 853, pointsize = 16)
plot(data.frame(Score = rowSums(responses), Theta = fullThetas), xlab = "Points", ylab = "Theta", main = "Relation between theta and points")
dev.off()

#---------
#ITEMS FIT 
#---------

#Check items fit (it takes some time)
# itemsFit <- itemfit(model)
# itemsFit <- itemsFit[order(as.integer(itemsFit$item)), ]
# rownames(itemsFit) <- NULL
# write.csv(itemsFit, file = "Data/itemsFit.csv", fileEncoding = "utf-8", row.names = F)

#Load saved items fit
itemsFit <- read.csv("Data/itemsFit.csv", encoding = "UTF-8")

#Check number of not fitting items (p < 0.05) - 35
nrow(itemsFit[itemsFit$p.S_X2 < 0.05, ])

#Order by p-values
# itemsFit <- itemsFit[order(itemsFit$p.S_X2), ]

#Plot any item
# itemplot(model, 652)

#Get numbers of not fitting items
notFitting <- itemsFit[itemsFit$p.S_X2 < 0.05, "item"]
save(notFitting, file = "Data/notFitting")

#----------------
#ITEMS PARAMETERS
#----------------

#Obtain items parameters from model
l <- coef(model)
l$GroupPars <- NULL
params <- data.frame(matrix(unlist(l), nrow=670, byrow=T))[1:2]
colnames(params) <- c("a1", "d")
params$Numer <- names(l)
params <- merge(cdi, params, by = "Numer")
params <- params[,c("Numer", "NumerSiG", "Pozycja", "a1", "d", "Kategoria")]
params <- params[order(as.integer(params$Numer)), ]
rownames(params) <- NULL
# write.csv(params, file = "Data/params.csv", fileEncoding = "utf-8", row.names = F)

#Save items params plot
png("Plots/params.png", width = 853, pointsize = 16)
plot(params$d, params$a1, xlab = "Difficulty", ylab = "Discrimination", main = "Items parameters")
dev.off()