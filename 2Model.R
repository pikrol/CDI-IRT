library(mirt)
library(ggpubr) #For making plots
options(max.print=999999)

#If not done after 1DataPreparation load ready csv:
responsesDemo <- read.csv("Data/responsesDemo.csv", encoding = "UTF-8") 
responses <- as.matrix(responsesDemo[,5:ncol(responsesDemo)])
cdi <- read.csv("Data/cdi.csv", encoding = "UTF-8")

#To save any plot
# png("Plots/...", width = 853, pointsize = 16)
# dev.off()

#--------------
#MODEL CREATION 
#--------------

#Create mirt model (it takes some time)
# library(beepr)
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

#Get items params
params <- as.data.frame(coef(model, IRTpars = TRUE, simplify = TRUE)$items)
params$g <- NULL
params$u <- NULL

#Plot difficulty vs. number of selections (to check if params makes sense)
# plot(params$b, colSums(responses), xlab = "Item difficulty", ylab = "Number of selections")

#Prepare params df with positions and categories
params$position <- cdi$Pozycja
params$category <- cdi$Kategoria
params$number.ws <- cdi$Numer
write.csv(params, file = "Data/params.csv", fileEncoding = "utf-8", row.names = F)

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

#Obtain full thetas
# fullThetas <- fscores(model)

#Save full thetas to file
# fullThetas <- data.frame(KOD = responsesDemo$KOD, fullTheta = fullThetas)
# colnames(fullThetas) <- c("KOD", "fullTheta")
# write.csv(fullThetas, file = "Data/fullThetas.csv", fileEncoding = "utf-8", row.names = F)

#Read already prepared full thetas
fullThetas <- read.csv("Data/fullThetas.csv", encoding = "UTF-8")

#Save full thetas histogram
hist(fullThetas$fullTheta, xlab = "Theta", main = "Histogram of full thetas")

#Plot full thetas distribution in age groups
fullThetasAge <- data.frame(age = responsesDemo$WiekMiesiące, fullTheta = fullThetas$fullTheta)
gghistogram(fullThetasAge, x = "fullTheta", bins = 10) + 
  facet_wrap(~age) + 
  labs(title = "Full thetas distribution in particular age groups") + 
  xlab("Full theta") + 
  ylab("Count")

#Plot total score vs. theta
# plot(data.frame(Theta = fullThetas$fullTheta, Score = rowSums(responses)), xlab = "Theta", ylab = "Total score", main = "Relation between theta and total score")

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
# nrow(itemsFit[itemsFit$p.S_X2 < 0.05, ])

#Order by p-values
# itemsFit <- itemsFit[order(itemsFit$p.S_X2), ]

#Plot any item
# itemplot(model, 652)

#Get numbers of not fitting items
# notFitting <- itemsFit[itemsFit$p.S_X2 < 0.05, "item"]
# save(notFitting, file = "Data/notFitting")

#Load saved vector with not fitting items numbers
load("Data/notFitting")

#Prepare df with not fitting items
notFittingItems <- cdi[is.element(cdi$Numer, notFitting), ]

#Order by RMSEA
# itemsFitRestricted <- itemsFit[is.element(itemsFit$item, notFitting), ]
# itemsFitRestricted <- itemsFitRestricted[order(itemsFitRestricted$RMSEA.S_X2), ]

#-----------------
#NOT FITTING ITEMS
#-----------------

#Prepare df with not fitting items and their params
# notFittingItems$Discrimination <- params[is.element(params$number.ws, notFittingItems$Numer), "a"]
# notFittingItems$Difficulty <- params[is.element(params$number.ws, notFittingItems$Numer), "b"]
# notFittingItems$DscrmnNormal <- ifelse(notFittingItems$Discrimination > mean(params$a) - sd(params$a) & notFittingItems$Discrimination < mean(params$a) + sd(params$a), 1, 0)
# notFittingItems$DiffNormal <- ifelse(notFittingItems$Difficulty > mean(params$b) - sd(params$b) & notFittingItems$Difficulty < mean(params$b) + sd(params$b), 1, 0)
write.csv(notFittingItems, file = "Results/notFittingItems.csv", fileEncoding = "utf-8", row.names = F)

#Check not fitting categories
# notFittingCategories <- sort(table(notFittingItems$Kategoria), decreasing = TRUE)
notFittingCategories <- sort(table(notFittingItems$Kategoria) / table(cdi$Kategoria), decreasing = TRUE)
write.table(notFittingCategories, file = "Results/notFittingCategoriesPercents.txt", sep = "\t", col.names = c("Category", "Percent of not fitting items"), row.names = FALSE)