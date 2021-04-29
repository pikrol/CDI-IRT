library(plyr) #For laply
library(dplyr) #For distribution analysis
library(forcats) #For data imputation (replacing NAs)

#Prepare results folder
resultsFolder <- paste0("Plots/", folder0, "/", folder, "/")
if(!dir.exists(file.path(getwd(), "Plots"))) dir.create(file.path(getwd(), "Plots"))
if(!dir.exists(file.path(paste0(getwd(), "Plots/"), folder0))) dir.create(file.path(paste0(getwd(), "/Plots"), folder0))
if(!dir.exists(file.path(paste0(getwd(), "Plots/", folder0, "/"), folder))) dir.create(file.path(paste0(getwd(), "/Plots/", folder0, "/"), folder))

#Set mirtCAT results variable
results <- results1

#Prepare frequency data frame
frequencyDf <- data.frame(itemNr = 1 : nrow(params))
itemsAnswered <- laply(results, function(x) x$items_answered)
itemsAnsweredFreq <- as.data.frame(table(as.vector(itemsAnswered)))
colnames(itemsAnsweredFreq) <- c("itemNr", "freq")

#--------------------------------------------
#CORRELATION BETWEEN ESTIMATED AND TRUE THETA
#--------------------------------------------

#Get estimated thetas
thetas <- laply(results, function(x) x$thetas)

#Plot est. theta vs. true theta
title <- paste0(itemsNr, " items - ", titleSufix)
plot <- ggscatter(data.frame(estTheta = thetas, fullTheta = fullThetas$fullTheta), y = "fullTheta", x = "estTheta",
                  add = "reg.line", conf.int = TRUE,
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Estimated Theta", ylab = "True Theta", title = title)
print(plot)
plotSave(plot, filename = paste0(resultsFolder, "cor - ", itemsNr, " items", ".png"))

#------------------------------
#DISTRIBUTION OF USAGE OF ITEMS
#------------------------------

frequencyDf <- merge(frequencyDf, itemsAnsweredFreq, by = "itemNr", all.x = TRUE)
frequencyDf[is.na(frequencyDf)] <- 0
frequencyDf$freq <- (frequencyDf$freq / length(results))
cuts <- cut(frequencyDf$freq, breaks = c(0, 0.1, 0.2, 0.3, 0.4, 1), labels = c("(0 - 10%]", "(10 - 20%]", "(20 - 30%]", "(30 - 40%]", "over 40%"), dig.lab = 0)
frequencyDf$cut <- cuts
frequencyDf = frequencyDf %>% mutate_if(is.factor,
                                        fct_explicit_na,
                                        na_level = "0")

#Prepare data frame with counts and cuts
df <- frequencyDf %>%
  group_by(cut) %>%
  summarise(count = n())
df$cut <- factor(df$cut,levels(df$cut)[c(6,1,2,3,4,5)])

#Plot distribution
title <- paste0(itemsNr, " items - ", titleSufix)
plot <- ggplot(df, aes(x = cut, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle(title) +
  geom_text(aes(label = count), vjust = -0.3) + 
  theme_pubclean() 
print(plot)
plotSave(plot, filename = paste0(resultsFolder, "distr - ", itemsNr, " items", ".png"))

#------------
#THETA VS. SE
#------------

#Get error estimates
SEthetas <- laply(results, function(x) x$SE_thetas)

#Plot est.theta vs. SE
title <- paste0(itemsNr, " items - ", titleSufix)
plot <- ggscatter(data.frame(estTheta = thetas, SE = SEthetas), y = "SE", x = "estTheta", #shape=21 for empty points
                  
                  xlab = "Theta", ylab = "SE", title = title)
print(plot)
plotSave(plot, filename = paste0(resultsFolder, "SE - ", itemsNr, " items", ".png"))

#-----------------------
#NOT FITTING ITEMS USED?
#-----------------------

#Prepare frequency data frame
itemsAnsweredFreq$freq <- round(itemsAnsweredFreq$freq/length(results), 2)

#Add column whether item is not fitting or not
itemsAnsweredFreq$notFitting <- laply(itemsAnsweredFreq$itemNr, function(x){
  if (is.element(x, notFitting)){
    return(1)
  } else {
    return(0)
  }
})
itemsAnsweredFreq <- itemsAnsweredFreq[order(itemsAnsweredFreq$notFitting, decreasing = TRUE), ]

#Add position column
cdi <- read.csv("Data/cdi.csv", encoding = "UTF-8")
itemsAnsweredFreq$position <- cdi[itemsAnsweredFreq$itemNr, "Pozycja"]

#Save to file
write.csv(itemsAnsweredFreq, file = "Results/notFittingItemsUsage.csv", fileEncoding = "utf-8", row.names = F)
write.table(table(itemsAnsweredFreq$notFitting), file = "Results/notFittingItemsNumbers.txt", sep = "\t", col.names = FALSE, row.names = FALSE)

#----------------
#WHAT ITEMS USED?
#----------------

itemsAnsweredFreq <- itemsAnsweredFreq[order(itemsAnsweredFreq$freq, decreasing = TRUE), ]
