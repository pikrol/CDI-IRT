library(plyr) #For laply
library(dplyr) #For distribution analysis
library(forcats) #For data imputation (replacing NAs)

#Read thetas, SEs and items answered
thetas <- laply(results, function(x) x$thetas)
SEthetas <- laply(results, function(x) x$SE_thetas)
itemsAnswered <- laply(results, function(x) x$items_answered) #if no SE criterion
if (exists("SE")) testsLengths <- laply(results, function(x) length(x$items_answered)) #if SE criterion

#Check average length of the test (if min SE stop criterion)
averageLength <- mean(laply(results, function(x) length(x$items_answered)))

#Update meanSE (if number of items stop criterion used)
if (exists("itemsNr")){
  meanSE[meanSE$items == itemsNr, "meanSE"] <- mean(SEthetas)
  write.csv(meanSE, file = meanSEfile, fileEncoding = "utf-8", row.names = F)
}

#----------------------------------------------------------------------
#INVESTIGATE TESTS LENGTHS [OPTIONAL - USE ONLY WITH SE STOP CRITERION]
#----------------------------------------------------------------------

cuts <- cut(testsLengths, breaks = c(0, 10, 20, 50, 100, 200, 669, 670), labels = c("1 - 10", "11 - 20", "21 - 50", "51 - 100", "101 - 200", "201 - 669", "670"), dig.lab = 0)
df <- as.data.frame(table(cuts))

#Plot distribution
plot <- ggplot(df, aes(x = cuts, y = Freq)) +
  xlab("Number of items") +
  ylab("Number of tests") +
  geom_bar(stat = "identity") +
  ggtitle(paste0(title, "\nAverage test length: ", round(mean(testsLengths), 2))) +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme_pubclean() 
print(plot)
plotSave(plot, filename = paste0(simFolder, "tests lengths.png"))

#--------------------------------------------
#CORRELATION BETWEEN ESTIMATED AND TRUE THETA
#--------------------------------------------

#Plot est. theta vs. true theta
plot <- ggscatter(data.frame(estTheta = thetas, fullTheta = fullThetasDf$fullTheta), y = "fullTheta", x = "estTheta",
                  add = "reg.line", conf.int = TRUE,
                  cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Estimated Theta", ylab = "True Theta", title = title)
print(plot)
plotSave(plot, filename = corFileName)

#------------
#THETA VS. SE
#------------

#Plot est.theta vs. SE
plot <- ggscatter(data.frame(estTheta = thetas, SE = SEthetas), y = "SE", x = "estTheta", #shape=21 for empty points
                  xlab = "Theta", ylab = "SE", title = paste0(title, "\nAverage SE: ", round(mean(SEthetas),2)))
print(plot)
plotSave(plot, filename = seFileName)

#------------------------------
#DISTRIBUTION OF USAGE OF ITEMS
#------------------------------

#Prepare df with items used and their frequency
itemsAnsweredFreq <- as.data.frame(table(as.vector(itemsAnswered)))
colnames(itemsAnsweredFreq) <- c("itemNr", "freq")

#Prepare frequency df
frequencyDf <- data.frame(itemNr = 1 : nrow(params))
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
plot <- ggplot(df, aes(x = cut, y = count)) +
  geom_bar(stat = "identity") +
  ggtitle(title) +
  geom_text(aes(label = count), vjust = -0.3) + 
  theme_pubclean() 
print(plot)
plotSave(plot, filename = paste0(simFolder, "distr - ", itemsNr, " items", ".png"))

#----------------------------
#MEAN SE VS. NUMEBER OF ITEMS
#----------------------------

plot <- ggplot(meanSE, aes(items, meanSE)) + geom_point() + geom_line() + xlab("Number of items") + ylab("Mean SE") +
  ggtitle(paste0("Mean SE vs. number of items - ", titleSufix))
print(plot)
plotSave(plot, filename = paste0(simFolder, "Mean SE vs. number of items", ".png"))

#---------------------
#WHAT CATEGORIES USED?
#---------------------

cdi <- read.csv("Data/cdi.csv", encoding = "UTF-8")
catsAnswered <- as.vector(itemsAnswered)
catsAnswered <- cdi[catsAnswered, "category"]
catsAnswered <- sort(table(catsAnswered)/length(catsAnswered), decreasing = TRUE)
write.table(catsAnswered, file = paste0(simFolder, "Popular categories - ", itemsNr, " items.txt"), sep = "\t", col.names = c("Category", "Percent of items used"), row.names = FALSE)

#--------------
#CDI CATEGORIES
#--------------

cdiCategories <- sort(table(cdi$category)/nrow(cdi), decreasing = TRUE)
write.table(cdiCategories, file = paste0(resultsPath, "cdi categories.txt"), sep = "\t", col.names = c("Category", "Percent of items"), row.names = FALSE)

#-----------------
#WHICH ITEMS USED?
#-----------------

#Prepare df with items used and their frequency
itemsAnsweredFreq <- as.data.frame(table(as.vector(itemsAnswered)))
colnames(itemsAnsweredFreq) <- c("itemNr", "freq")
itemsAnsweredFreq$freq <- round(itemsAnsweredFreq$freq/length(results), 2)

#Add position & category
cdi <- read.csv("Data/cdi.csv", encoding = "UTF-8")
cdi$itemNr <- cdi$number.ws
itemsAnsweredFreq <- merge(itemsAnsweredFreq, cdi, by="itemNr")
itemsAnsweredFreq <- within(itemsAnsweredFreq, rm(number.ws, number.wg))
itemsAnsweredFreq <- itemsAnsweredFreq[order(itemsAnsweredFreq$freq, decreasing = TRUE), ]
colnames(itemsAnsweredFreq) <- c("itemNr", "freq", "category", "position")

#Save to file
write.csv(itemsAnsweredFreq, file = paste0(simFolder, "Which items - ", itemsNr, "i.csv"), fileEncoding = "utf-8", row.names = F)
# 
# #Check which categories were most present in simulations
# categoriesFreq <- sort(table(itemsAnsweredFreq$category), decreasing = TRUE)
# write.table(categoriesFreq, file = "Results/Number of items used with respect to category.txt", sep = "\t", col.names = c("Category", "Number of items"), row.names = FALSE)
# 
# #Check how many items were used from particular categories
# categoriesFreq <- sort(table(itemsAnsweredFreq$category) / table(cdi$Kategoria), decreasing = TRUE)
# write.table(categoriesFreq, file = "Results/Percents of items from categories.txt", sep = "\t", col.names = c("Category", "Percent of items used"), row.names = FALSE)

