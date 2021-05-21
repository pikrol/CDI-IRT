library(plyr) #For laply
library(dplyr) #For distribution analysis
library(forcats) #For data imputation (replacing NAs)

#Read thetas, SEs and items answered
thetas <- laply(results, function(x) x$thetas)
SEthetas <- laply(results, function(x) x$SE_thetas)
if (fixedNumberStop) itemsAnswered <- laply(results, function(x) x$items_answered) #if no SE criterion
if (!fixedNumberStop) testsLengths <- laply(results, function(x) length(x$items_answered)) #if SE criterion

#Update meanSE df (if number of items stop criterion used)
if (fixedNumberStop){
  meanSE[meanSE$items == itemsNr, "meanSE"] <- mean(SEthetas)
  write.csv(meanSE, file = meanSEfile, fileEncoding = "utf-8", row.names = F)
}

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

#---------------------------------------------
### STOP CRITERION: MINIMAL STANDARD ERROR ###
#---------------------------------------------
#-------------------------
#INVESTIGATE TESTS LENGTHS
#-------------------------

#Prepare cuts
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
  
#---------------------
#THETA VS. TEST LENGTH
#---------------------

#Plot est. theta vs. true theta
plot <- ggscatter(data.frame(estTheta = thetas, testLength = testsLengths), y = "testLength", x = "estTheta",
                  xlab = "Estimated Theta", ylab = "Length of test", title = title)
print(plot)
plotSave(plot, filename = paste0(simFolder, "theta vs. length.png"))

#--------------------------------------------
### STOP CRITERION: FIXED NUMBER OF ITEMS ###
#--------------------------------------------
#------------------------------
#DISTRIBUTION OF USAGE OF ITEMS
#------------------------------

if (fixedNumberStop){
  
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
  
}

#----------------------------
#MEAN SE VS. NUMEBER OF ITEMS
#----------------------------

plot <- ggplot(meanSE, aes(items, meanSE)) + geom_point(size = 4) + xlab("Number of items") + ylab("Mean SE") +
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