#Set proper current working directory
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

#Read data
data <- read.csv("Data/irmik_lex.csv", encoding = "UTF-8")

#Count age in days
data$WiekDni<- difftime(data$DataWypAnkietyRodzic, data$DataUrDziecka, units = c("days"))
data$WiekDni <- ceiling(data$WiekDni)
colnames(data)[which(names(data) == "Wiek")] <- "WiekMiesiące"

#Remove unnecessary columns
data <- data[, c("DZIECKO_ID", "KOD", "WiekDni", "WiekMiesiące", "Płeć", "Wersja", "Kategoria", "Numer", "NumerSiG", "Pozycja", "R", "M")]

#Create df corresponding to CDI's content
cdi <- data[, c("Kategoria", "Numer", "NumerSiG", "Pozycja")]
cdi <- unique(cdi)
cdi <- cdi[order(cdi$Numer),]
cdi$Numer <- as.character(cdi$Numer)

#Remove wg version
data <- data[data$Wersja=="Z",]

#Reshape into wide format
data <- reshape(data,                           
                   timevar = "Numer",            
                   idvar = c("KOD","WiekDni","WiekMiesiące","Płeć"),        
                   direction = "wide",           
                   v.names = "M",           
                   drop = c("DZIECKO_ID", "R", "Pozycja", "Kategoria", "NumerSiG", "Wersja"))

#Prepare answers matrix
responses <- as.matrix(data[,5:ncol(data)])

#Remove "M." from column names
colnames(responses) <- paste0("item", substring(colnames(responses), 3))

#Prepare responses df with demographic variables for CAT simulations
responses <- responses[, order(as.integer(substring(colnames(responses), 5)))]
responsesDemo <- merge(data[1:4], responses, by=0)
responsesDemo$Row.names <- NULL
responsesDemo <- responsesDemo[order(responsesDemo$KOD), ]
rownames(responsesDemo) <- NULL
#write.csv(responsesDemo, file = "Data/responsesDemo.csv", fileEncoding = "utf-8", row.names = F)