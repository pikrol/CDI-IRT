#Read data (accesible on https://drive.google.com/file/d/1i4YupJCfE6qt1iZ9A6iqZaxXv71gZ_JV/view)
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
row.names(cdi) <- NULL
colnames(cdi) <- c("category", "number.ws", "number.wg", "position")
write.csv(cdi, file = "Data/cdi.csv", fileEncoding = "utf-8", row.names = F)

#Choose version
data <- data[data$Wersja=="Z",]

#Reshape into wide format (v.names = M|R and drop the other one)
data <- reshape(data,                           
                   timevar = "Numer",            
                   idvar = c("KOD","WiekDni","WiekMiesiące","Płeć"),        
                   direction = "wide",           
                   v.names = "M",           
                   drop = c("DZIECKO_ID", "R", "Pozycja", "Kategoria", "NumerSiG", "Wersja"))

#Prepare responses matrix
responses <- as.matrix(data[,5:ncol(data)])
colnames(responses) <- paste0("item", substring(colnames(responses), 3))
responses <- responses[, order(as.integer(substring(colnames(responses), 5)))]

#Prepare responses df with demographic variables
responses_demo <- merge(data[1:4], responses, by=0)
responses_demo$Row.names <- NULL
responses_demo <- responses_demo[order(responses_demo$KOD), ]
rownames(responses_demo) <- NULL

#Change to English names
names(responses_demo)[names(responses_demo) == 'KOD'] <- 'participant'
names(responses_demo)[names(responses_demo) == 'WiekDni'] <- 'days'
names(responses_demo)[names(responses_demo) == 'WiekMiesiące'] <- 'months'
names(responses_demo)[names(responses_demo) == 'Płeć'] <- 'gender'
write.csv(responses_demo, file = "Data/responses_demo.csv", fileEncoding = "utf-8", row.names = F)