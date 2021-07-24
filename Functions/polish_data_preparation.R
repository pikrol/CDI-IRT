# Function preparing Polish CDI data (version = "G"|"Z", write_to_files writes to ./Data/)
# Currently for WG prepares comprehension responses only

prepare_irmik <- function(version, write_to_files = FALSE) {

     #Read data
     data <- read.csv("Data/irmik_lex.csv", encoding = "UTF-8")
     
     #Choose version
     data <- data[data$Wersja==version,]
     
     #Count age in days
     data$WiekDni<- difftime(data$DataWypAnkietyRodzic, data$DataUrDziecka, units = c("days"))
     data$WiekDni <- ceiling(data$WiekDni)
     colnames(data)[which(names(data) == "Wiek")] <- "WiekMiesiące"
     
     #Remove unnecessary columns
     data <- data[, c("DZIECKO_ID", "KOD", "WiekDni", "WiekMiesiące", "Płeć", "Wersja", "Kategoria", "Numer", "NumerSiG", "Pozycja", "R", "M")]
     
     #Create df corresponding to CDI's content
     cdi <<- data[, c("Kategoria", "Numer", "NumerSiG", "Pozycja")]
     cdi <<- unique(cdi)
     if(version == "G") cdi <<- cdi[order(cdi$NumerSiG),] else cdi <<- cdi[order(cdi$Numer),]
     colnames(cdi) <<- c("category", "number.ws", "number.wg", "position")
     if(write_to_files) write.csv(cdi, file = "Data/cdi.csv", fileEncoding = "utf-8", row.names = F)
     
     #Create table with cdi categories and percents of items
     cdi_categories <<- sort(table(cdi$category)/nrow(cdi), decreasing = TRUE)
     if(write_to_files) write.table(cdi_categories, file =  "Data/cdi_categories.txt", sep = "\t", col.names = c("Category", "Percent of items"), fileEncoding = "utf-8", row.names = FALSE)
     
     #Reshape into wide format
     if(version == "G") {
          data <- reshape(data,                           
                             timevar = "NumerSiG",            
                             idvar = c("KOD","WiekDni","WiekMiesiące","Płeć"),        
                             direction = "wide",           
                             v.names = "R",           
                             drop = c("DZIECKO_ID", "M", "Pozycja", "Kategoria", "Numer", "Wersja"))
     } else {
          data <- reshape(data,                           
                          timevar = "Numer",            
                          idvar = c("KOD","WiekDni","WiekMiesiące","Płeć"),        
                          direction = "wide",           
                          v.names = "M",           
                          drop = c("DZIECKO_ID", "R", "Pozycja", "Kategoria", "NumerSiG", "Wersja"))
     }     
     
     #Prepare responses matrix
     responses <- as.matrix(data[,5:ncol(data)])
     colnames(responses) <- paste0("item", substring(colnames(responses), 3))
     responses <- responses[, order(as.integer(substring(colnames(responses), 5)))]
     
     #Prepare responses df with demographic variables
     responses_demo <<- merge(data[1:4], responses, by=0)
     responses_demo$Row.names <<- NULL
     responses_demo <<- responses_demo[order(responses_demo$KOD), ]

     #Change to English names
     names(responses_demo)[names(responses_demo) == 'KOD'] <<- 'participant'
     names(responses_demo)[names(responses_demo) == 'WiekDni'] <<- 'days'
     names(responses_demo)[names(responses_demo) == 'WiekMiesiące'] <<- 'months'
     names(responses_demo)[names(responses_demo) == 'Płeć'] <<- 'gender'
     if(write_to_files) write.csv(responses_demo, file = "Data/responses_demo.csv", fileEncoding = "utf-8", row.names = F)
}
