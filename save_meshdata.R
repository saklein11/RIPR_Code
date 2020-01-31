library(XML)

## this file should be updated on a yearly basis
## downloaded from https://www.nlm.nih.gov/databases/download/mesh.html
## go to "XML Format: current production year mesh" 
data <- xmlParse("C:\\Users/allison.kolbe/Downloads/desc2020.xml")
xml_data2 <- xmlToDataFrame(data)
xml_data2$DescriptorName <- tolower(xml_data2$DescriptorName)

write.csv(xml_data2, file = "C:\\Users/allison.kolbe/Desktop/Bibliometrics/mesh2020.csv")