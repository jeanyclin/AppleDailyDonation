library(XML)

#========== extract data from webpage ==========#
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

extract.webpage = function(link, tag.path){
    webpage = readLines(url(link))
    htmlpage = htmlParse(webpage, asText = TRUE)
    tag.values = xpathSApply(htmlpage, tag.path, xmlValue)
    
    tag.values
}

donation.amount = donation.amount[-3024]



for(i in 1:nrow(donationData)){
    
    webpage = readLines(paste("http://search.appledaily.com.tw/charity/projdetail/proj/", donationData[i,"aid"], sep=""))
    htmlpage = htmlParse(webpage, asText = TRUE)
    table.cell.data = xpathSApply(htmlpage, "//td[@id='wordcenter']", xmlValue)
    
    if(length(table.cell.data) > 0){
        donation.details = data.frame(matrix(ncol = 4, nrow = 10))
        colnames(donation.details) = c("Index", "Name", "Amount", "Date")
        index = 1
        for(j in seq(from = 1, to = length(table.cell.data), by = 4)){
            donation.details[index,] = as.character(trim(table.cell.data[j:(j+3)]))
            index = index + 1
        }
    }
    
    write.csv(donation.details, paste(donationData[i,"aid"], ".csv", sep=""), row.names = F)
}

#========== get the donation count for each person ==========#
donor.count = data.frame(matrix(ncol = 2, nrow = 0))
colnames(donor.count) = c("Name", "Count")
filepath = "C:/Users/Jean/Documents/donation_details/"

for(k in 1:10){
    print(donationData[k,"aid"])
    donation.details = read.csv(paste(filepath, donationData[k,"aid"], ".csv", sep=""), header = T, stringsAsFactors = F)
    
    for(i in 1:nrow(donation.details)){
        donor.names = unlist(strsplit(donation.details[i, "Name"], "、"))
        
        for(j in seq(donor.names)){
            select.row = which(donor.count[,"Name"] == donor.names[j])
            
            if(length(select.row) == 0){
                donor.count = rbind(donor.count, data.frame(Name = donor.names[j], Count = 1 ))
            }else if(length(select.row == 1)){
                donor.count[select.row, "Count"] = donor.count[select.row, "Count"] + 1
            }else{
                print("ERROR")
                print(select.row)
                break
            }
        }
    }
}
