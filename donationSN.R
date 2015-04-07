library(XML)

#========== extract data from webpage ==========#
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

library(XML)
extract.webpage = function(link, tag.path){
    webpage = readLines(url(link))
    htmlpage = htmlParse(webpage, asText = TRUE)
    tag.values = xpathSApply(htmlpage, tag.path, xmlValue)
    
    tag.values
}

# get all donation ID, date and amount
get.donation.progress = function(){
    donation.progress = data.frame(matrix(ncol = 3, nrow = 10))
    colnames(donation.progress) = c("projectID", "date", "amount")
    index = 1
    for(i in 1:173){
        link = paste("http://search.appledaily.com.tw/charity/projlist/Page/", i, sep = "")
        tag.path = "//td"
        
        #get the web page content
        tag.values = extract.webpage(link, tag.path)
        
        #get the project ID and donation amount
        for(j in seq(from = 2, to = length(tag.values), by = 6)){
            if(tag.values[j+3] == "已結案"){
                donation.progress[index, "projectID"] = tag.values[j]
                donation.progress[index, "date"] = tag.values[j+2]
                donation.progress[index, "amount"] = as.integer(tag.values[j+4])
                index = index + 1 
            }
        }
        
    }
    write.csv(donation.progress, "donation_progress.csv", row.names = F)
    
    donation.progress
}


#get the donation details of each project from the websites
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
get.donation.count = function(num.projects){
    donor.count = data.frame(matrix(ncol = 2, nrow = 0))
    colnames(donor.count) = c("Name", "Count")
    filepath = "./donation_details/"
    
    #order aid by published date
    dt.published.ordered = donationData[with(donationData, order(dt.published)), c("aid", "dt.published")]
    
    for(k in 1:num.projects){
        print(dt.published.ordered[k,"aid"])
        donation.details = read.csv(paste(filepath, dt.published.ordered[k,"aid"], ".csv", sep=""), header = T, stringsAsFactors = F)
        
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
    
    write.csv(donor.count, "donor_count.csv", row.names = F)
    
}


gen.event.matrix = function(start.proj, end.proj, filepath){
    #order aid by published date
    dt.published.ordered = donationData[with(donationData, order(dt.published)), c("aid", "dt.published")]
    
    #get the start date of the donation event
    donation.details = read.csv(paste(filepath, dt.published.ordered[start.proj,"aid"], ".csv", sep=""), header = T, stringsAsFactors = F)
    donation.details$Date = as.Date(donation.details$Date)
    start.date = min(donation.details$Date)
    
    #get the end date of donation event
    donation.details = read.csv(paste(filepath, dt.published.ordered[end.proj,"aid"], ".csv", sep=""), header = T, stringsAsFactors = F)
    donation.details$Date = as.Date(donation.details$Date)
    end.date = max(donation.details$Date)
    
    # for event matrix data structurej
    event.matrix = matrix(data = 0, ncol = as.integer(end.date - start.date)+1 , nrow = nrow(donor.count))
    row.names(event.matrix) = donor.count$Name
    colnames(event.matrix) = as.character(seq(from = start.date, to = end.date, by="day"))
    
    for(k in start.proj:end.proj){
        print(dt.published.ordered[k,"aid"])
        donation.details = read.csv(paste(filepath, dt.published.ordered[k,"aid"], ".csv", sep=""), header = T, stringsAsFactors = F)
        donation.details$Date = strftime(donation.details$Date, "%Y-%m-%d")
        
        for(i in 1:nrow(donation.details)){
            donor.names = unlist(strsplit(donation.details[i, "Name"], "、"))
            
            for(j in seq(donor.names)){
                event.matrix[donor.names[j], donation.details[i,"Date"]] = event.matrix[donor.names[j], donation.details[i,"Date"]] + 1
            }
        }
    }
    
    event.matrix   
}

#generate donation event matrix in 2008
event.matrix.2008 = gen.event.matrix(start.proj = 1, end.proj = 288, filepath = "./donation_details/")

# donation trend of each project
get.project.trend = function(proj.index, filepath){
    #order aid by published date
    dt.published.ordered = donationData[with(donationData, order(dt.published)), c("aid", "dt.published")]
    
    #get the start date of the donation event
    donation.details = read.csv(paste(filepath, dt.published.ordered[proj.index,"aid"], ".csv", sep=""), header = T, stringsAsFactors = F)
    
    donation.amount.daily = tapply(donation.details$Amount, donation.details$Date, sum)
    trend = data.frame(date = names(donation.amount.daily), total = donation.amount.daily)
    trend$date = as.Date(trend$date, "%Y/%m/%d")
    trend = trend[with(trend, order(date, decreasing = F)), ]
    trend
}

# get donation trend of random projects
proj.indices = sample(1:1581, 3, replace = FALSE)

for(i in seq(proj.indices)){
    trend = get.project.trend(proj.indices[i], "./donation_details/")
    print(sum(trend$total))
    require(ggplot2)
    g = ggplot( data = trend, aes( date, total ))
    print(g + geom_point() +geom_line())
}
    

