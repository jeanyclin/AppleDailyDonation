library(XML)
library(RCurl)

donation.data = read.csv("./df_adjusted.csv", stringsAsFactors = FALSE)

# extract the article on the web

for( i in 1:nrow(donation.data)){
    print(i)
    url <- donation.data$url.article[i]
    html <- htmlParse(getURL(url))
    url.list.intro <- xpathSApply(html, "//p[@id='introid']", xmlValue)
    url.list.content <- xpathSApply(html, "//p[@id='bcontent']", xmlValue)
    intro = paste(unlist(url.list.intro), collapse=" ") 
    content = paste(unlist(url.list.content), collapse=" ") 
    article = paste(intro, content, sep=" ")
    write(article, paste("./donation_articles/", donation.data$aid[i], ".txt", sep=""))
}


