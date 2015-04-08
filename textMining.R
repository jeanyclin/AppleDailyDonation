library(XML)
library(RCurl)

data <- list()

for( i in 1:nrow(donationData)){
    tmp <- paste(i, '.html', sep='')
    url <- paste('www.ptt.cc/bbs/StupidClown/index', tmp, sep='')
    html <- htmlParse(getURL(url))
    url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
    data <- rbind(data, paste('www.ptt.cc', url.list, sep=''))
}
data <- unlist(data)