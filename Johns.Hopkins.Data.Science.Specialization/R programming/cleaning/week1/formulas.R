formulas used

sum(gasdata1$Zip*gasdata1$Ext,na.rm=T)

gasdata1<-read.xlsx("sample.xlsx",sheetIndex=1,rowIndex=18:23,colIndex=7:15)

download.file(fileUrl, destfile='sample.xlsx', mode='wb')

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"

> fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
> data4 <- xmlTreeParse(fileURL, useInternal = TRUE)
> rootNode <- xmlRoot(data4)
> xmlName(rootNode)

rootNode[[1]][[1]][[2]]
dat2 <- xpathSApply(rootNode,"//zipcode",xmlValue)dat2 <0

> dat3 <- dat2==21231
> dat4 <- dat2[dat3]in

systemtime1 <- system.time(for(i in 1:100){sapply(split(DT$pwgtp15,DT$SEX),mean)})