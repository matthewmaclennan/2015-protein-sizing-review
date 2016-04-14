library(XML);library(RCurl);
#Do search on multiple databases for KEYWORD
#Finding DOIs in html
xpathApply(htmlTreeParse(getURL("http://pubs.acs.org/JACSbeta/jvi/issue31.html"),useInternalNode=T),"//div[@class='DOI']")
#accessing the page for articles DOI
#
#
#WOL html search for DOIs; include the string &start=1 in the url to change the starting point.
regmatches(unlist(xpathApply(xmlTreeParse(getURL("http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&query=capillary+electrophoresis+acid&inTheLastList=6&queryStringEntered=false&searchRowCriteria[0].fieldName=all-fields&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].fieldName=all-fields&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and"),
useInternalNode=T),"//@href")),
gregexpr("/doi/[A-Za-z0-9.-/\\(\\)]+[e?pdf|full|abstract|references]$",
unlist(xpathApply(xmlTreeParse(getURL("http://onlinelibrary.wiley.com/advanced/search/results/reentry?scope=allContent&query=capillary+electrophoresis+acid&inTheLastList=6&queryStringEntered=false&searchRowCriteria[0].fieldName=all-fields&searchRowCriteria[0].booleanConnector=and&searchRowCriteria[1].fieldName=all-fields&searchRowCriteria[1].booleanConnector=and&searchRowCriteria[2].fieldName=all-fields&searchRowCriteria[2].booleanConnector=and"),
useInternalNode=T),"//@href"))))
#creates list object
#name bob and lop off the reader format string to leave the doi strings (not URLs)
unlist(strsplit(unlist(bob),"(abstract|references|e?pdf?|full)$"))
#Springer uses the following





<><><>><><><><><><><><><>><><><><><><><><><<


library(XML);library(RCurl);
totalresults<-xpathApply(xmlTreeParse(getURL("api.elsevier.com/content/search/scidir?apiKey=ba0f6a67c909e5cd1e2dae428bb2b94d&query=TITLE-ABS-KEY%28dynamic+light+scattering%29&date=2010-2016&httpAccept=%20application%2Fxml&start=0&count=100")
,useInternalNode=T),"//opensearch:totalResults/text() div 1")
refreshes<-floor(totalresults/100)
results<-c()
  for(i in 0:refreshes*100){
    resultpage<-getURL(paste0("api.elsevier.com/content/search/scidir?apiKey=ba0f6a67c909e5cd1e2dae428bb2b94d&query=TITLE-ABS-KEY%28dynamic+light+scattering%29&date=2010-2016&httpAccept=%20application%2Fxml&start=",i,"&count=100"))

    results<-c(results,resultpage)
  }
DLSelsevier<-results
#sequester titles
DLSelsevierTitles<-lapply(DLSelsevier,function(x) unlist(xpathApply(htmlTreeParse(x,useInternalNode=T),"//title",xmlValue)))
#unlist to single vector
DLSelsevierTitles<-unlist(DLSelsevierTitles)
write.csv(DLSelsevierTitles,"C:\\Users\\Matthew S. MacLennan\\Desktop\\DLSelsevierTitles.csv")
library(tm)

tmpreproc<-function(DLSelsevierTitlesx){
require(tm)
require(SnowballC)
DLSelsevierTitlesC<-Corpus(VectorSource(DLSelsevierTitlesx))
DLSelsevierTitlesC <- tm_map(DLSelsevierTitlesC, removePunctuation)
DLSelsevierTitlesC <- tm_map(DLSelsevierTitlesC, tolower)
DLSelsevierTitlesC <- tm_map(DLSelsevierTitlesC, removeWords, stopwords("english"))
DLSelsevierTitlesC <- tm_map(DLSelsevierTitlesC, stemDocument)
DLSelsevierTitlesC <- tm_map(DLSelsevierTitlesC, stripWhitespace)
DLSelsevierTitlesC <- tm_map(DLSelsevierTitlesC, PlainTextDocument)
#analyze
DLSeT.dtm<-DocumentTermMatrix(DLSelsevierTitlesC)
}

dtm<-tmpreproc(DLSelsevierTitles)

#split titles into words
#strsplit(DLSelsevierTitles," ")
#word frequency table sorted smallest to largest
#sort(table(unlist(strsplit(DLSelsevierTitles," "))))




#https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html




dtms <- removeSparseTerms(dtm, 0.75) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="ward.D2")   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") 

### K-means clustering   
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.98) # Prepare the data (max 2% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 

<><><><><><><><><><><><><><><>

DLSelsevierDOIs<-lapply(DLSelsevier,function(x) unlist(xpathApply(htmlTreeParse(x,useInternalNode=T),"//doi",xmlValue)))
DLSelsevierDOIs<-unlist(DLSelsevierDOIs)
r2<-getURL(paste0("api.elsevier.com/content/article/doi/",DLSelsevierDOIs,"?apiKey=ba0f6a67c909e5cd1e2dae428bb2b94d"))
DLSelsevierAbs<-xpathApply(htmlTreeParse(r2,useInternalNode=T),"//description",xmlValue)
write.csv(DLSelsevierAbs,"C:\\Users\\Matthew S. MacLennan\\Desktop\\DLSelsevierAbs.csv")
#proceed into the workup codes used above.



dtm<-tmpreproc(DLSelsevierAbs)

dtms <- removeSparseTerms(dtm, 0.75) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="ward.D2")   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") 

### K-means clustering   
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.9) # Prepare the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0) 



########################################

#Query results per year
datesy<-seq(1900,2016,1)
library(XML);library(RCurl);
bobl<-as.list(getURL(paste0("api.elsevier.com/content/search/scidir?apiKey=ba0f6a67c909e5cd1e2dae428bb2b94d&query=TITLE-ABS-KEY%28dynamic+light+scattering%29&date=",datesy,"&httpAccept=%20application%2Fxml&start=0&count=100")))
boblp<-lapply(bobl,function(x) xmlTreeParse(x,useInternalNode=T))
totalresults<-lapply(boblp,function(x) xpathApply(x,"//opensearch:totalResults/text() div 1"))
#totalresults<-xpathApply(xmlTreeParse(getURL(paste0("api.elsevier.com/content/search/scidir?apiKey=ba0f6a67c909e5cd1e2dae428bb2b94d&query=TITLE-ABS-KEY%28dynamic+light+scattering%29&date=",datesy,"&httpAccept=%20application%2Fxml&start=0&count=100")),useInternalNode=T),"//opensearch:totalResults/text() div 1")
plot(unlist(totalresults)~datesy,type="h",xlab="Year",xlim=c(1900,2016))
title(main="Yearly Sciencedirect articles featuring\n'Dynamic Light Scattering'\nin title-abstract-keywords")
write.csv(cbind(unlist(totalresults),datesy),"C:\\Users\\Matthew S. MacLennan\\Desktop\\totalresultsDLS.csv")

barplot(unlist(totalresults),xlab="Year",width=3,names.arg=datesy,ylim=c(0,800))
title(main="Yearly Sciencedirect articles featuring\n'Dynamic Light Scattering'\nin title-abstract-keywords")

##########################################

byyear<-function(keyword){
#Query results per year
datesy<-seq(1900,2016,1)
library(XML);library(RCurl);
bobl<-as.list(getURL(paste0("api.elsevier.com/content/search/scidir?apiKey=ba0f6a67c909e5cd1e2dae428bb2b94d&query=TITLE-ABS-KEY%28",keyword,"%29&date=",datesy,"&httpAccept=%20application%2Fxml&start=0&count=100")))
boblp<-lapply(bobl,function(x) xmlTreeParse(x,useInternalNode=T))
totalresults<-lapply(boblp,function(x) xpathApply(x,"//opensearch:totalResults/text() div 1"))
totalresults<-cbind(unlist(totalresults),datesy)
list1<-list(bobl,totalresults)
}

#customize the following for plotting

barplot(unlist(totalresults),xlab="Year",width=3,names.arg=datesy,ylim=c(0,800))
title(main="Yearly Sciencedirect articles featuring\n'Dynamic Light Scattering'\nin title-abstract-keywords")



###########################################

#find unique unstemmed words with "nano" in them, where dlstits is the dls search results of titles.
unique(unlist(regmatches(dlstits[,2],gregexpr("nano\\w+",dlstits[,2]))))

#what kinds of nanoparticles (by seeing previous word)
unlist(regmatches(dlstits[,2],gregexpr("\\w+ nanoparticle",dlstits[,2])))

#or nano-
unlist(regmatches(dlstits[,2],gregexpr("\\w+ nano\\w+",dlstits[,2])))

#or protein

#graph object containing particle types
unlist(regmatches(dlstits[,2],gregexpr("\\w+ nano\\w+",dlstits[,2])))
#stem ending "s"
stemd<-gsub("s$","",unlist(regmatches(dlstits[,2],gregexpr("\\w+ nano\\w+",dlstits[,2]))))
#split and into matrix
stemdi<-matrix(unlist(strsplit(stemd," ")),ncol=2,byrow=T)
#igraph-ready!
library(igraph)
stemdig<-graph.edgelist(stemdi)
#only nanoparticle
table(stemdi[which(stemdi[,2]=="nanoparticle"),1])
stemdi2<-graph.edgelist(stemdi[which(stemdi[,2]=="nanoparticle"),])
#Make graph with small nodes and no outer labels


#############################################

#finding "dynamic light scattering" (in titles file) and substituting it with "dynamic_light_scattering"

#Repeating the package::tm commands from above.

##############################################

#Jaccard code for "dynamic light scattering" and "protein"


################################################
#Load large csv file
library(data.table)
dlsabs<-fread("C:\\Users\\Matthew S. MacLennan\\Desktop\\DLSelsevierAbs.csv",header=F,sep=",")
dlsabs<-t(as.matrix(dlsabs))
#Unique startwords in the abstracts based on first 8 characters
begin<-unique(unlist(regmatches(dlsabs,gregexpr("^\\w{8}",dlsabs))))
#Analyze the results by eye and there shouldn't be too many different types.
begin<-"[Aa]bstract|ABSTRACT|Purpose|Summary|Background"
dlsabs<-gsub(begin,"",dlsabs)

################################################
Jacc<-function(data,w1,w2){
#abstracts containing w1
pn<-(regmatches(data,gregexpr(w1,data)))
pabs<-data[unlist(lapply(pn,function(x) length(x)>0))]
#abs containing w2
np<-(regmatches(data,gregexpr(w2,data)))
nabs<-data[unlist(lapply(np,function(x) length(x)>0))]
#Union
pnnp<-(regmatches(pabs,gregexpr(w2,pabs)))
pnnpabs<-data[unlist(lapply(pnnp,function(x) length(x)>0))]
#Lengths = hits
#lapply(list(nabs,pabs,pnnpabs),length)
#Jaccard metric for protein and nanoparticle
length(pnnpabs)/(length(nabs)+length(pabs))
}
#############################################################
artfreq<-function(data,w1){
pn<-(regmatches(data,gregexpr(w1,data)))
pabs<-data[unlist(lapply(pn,function(x) length(x)>0))]
length(pabs)/length(data)
}
#############################################################
Jacconlist<-function(data,L1,L2){
	a<-matrix(c(0,0,0),ncol=3,byrow=T)
	for(i in 1:length(L1)){
		for(j in 1:length(L2)){
	a<-rbind(a,c(L1[i],L2[j],Jacc(data,L1[i],L2[j])))
		}
	a<-a
	}
	a<-a[-1,]
	print(a)
}
##############################################################

#simple arrangement of term frequency in abstracts. dlsabsdls is identical to dlsabs, except that the
#term "dynamic light scattering" has been changed to "dls".
image(matrix(unlist(lapply(regmatches(dlsabsdls,gregexpr("protein",dlsabsdls)),length)),ncol=57,byrow=T))

###############################################################

#Create term-term adjacency matrix
ttms<- t(as.matrix(dtms)) %*% as.matrix(dtms)
#ICA on ttms (5 components)
library(fastICA)
ttmsica3<-fastICA(ttms,n.comp=3)
names(sort(ttmsica3$S[,1],decreasing=T))[1:10]
library(wordcloud)
wordcloud(names(ttmsica3$S[,1]),freq=ttmsica3$S[,1],random.order=F,random.color=T,min.freq=-7)
wordcloud(names(ttmsica3$S[,2]),freq=ttmsica3$S[,2],random.order=F,random.color=T,min.freq=-7)
wordcloud(names(ttmsica3$S[,3]),freq=ttmsica3$S[,3],random.order=F,random.color=T,min.freq=-7)
