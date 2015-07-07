#Jaccard scraper for scopus
scscj<-function(apikey,query1,query2){
  list1<-c()
  for(i in 1:length(query1)){
    list1<-c(list1,as.numeric(xpathApply(htmlTreeParse(getURL(paste0("http://api.elsevier.com/content/search/scopus/?apiKey=",apikey,"&query=",query1[[i]],"&field=doi&httpAccept=%20application%2Fatom%2Bxml&count=1&start=0")),
      useInternalNode=T),"//totalresults",xmlValue)))
  }
  list1
#
  list2<-c()
  for(i in 1:length(query2)){
    list2<-c(list2,as.numeric(xpathApply(htmlTreeParse(getURL(paste0("http://api.elsevier.com/content/search/scopus/?apiKey=",apikey,"&query=",query2[[i]],"&field=doi&httpAccept=%20application%2Fatom%2Bxml&count=1&start=0")),
      useInternalNode=T),"//totalresults",xmlValue)))
  }
#


}
