#Jaccard scraper for scopus
scscj<-function(apikey,query1,query2){
  listOR<-c()
  for(i in 1:length(query1)){
    for(j in 1:length(query2)){
      listOR<-c(listOR,as.numeric(xpathApply(htmlTreeParse(getURL(paste0("http://api.elsevier.com/content/search/scopus/?apiKey=",apikey,"&query=",query1[[i]]"+OR+",qu,"&field=doi&httpAccept=%20application%2Fatom%2Bxml&count=1&start=0")),
        useInternalNode=T),"//totalresults",xmlValue)))
    }
  }
#
  listAND<-c()
  for(i in 1:length(query2)){
    list2<-c(list2,as.numeric(xpathApply(htmlTreeParse(getURL(paste0("http://api.elsevier.com/content/search/scopus/?apiKey=",apikey,"&query=",query2[[i]],"&field=doi&httpAccept=%20application%2Fatom%2Bxml&count=1&start=0")),
      useInternalNode=T),"//totalresults",xmlValue)))
  }

}


#Example: Jaccard search of dynamic light scattering versus its synonyms.
syn<-unlist(strsplit(unlist(xpathApply(CMO.owl,"//class/label[.='dynamic light scattering']/ancestor::class/hasexactsynonym",xmlValue)),
      "\n +"))
query2<-paste0("%22",gsub(" ","+",syn),"%22")
query1<-"%22dynamic+light+scattering%22"
