source('./properties.R')

getGeoCode<-function(siDo = "", siGu = "", gu= "", dong ="", road = "", buildRepNum = "", buildingLeapNum = ""){
  con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)

  cleanVal <- function(val){
    val <- trimws(val)
    if(is.null(val)){
      return("")
    }
    if(is.na(val)){
      return("")
    }
    if(val == "?"){
      return("")
    } else {
      return(val)
    }
  }

  siDo <- cleanVal(siDo)
  siGu <- cleanVal(siGu)
  gu <- cleanVal(gu)
  dong <- cleanVal(dong)
  road <- cleanVal(road)
  buildRepNum <- cleanVal(buildRepNum)
  buildingLeapNum <- cleanVal(buildingLeapNum)

  candiQueries <- data.frame(
    query = c(
      buildQuery(siDo, siGu, gu, dong, road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, siGu, gu, dong, road, '', ''),
      buildQuery(siDo, siGu,'', dong, road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, siGu,'', dong, road, '', ''),
      buildQuery(siDo, '', gu, dong, road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, '', gu, dong, road,'',''),
      buildQuery(siDo, '', '', dong, road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, '', '', dong, road,'',''),
      buildQuery(siDo, '', '', '', road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, '', '', '', road,'',''),
      buildQuery(siDo, siGu, gu, '', road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, siGu, gu, '', road,'',''),
      buildQuery(siDo, siGu, '', '', road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, siGu, '', '', road,'',''),
      buildQuery(siDo, '', gu, '', road, buildRepNum, buildingLeapNum),
      buildQuery(siDo, '', gu, '', road,'',''),
      buildQuery(siDo, siGu, gu, dong, '','',''),
      buildQuery(siDo, siGu, '', dong, '','',''),
      buildQuery(siDo, '', gu, dong, '','',''),
      buildQuery(siDo, '', '', dong, '','',''),
      buildQuery(siDo, siGu, gu, '', '','',''),
      buildQuery(siDo, siGu, '', '', '','',''),
      buildQuery(siDo, '', gu, '', '','','')
    )
  ) %>% filter(query != "") %>% unique()
  for(query in candiQueries$query){
    geoCodes<-dbGetQuery(con, query)
    print(query)
    if(nrow(geoCodes) == 0) next
    print(query)
    ss<-geoCodes %>% filter(is.numeric(e) & is.numeric(n)) %>% summarise(avgE = mean(e), avgN = mean(n))
    return(data.frame(e = ss$avgE, n = ss$avgN))
  }
  return(data.frame(e = NA, n = NA))
}
