getGeoCode<-function(siDo = "", siGu = "", gu= "", dong ="", road = "", building_code = "", building_second_code = ""){
  con <- dbConnect(RSQLite::SQLite(), dbname="./address.sqlite")

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
  building_code <- cleanVal(building_code)
  building_second_code <- cleanVal(building_second_code)

  candiQueries <- data.frame(query = c(
    buildQuery(siDo, siGu, gu, dong, road, building_code, building_second_code),
    buildQuery(siDo, siGu, gu, dong, road, '', ''),
    buildQuery(siDo, siGu,'', dong, road, building_code, building_second_code),
    buildQuery(siDo, siGu,'', dong, road, '', ''),
    buildQuery(siDo, '', gu, dong, road, building_code, building_second_code),
    buildQuery(siDo, '', gu, dong, road,'',''),
    buildQuery(siDo, '', '', dong, road, building_code, building_second_code),
    buildQuery(siDo, '', '', dong, road,'',''),
    buildQuery(siDo, '', '', '', road, building_code, building_second_code),
    buildQuery(siDo, '', '', '', road,'',''),
    buildQuery(siDo, siGu, gu, '', road, building_code, building_second_code),
    buildQuery(siDo, siGu, gu, '', road,'',''),
    buildQuery(siDo, siGu, '', '', road, building_code, building_second_code),
    buildQuery(siDo, siGu, '', '', road,'',''),
    buildQuery(siDo, '', gu, '', road, building_code, building_second_code),
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
    if(nrow(geoCodes) == 0) next
    ss<-geoCodes %>% filter(is.numeric(X) & is.numeric(Y)) %>% summarise(avgX = mean(X), avgY = mean(Y))
    return(data.frame(E = ss$avgX, N = ss$avgY))
  }
  return(data.frame(E = NA, N = NA))
}
