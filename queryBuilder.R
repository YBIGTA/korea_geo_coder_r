buildQuery <- function(siDo = "", siGu = "", gu= "", dong ="", road = "", building_code = "", building_second_code = ""){
  fullBuildingInputQuery <- "select * from address
  where si_do = '$siDo'
  and si_gu = '$siGu'
  and dong = '$dong'
  and road = '$road'
  and building_code = '$building_code'
  and building_second_code = '$building_second_code';"
  fullInputQuery <- "select * from address
  where si_do = '$siDo'
  and si_gu = '$siGu'
  and dong = '$dong'
  and road = '$road';"
  dongRoadInputQuery <- "select * from address
  where si_do = '$siDo'
  and dong = '$dong'
  and road = '$road';"
  dongRoadBuildingInputQuery <- "select * from address
  where si_do = '$siDo'
  and dong = '$dong'
  and road = '$road'
  and building_code = '$building_code'
  and building_second_code = '$building_second_code';"
  siGudongInputQuery <- "select * from address
  where si_do = '$siDo'
  and si_gu = '$siGu'
  and dong = '$dong';"
  siGuInputQuery <- "select * from address
  where si_do = '$siDo'
  and si_gu = '$siGu';"
  dongInputQuery <- "select * from address
  where si_do = '$siDo'
  and dong = '$dong';"

  roadInputQuery <- "select * from address
  where si_do = '$siDo'
  and road = '$road';"
  roadBuildingInputQuery <- "select * from address
  where si_do = '$siDo'
  and road = '$road'
  and building_code = '$building_code'
  and building_second_code = '$building_second_code';"

  # deal with sido naming diversity
  siDo <- toSido(siDo)
  road <- gsub(" ","", road)
  # defense logic
  if(building_code != "" & building_second_code == ""){
    building_second_code = "0"
  }

  query <- ""
  if(siDo != "" & siGu != "" & gu != "" & dong != "" & road != "" & building_code != "" & building_second_code != ""){
    query <- fullBuildingInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',paste(siGu, gu, sep = " "),query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$building_code',building_code,query)
    query <- gsub('\\$building_second_code',building_second_code,query)
  } else if(siDo != "" & siGu != "" & gu != "" & dong != "" & road != ""){
    query <- fullInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',paste(siGu, gu, sep = " "),query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & siGu != "" & dong != "" & road != "" & building_code != "" & building_second_code != ""){
    query <- fullBuildingInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$building_code',building_code,query)
    query <- gsub('\\$building_second_code',building_second_code,query)
  } else if(siDo != "" & siGu != "" & dong != "" & road != ""){
    query <- fullInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & gu != "" & dong != "" & road != "" & building_code != "" & building_second_code != ""){
    query <- fullBuildingInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',gu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$building_code',building_code,query)
    query <- gsub('\\$building_second_code',building_second_code,query)
  } else if(siDo != "" & gu != "" & dong != "" & road != ""){
    query <- fullInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',gu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & dong != "" & road != "" & building_code != "" & building_second_code != ""){
    query <- dongRoadBuildingInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$building_code',building_code,query)
    query <- gsub('\\$building_second_code',building_second_code,query)
  }else if(siDo != "" & dong != "" & road != ""){
    query <- dongRoadInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & road!= ""){
    query <- roadBuildingInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$building_code',building_code,query)
    query <- gsub('\\$building_second_code',building_second_code,query)
  } else if(siDo != "" & road!= ""){
    query <- roadInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & siGu != "" & gu != "" & dong != ""){
    query <- siGudongInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',paste(siGu, gu, sep = " "),query)
    query <- gsub('\\$dong',dong,query)
  } else if(siDo != "" & siGu != "" & dong != ""){
    query <- siGudongInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$dong',dong,query)
  } else if(siDo != "" & gu != "" & dong != ""){
    query <- siGudongInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',gu,query)
    query <- gsub('\\$dong',dong,query)
  } else if(siDo != "" & dong != ""){
    query <- dongInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$dong',dong,query)
  } else if(siDo != "" & siGu != "" & gu != ""){
    query <- siGuInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',paste(siGu, gu, sep = " "),query)
  } else if(siDo != "" & siGu != ""){
    query <- siGuInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
  } else if(siDo != "" & gu != ""){
    query <- siGuInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',gu,query)
  }
  return(query)

}
