buildQuery <- function(siDo = "", siGu = "", gu= "", dong ="", road= "", buildRepNum = "", buildLeapNum = ""){
  fullBuildInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and si_gun_nm = '$siGu'
  and gu_nm = '$gu'
  and dong_nm = '$dong'
  and road_nm = '$road'
  and build_rep_num = '$buildRepNum'
  and build_leap_num = '$buildLeapNum';"
  fullInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and si_gun_nm = '$siGu'
  and gu_nm = '$gu'
  and dong_nm = '$dong'
  and road_nm = '$road';"
  siDoSiGuDongRoadBuildQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and si_gun_nm = '$siGu'
  and dong_nm = '$dong'
  and road_nm = '$road'
  and build_rep_num = '$buildRepNum'
  and build_leap_num = '$buildLeapNum';"
  siDoSiGuDongRoadQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and si_gun_nm = '$siGu'
  and dong_nm = '$dong'
  and road_nm = '$road';"
  siDoGuDongRoadBuildQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and gu_nm = '$gu'
  and dong_nm = '$dong'
  and road_nm = '$road'
  and build_rep_num = '$buildRepNum'
  and build_leap_num = '$buildLeapNum';"
  siDoGuDongRoadQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and gu_nm = '$gu'
  and dong_nm = '$dong'
  and road_nm = '$road';"
  dongRoadBuildInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and dong_nm = '$dong'
  and road_nm = '$road'
  and build_rep_num = '$buildRepNum'
  and build_leap_num = '$buildLeapNum';"
  dongRoadInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and dong_nm = '$dong'
  and road_nm = '$road';"
  siGuDongInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and si_gun_nm = '$siGu'
  and dong_nm = '$dong';"
  siGuInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and si_gun_nm = '$siGu';"
  siGunGuInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and si_gun_nm = '$siGu'
  and gu_nm = '$gu';"
  guInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and gu_nm = '$gu';"
  dongInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and dong_nm = '$dong';"
  roadBuildInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and road_nm = '$road'
  and build_rep_num = '$buildRepNum'
  and build_leap_num = '$buildLeapNum';"
  roadInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and road_nm = '$road';"
  roadBuildInputQuery <- "select * from geo_code
  where si_do_nm = '$siDo'
  and road_nm = '$road'
  and build_rep_num = '$buildRepNum'
  and build_leap_num = '$buildLeapNum'"

  # deal with sido naming diversity
  siDo <- toSido(siDo)
  road<- gsub(" ","", road)
  # defense logic
  if(buildRepNum != "" & buildLeapNum == ""){
    buildLeapNum = "0"
  }

  query <- ""
  if(siDo != "" & siGu != "" & gu != "" & dong != "" & road!= "" & buildRepNum != "" & buildLeapNum != ""){
    query <- fullBuildInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$gu',gu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$buildRepNum',buildRepNum,query)
    query <- gsub('\\$buildLeapNum',buildLeapNum,query)
  } else if(siDo != "" & siGu != "" & gu != "" & dong != "" & road!= ""){
    query <- fullInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$gu',gu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & siGu != "" & dong != "" & road!= "" & buildRepNum != "" & buildLeapNum != ""){
    query <- siDoSiGuDongRoadBuildQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$buildRepNum',buildRepNum,query)
    query <- gsub('\\$buildLeapNum',buildLeapNum,query)
  } else if(siDo != "" & siGu != "" & dong != "" & road!= ""){
    query <- siDoSiGunDongRoadQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & gu != "" & dong != "" & road!= "" & buildRepNum != "" & buildLeapNum != ""){
    query <- siDoGuDongRoadBuildQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$gu',gu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$buildRepNum',buildRepNum,query)
    query <- gsub('\\$buildLeapNum',buildLeapNum,query)
  } else if(siDo != "" & gu != "" & dong != "" & road!= ""){
    query <- siDoGuDongRoadQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$gu',gu,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & dong != "" & road!= "" & buildRepNum != "" & buildLeapNum != ""){
    query <- dongRoadBuildInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$buildRepNum',buildRepNum,query)
    query <- gsub('\\$buildLeapNum',buildLeapNum,query)
  } else if(siDo != "" & dong != "" & road!= ""){
    query <- dongRoadInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$dong',dong,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & road != "" & buildRepNum != "" & buildLeapNum != ""){
    query <- roadBuildInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$road',road,query)
    query <- gsub('\\$buildRepNum',buildRepNum,query)
    query <- gsub('\\$buildLeapNum',buildLeapNum,query)
  } else if(siDo != "" & road != ""){
    query <- roadInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$road',road,query)
  } else if(siDo != "" & siGu != "" & gu != "" & dong != ""){
    query <- siGuDongInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$dong',dong,query)
  } else if(siDo != "" & siGu != "" & dong != ""){
    query <- siGuDongInputQuery
    query <- gsub('\\$siDo',siDo,query)
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$dong',dong,query)
  } else if(siDo != "" & gu != "" & dong != ""){
    query <- siGuDongInputQuery
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
    query <- gsub('\\$siGu',siGu,query)
    query <- gsub('\\$gu',gu,query)
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
