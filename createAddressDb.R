#### http://www.juso.go.kr/addrlink/addressBuildDevNew.do?menu=geodata
#### http://www.juso.go.kr/dn.do?boardId=GEODATA&fileName=%EA%B3%B5%EA%B0%84%EC%A0%95%EB%B3%B4%EC%9A%94%EC%95%BDDB_7%EC%9B%94%EB%B6%84.zip&realFileName=ENTRC_DB_1707.zip&regYmd=2017&num=15&fileNo=77353&logging=Y


loadOrInstall<-function(packageName = ""){
  if(!require(packageName, character.only = TRUE)){
    install.packages(packageName)
    require(packageName, character.only = TRUE)
  }
}

loadOrInstall('RSQLite')
loadOrInstall('dplyr')
loadOrInstall('data.table')
loadOrInstall('curl')

source('./dbConnector.R')

createTableQuery <- "
create table geo_code (
  si_do_nm text,
  si_gun_nm text,
  gu_nm text,
  dong_nm text,
  road_nm text,
  build_rep_num integer,
  build_leap_num integer,
  e decimal(15,6),
  n decimal(15,6)
  )"

con <- getConnection()
dbSendQuery(con, 'drop table geo_code')
dbSendQuery(con, createTableQuery)

address2GeoSourcePath <- 'http://www.juso.go.kr/dn.do?boardId=GEODATA&fileName=%EA%B3%B5%EA%B0%84%EC%A0%95%EB%B3%B4%EC%9A%94%EC%95%BDDB_7%EC%9B%94%EB%B6%84.zip&realFileName=ENTRC_DB_1707.zip&regYmd=2017&num=15&fileNo=77353&logging=Y'

address2GeoSourceZip <- './address2GeoSource.zip'

curl_download(address2GeoSourcePath, address2GeoSourceZip)

address2GeoSourceDir <-'./address2GeoTemp'
unzip(address2GeoSourceZip, exdir = address2GeoSourceDir)

sources<- list.files(address2GeoSourceDir)

dataInsertQuery <- "insert into geo_code (si_do_nm, si_gun_nm, gu_nm, dong_nm, road_nm, build_rep_num, build_leap_num, e, n ) values "
valuesForamt <- " ('$si_do_nm', '$si_gun_nm', '$gu_nm', '$dong_nm', '$road_nm', $build_rep_num ,$build_leap_num, $e, $n)"

for(file in sources){
  if(!endsWith(file, 'txt')) next
  source<-read.csv(paste(address2GeoSourceDir,"/",file, sep =""), sep ='|', header = FALSE, fileEncoding='euc-kr')
  names(source) <- c('si_gun_code','entrance_serial','law_dong_cd','si_do_nm','si_gun_nm','dong_nm','road_cd',
  'road_nm','is_base','build_rep_num','build_leap_num','build_nm','post_cd','build_usage_type','build_type','public_dong','e','n')

  chunkSize = 200
  print(paste(file,'start to insert data'))
  source<-as.data.table(source)
  for(i in seq(1, as.integer(nrow(source) / chunkSize) + 1)){
    startIdx <- (i - 1) * chunkSize
    endIdx <- min(i * chunkSize, nrow(source))
    values<-apply(source[startIdx:endIdx],1,function(x){
      si_gu <- ""
      gu_nm <- ""

      si_gu_nm <- x['si_gun_nm']

      if(!is.na(si_gu_nm)) {
        tok<-unlist(strsplit(si_gu_nm," "))

        if(length(tok) == 2){
          si_gu <- tok[1]
          gu_nm <- tok[2]
        } else if(length(tok) == 1 ){
          si_gu <- si_gu_nm
        }
      }
      q<-valuesForamt
      q<-gsub('\\$si_do_nm',x['si_do_nm'],q)
      q<-gsub('\\$si_gun_nm',si_gu,q)
      q<-gsub('\\$gu_nm',gu_nm,q)
      q<-gsub('\\$dong_nm',x['dong_nm'],q)
      q<-gsub('\\$road_nm',x['road_nm'],q)
      q<-gsub('\\$build_rep_num',x['build_rep_num'],q)
      q<-gsub('\\$build_leap_num',x['build_leap_num'],q)
      q<-gsub('\\$e',x['e'],q)
      q<-gsub('\\$n',x['n'],q)
      return(q)
      })

    values<-values[!is.na(values)]
    q<-paste(dataInsertQuery, paste(values, collapse = ","), collapse = " ")
    dbSendQuery(con, q)
  }
}

dbSendQuery(con, "create index idx_full_idx on geo_code (si_do_nm, si_gun_nm, gu_nm, dong_nm, road_nm, build_rep_num, build_leap_num)")
dbSendQuery(con, "create index idx_si_do_nm_si_gu_nm_dong_nm_road_nm_build_rep_num_build_leap_num on geo_code (si_do_nm, si_gun_nm, dong_nm, road_nm, build_rep_num, build_leap_num)")
dbSendQuery(con, "create index idx_si_do_nm_si_gu_nm_dong_nm_road_nm on geo_code (si_do_nm, si_gun_nm, dong_nm, road_nm)")
dbSendQuery(con, "create index idx_si_do_nm_gu_nm_dong_nm_road_nm_build_rep_num_build_leap_num on geo_code (si_do_nm, gu_nm, dong_nm, road_nm, build_rep_num, build_leap_num)")
dbSendQuery(con, "create index idx_si_do_nm_gu_nm_dong_nm_road_nm on geo_code (si_do_nm, gu_nm, dong_nm, road_nm)")
dbSendQuery(con, "create index idx_si_do_nm_dong_nm_road_nm_build_rep_num_build_leap_num on geo_code (si_do_nm, dong_nm, road_nm, build_rep_num, build_leap_num)")
dbSendQuery(con, "create index idx_si_do_nm_dong_nm_road_nm on geo_code (si_do_nm, dong_nm, road_nm)")
dbSendQuery(con, "create index idx_si_do_nm_road_nm_build_rep_num_build_leap_num on geo_code (si_do_nm,road_nm, build_rep_num, build_leap_num)")
dbSendQuery(con, "create index idx_si_do_nm_road_nm on geo_code (si_do_nm, dong_nm, road_nm)")
dbSendQuery(con, "create index idx_si_do_nm_si_gu_nm_road_nm_build_rep_num_build_leap_num on geo_code (si_do_nm, si_gun_nm, road_nm, build_rep_num, build_leap_num)")
dbSendQuery(con, "create index idx_si_do_nm_si_gu_nm_road_nm on geo_code (si_do_nm, si_gun_nm, road_nm)")
dbSendQuery(con, "create index idx_si_do_nm_gu_nm_road_nm_build_rep_num_build_leap_num on geo_code (si_do_nm, gu_nm, road_nm, build_rep_num, build_leap_num)")
dbSendQuery(con, "create index idx_si_do_nm_gu_nm_road_nm on geo_code (si_do_nm, gu_nm, road_nm)")

unlink(address2GeoSourceDir, recursive =TRUE)
unlink(address2GeoSourceZip, recursive =TRUE)
