toSido <- function(siDoShorcut) {
  if(siDoShorcut == '서울'){
    return('서울특별시')
  } else if (siDoShorcut == '대전'){
    return('대전광역시')
  } else if (siDoShorcut == '울산'){
    return('울산광역시')
  } else if (siDoShorcut == '대구'){
    return('대구광역시')
  } else if(siDoShorcut == '부산'){
    return('부산광역시')
  } else if(siDoShorcut == '인천'){
    return('인천광역시')
  } else if (siDoShorcut == '제주'){
    return('제주특별자치도')
  } else if (siDoShorcut == '세종'){
    return('세종특별시')
  } else if (siDoShorcut == '광주'){
    return('광주광역시')
  } else if (siDoShorcut == '경기'){
    return('경기도')
  } else if (siDoShorcut == '강원'){
    return('강원도')
  } else if (siDoShorcut == '충북'){
    return('충청북도')
  } else if (siDoShorcut == '충남'){
    return('충청남도')
  } else if (siDoShorcut == '전북'){
    return('전라북도')
  } else if (siDoShorcut == '전남'){
    return('전라남도')
  } else if (siDoShorcut == '경북'){
    return('경상북도')
  } else if (siDoShorcut == '경남'){
    return('경상남도')
  } else {
    return(siDoShorcut)
  }
}
