 install.packages('httr')
 install.packages('rvest')
 install.packages('tidyverse')

 library(httr)
 library(rvest)
 library(tidyverse)

 #로그인 페이지
 login <- 'https://www.jobplanet.co.kr/users/sign_in'
 
 # 로그인 정보를 입력하고 HTTP 요청 
 resp <- POST(url = login,
              body = list('user[email]' = '',
                          'user[password]' = ''))
 
 # 응답 상태코드를 확인합니다. 200이면 정상입니다.
 status_code(x = resp)

 #크롤링을 원하는 회사명 입력
compNm <- ''


# CSS Selector로 텍스트만 수집하는 함수를 생성
getHtmlText <- function(x, css) {
  
  result <- x %>% 
    html_node(css = css) %>% 
    html_text()
  
  return(result)
}


# CSS Selector로 별점을 수집하는 함수를 생성
getHtmlRate <- function(x, css, name) {
  
  result <- x %>% 
    html_node(css = css) %>% 
    html_attr(name = name) %>% 
    str_remove_all(pattern = '(width:)|(%;)') %>% 
    as.numeric()
  
  return(result)
}

# 개별 기업리뷰를 수집하고 데이터 프레임으로 반환하는 함수를 생성
getData <- function(x) {
  
  # 기업리뷰를 포함하는 HTML 요소를 추출하여 items 객체에 할당
  items <- x %>% read_html() %>% html_nodes(css = 'section.content_ty4')
  
  # 웹 데이터를 수집하여 df 객체에 할당 
  df <- 
    data.frame(
      회사이름 = x %>% read_html() %>% html_node(css = 'h1.name') %>% html_text(),
      회사코드 = items %>% html_attr(name = 'data-company_id'),
      리뷰코드 = items %>% html_attr(name = 'data-content_id'),
      직종구분 = getHtmlText(x = items, css = 'div.content_top_ty2 span:nth-child(2)'),
      재직상태 = getHtmlText(x = items, css = 'div.content_top_ty2 span:nth-child(4)'),
      근무지역 = getHtmlText(x = items, css = 'div.content_top_ty2 span:nth-child(6)'),
      등록일자 = getHtmlText(x = items, css = 'div.content_top_ty2 span.txt2'),
      별점평가 = getHtmlRate(x = items, css = 'div.star_score', name = 'style'),
      승진기회 = getHtmlRate(x = items, css = 'dl dd:nth-child(3) div div', name = 'style'),
      복지급여 = getHtmlRate(x = items, css = 'dl dd:nth-child(5) div div', name = 'style'),
      워라밸   = getHtmlRate(x = items, css = 'dl dd:nth-child(7) div div', name = 'style'),
      사내문화 = getHtmlRate(x = items, css = 'dl dd:nth-child(9) div div', name = 'style'),
      경영진   = getHtmlRate(x = items, css = 'dl dd:nth-child(11) div div', name = 'style'),
      기업장점 = getHtmlText(x = items, css = 'dl dd:nth-child(2) span'),
      기업단점 = getHtmlText(x = items, css = 'dl dd:nth-child(4) span'),
      바라는점 = getHtmlText(x = items, css = 'dl dd:nth-child(6) span'),
      성장예상 = getHtmlText(x = items, css = 'p.etc_box strong'),
      추천여부 = getHtmlText(x = items, css = 'p.txt.recommend.etc_box')
    )
  
  return(df)
}

# 총 페이지 수 계산
pages <- ceiling(x = reviewCnt / 5)#한페이지당 5개 나옴
print(x = pages)

uri <-'https://www.jobplanet.co.kr/companies/41466/reviews/%EB%86%8D%ED%98%91%EC%9D%80%ED%96%89'
resp <- GET(url = uri)
# 결과를 저장할 객체 생성
result <- getData(x = resp)

#모든 페이지 크롤링
for(i in 2:pages)
{
  uri=paste0('원하는 기업의 잡플래닛 url page='
             ,toString(i),'&review_type=')
  resp <- GET(url = uri)
  tmp<-getData(x=resp)
  result<-rbind(result, tmp)
  #print(uri)
}


print(x = result)

# 리뷰코드로 중복여부를 확인
duplicated(x = result$리뷰코드) %>% sum()

# 최종 데이터 저장
write.csv(result, 'result.csv')
