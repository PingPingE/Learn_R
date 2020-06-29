install.packages("rvest")

install.packages("httr")

install.packages("stringr")

install.packages("RSelenium")

library(rvest)

library(RSelenium)

library(httr)

library(stringr)

ch=wdman::chrome(port= 포트번호 ex)4445L) #크롬드라이버를 포트

remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open() #크롬 Open

URL<-"원하는 앱의 url"+"&showAllReviews=true"

remDr$navigate(URL) #설정 url로 이동
webElem <- remDr$findElement("css", "body")

webElem$sendKeysToElement(list(key = "end"))
flag <- TRUE

endCnt <- 0

while (flag) {
  
  Sys.sleep(10)
  
  webElemButton <- remDr$findElements(using = 'css selector',value = '.ZFr60d.CeoRYc')
  
  
  
  if(length(webElemButton)==1){
    
    endCnt <- 0
    
    webElem$sendKeysToElement(list(key = "home"))
    
    webElemButton <- remDr$findElements(using = 'css selector',value = '.ZFr60d.CeoRYc')
    
    remDr$mouseMoveToLocation(webElement = webElemButton[[1]]) #해당 버튼으로 포인터 이동
    
    remDr$click()
    
    webElem$sendKeysToElement(list(key = "end"))
    
    flag <- FALSE #추가한 부분
    
  }else{
    
    if(endCnt>3){
      
      flag <- FALSE
      
    }else{
      
      endCnt <- endCnt + 1
      
    }
    
  }
  
}

frontPage <- remDr$getPageSource() #페이지 전체 소스 가져오기

reviewNames <- read_html(frontPage[[1]]) %>% html_nodes('.bAhLNe.kx8XBd') %>% html_nodes('.X43Kjb') %>%  html_text() #페이지 전체 소스에서 리뷰 정보(이름, 날짜) 부분 추출하기 

reviewDates <- read_html(frontPage[[1]]) %>% html_nodes('.bAhLNe.kx8XBd') %>% html_nodes('.p2TkOb') %>%  html_text() #페이지 전체 소스에서 리뷰 정보(이름, 날짜) 부분 추출하기 

reviewComments <- read_html(frontPage[[1]]) %>% html_nodes('.UD7Dzf') %>%  html_text() #페이지 전체 소스에서 리뷰 정보(이름, 날짜) 부분 추출하기 

reviewData <- data.frame(name=reviewNames, date=reviewDates, comment=reviewComments)

write.csv(reviewData, paste0("net.bloodinfo.smartapp(",nrow(reviewData),").csv"))

remDr$close()
