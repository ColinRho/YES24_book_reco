suppressPackageStartupMessages({
  
  library(dplyr)
  library(data.table)
  library(magrittr)
  
})


###-0 data manipulation
 
# raw data
raw <- fread("data/Training Data Set_yes24.csv", encoding = "unknown")

# target categories
cats <- c("인문", "자기계발", "국내문학", "해외문학", "종교") 

# 주문, 취소, 환분
raw %>% select(구분) %>% table()

# 취소와 환불 제외, 불필요한 변수들 제외
order <- raw %>% filter(., 구분 == "주문") %>% select(-구분, -ISBN, -카트적재여부, -카트적재일자, -출판일자)

# if NofOrder == 1, is it meaningful reference? also some outliers(huge times of order)
order %>% group_by(회원번호) %>% summarise( n = n() ) %>% select(n) %>% 
  unlist(., use.names = F) %>% quantile(., c(0.01,0.99))

# 회원별로 구매횟수변수 추가
order %<>% group_by(회원번호) %>% mutate( NofOrder = n() )

# 구매량이 1이거나 99%이상인 106개 이상인 주문들 제거, 타깃 데이터프레임
order_light <- order %>% filter(NofOrder != 1 & NofOrder <= 115) %>% select(-NofOrder)

# 각 카테고리별 가장 많이 팔린 책들 
best_seller <- 
  
  function(category, num = 10L) {
    
    order %>% group_by(책제목) %>% filter(카테고리 == category) %>% summarise( n = n() ) %>%
      arrange(., desc(n)) %>% head(., num)
    
  }

best_sellers <- lapply(cats, best_seller, num = 50) ; names(best_sellers) <- cats

# 각 구매자들이 testing의 대상
# 모든 구매자들에 대해서 test할 수는 없다. 

order %>% select(NofOrder) %>% summary() 

# 1st Qu. = 15, 3rd QU. = 66

testee <- order %>% filter(NofOrder >= 15 & NofOrder <= 66) %>% 
  select(회원번호) %>% unlist(., use.names = F) 


###- 1. first algorithm (simple recommendation) 

# basis functions for simple_recommend() ##########################################################

whatIordered <- 
  
  # extract vector of books by each individual, it can be done with certain category
  # this function takes integer id as its input
  # data set 'order' required
  
  function(id, category = NULL) {
    
    if(is.null(category)) {
      
      # result gives a character vector
      order %>% filter(회원번호 == id) %>% select(책제목) %>% 
        unlist(., use.names = F)
      
    } else { # in case we assigned particular category of the book
      
      order %>% filter(회원번호 == id & 카테고리 == category) %>% select(책제목) %>% 
        unlist(., use.names = F)
      
    }
    
  }


basicrecommend <-
  
  # basic algorithm
  # recommend books based on what I bought before
  # this function works with each single book 
  # 'order_light'에만 의존
  
  function(book, id, category) {
    
    # figure out who bought same book with me
    order_light %>% filter(책제목 == book & 회원번호 != id) %>% select(회원번호) %>% 
      unlist(., use.names = F) -> co_readers
    
    # 
    order_light %>% filter(회원번호 %in% co_readers & 카테고리 == category & 책제목 != book) %>% 
      group_by(책제목) %>% summarise( n = n() ) %>% arrange(., desc(n))

  }


list_converge <-
  
  # this function might be conducted after "lapply(..., basicrecommend)"
  # converge lists of recommendations 
  # convergence direction is to l1
  
  function(l1, l2) {
    
    l1 %>% select(책제목) %>% unlist(., use.names = F) -> list1
    l2 %>% select(책제목) %>% unlist(., use.names = F) -> list2
    
    A <- list1[!(list1 %in% list2)] # only in l1
    B <- list1[list1 %in% list2] # both in l1 and l2, should be summed
    C <- list2[!(list2 %in% list1)] # only in l2, should be binded
    
    l1 %>% filter(책제목 %in% B) %>% arrange(책제목) -> t1
    l2 %>% filter(책제목 %in% B) %>% arrange(책제목) -> t2
    l1 %>% filter(책제목 %in% A) -> t3
    l2 %>% filter(책제목 %in% C) -> t4
    
    t1$n <- t1$n + t2$n
    
    t1 <- rbind(t1, t3, t4)
    
    arrange(t1, desc(n))
    
  }


recommend_by_coreaders <-
  
  # basic_recommend()와 list_converge()를 결합한 함수
  # this function gives fine set of recommend list of each individual
  # the output has two columns, its name and weight(:= n)
  
  function(id, category, test.books = NULL) {
    
    if(is.null(test.books)) { # testing이 아닌경우
     
      books <- whatIordered(id) # ordered books regardless of category
      
    } else { # testing의 경우 책의 목록을 별도로 제고
      
      books <- test.books 
      
    }
    
    # each elements of list are recommendation given by ith ordered book of 'id'
    l <- lapply(books, basicrecommend, id = id, category = category) 
    
    if(length(l) == 0) {
      
      # there is no common books according to ordered books
      return(NA)
      
    } else if(length(l) == 1) {
      
      l1 <- l[[1]]
      
    } else {
      
      # loop to merge lists, n considered
      for(i in 2:length(l)) {
        
        l1 <- l[[1]] 
        l2 <- l[[i]] 
        l1 <- list_converge(l1, l2)
        
      }
      
      l1 %>% filter(!(책제목 %in% books)) %>% arrange(., desc(n))
      
    }
    
  }

###################################################################################################

# main function
simple_recommend <- 
  
  # 1) 구매기록이 없는 회원 => 해당 카테고리 내의 인기항목을 추천
  # 2) 구매기록이 있는 회원 => 본인이 구매한 책을 구매한 다른 회원들이 구매한 목록 내에서 추천 
  # data sets 'order' and 'order_light' required to be in the 'Global Environment'
  # 
  
  function(id, category, num = 10L, weight = 0, test.books = NULL) {
    
    
    if(!(id %in% order[["회원번호"]])) {  # 구매기록이 없는경우
      
      best_sellers[[category]] %>% head(., num)
      
    } else { # 구매기록이 있는경우
      
      # 시리즈물을 구매하고 해당 시리즈의 다른 책은 구매하지 않은 경우 
      # 자신이 이미 구매한 도서는 제외 
      
      # 기본 추천 결과
      res <- recommend_by_coreaders(id = id, category = category, test.books = test.books)
      
      if(nrow(res) == 1) { # 구매한 책을 바탕으로 추천할 책이 없는 경우
        
        best_sellers[[category]] %>% head(., num) # 구매기록이 없는것과 동일 
        
      } else { # 일반적인 경우 
        
        res <- head(res, num) 
        bs <- best_sellers[[category]] %>% head(., num) 
        
        # 베스트셀러와 결과를 가중평균 
        bs$n <- bs$n * weight
        res$n <- res$n * (1-weight)
        list_converge(res, bs) %>% head(., num)
        
      }
      
    }
    
  }


