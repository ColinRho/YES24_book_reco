#- testing
## 검증알고리즘 

# 각 구매자들이 testing의 대상
# 모든 구매자들에 대해서 test할 수는 없다. 

order %>% select(NofOrder) %>% summary() 

# 1st Qu. = 15, 3rd QU. = 66

testee <- order %>% filter(NofOrder >= 15 & NofOrder <= 66) %>% 
  select(회원번호) %>% unlist(., use.names = F) 


# 각 고객별 test 대상이 되는 책들

test_book <- 
  
  function(id) {
    
    l <- sapply(cats, whatIordered, id = id)
    l <- l[sapply(l, function(x) { length(x) != 0 })] # 구매 기록이 없는 카테고리 제거 
    return(l)
    
  }

testing <- 
  
  # 한 사람이 구매한 도서들 중 하나를 빼고 추천도서 출력. 그 중에 제외한 도서가 있는지를 확인... 일종의 CV
  # 도서구매량이 너무 적으면(1,2개) test불가.
  # 대신 제외한 도서와 같은 카테고리를 추천해야 한다. 
  # 결과가 0 ~ 1 사이의 적중률(?) 로 나와야함
  
  function(id) {
    
    # test 대상이 되는 책들이자 정답
    l <- test_book(id) 

    if(length(l) == 0) {

      cat("해당 카테고리의 책을 구매하지 않았습니다.")

    } else {

      l <- melt(l) ; colnames(l) <- c("책제목", "카테고리")
      books <- whatIordered(id)
    
      # test 결과가 저장되는 벡터
      test.res <- c()
    
      # leave-one-out 알고리즘에 의한 testing loop
      for(i in 1:nrow(l)) {
      
        # i번째 책의 카테고리
        categ <- l$카테고리[i]
      
        # test 대상들 중에서 i번째를 제거한 나머지 책들
        loo <- books[which(books != l$책제목[i])]
      
        # i번째 책이 제거된 상태에서의 추천
        res <- simple_recommend(id, category = categ, test.books = loo) 
      
        if(l$책제목[i] %in% res) { # 추천결과에 제거된 책이 있다면,
        
          test.res[i] <- 1
        
        } else { # 없다면 (error)
          
          test.res[i] <- 0
        
        }


    }

    
      
    }
    
    return(test.res)
  }


