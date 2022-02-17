get_data <- function(){
  call_1 <- "curl -v -X POST -u tobias.anton.85@gmail.com https://api.tidepool.org/auth/login"
  hcall_2 <- 'curl -s -X GET -H "x-tidepool-session-token: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJkdXIiOjI1OTIwMDAsImV4cCI6MTU2NjczOTM2NCwic3ZyIjoibm8iLCJ1c3IiOiI1NzQxN2Y4OTg5In0.nhGADpf5xrZQklSMPz6d9HXnhZaXPLLkC-TmfpCxt5c" -H "Content-Type: application/json" "https://api.tidepool.org/data/57417f8989" > data_download.json'
}


out1 <- system(call_1, intern = TRUE)

library(curlconverter)

# endpoint + {user-id}
GET("https://api.tidepool.org/data/57417f8989")

POST("https://api.tidepool.org/auth/login", 
     config = list(authenticate("", 
                                "")
                   )
     )
