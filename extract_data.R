
library(httr)

email <- "charles@datachamp.fr"
password <- "D8k56qwRFbknI5yv9J0cWDQJ"
path <- "https://www.udemy.com/join/login-popup/?locale=en_US&response_type=json&next=https%3A%2F%2Fwww.udemy.com%2F&ref=&xref=&display_type=popup"

request <- list(
    email = email,
    password = password
)

response <- POST(url = "https://www.udemy.com",
                 body = request)

response <- POST(url = "https://www.udemy.com")



library(RSelenium)

rD <- rsDriver(verbose = TRUE, browser = "phantomjs")
remDr <- rD$client