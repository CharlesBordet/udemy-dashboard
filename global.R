
library(data.table)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(shiny)
library(shinydashboard)
library(shinyjs)

sales <- readRDS("data/sales.rds") %>% data.table
setkey(sales, "Transaction Id")
redemptions <- readRDS("data/redemptions.rds") %>% data.table
setkey(redemptions, "Split Id")
refunds <- readRDS("data/refunds.rds") %>% data.table
setkey(refunds, "Refund Date")

Sys.setlocale("LC_TIME", "C")
