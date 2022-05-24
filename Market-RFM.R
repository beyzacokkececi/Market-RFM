library(readxl)
library(dplyr)
library(rfm)
library(ggplot2)
library(tidyr)
library(treemap)
library(treemapify)

market_listings <- read.csv("market_listings.csv")
market_listings$listingdate <- as.Date(market_listings$listingdate)
typeof(market_listings$listingdate)

rfm_results <- rfm_table_order(
  data = market_listings,
  customer_id = id,
  revenue = listingprice,
  order_date = listingdate, 
  analysis_date = as.Date("2022-05-24"),
  recency_bins = 3,
  frequency_bins = 3,
  monetary_bins = 3
)

rfm_heatmap(rfm_results) 
rfm_bar_chart(rfm_results)
rfm_rm_plot(rfm_results)

segment_names <- c("Champions", "Promising", "Need Attention", "Lost", "Loyal")

recency_lower <- c(3,3,2,1,3)
recency_upper <- c(3,3,2,1,3)
frequency_lower <- c(2,1,1,1,2)
frequency_upper <- c(3,2,3,3,3)
monetary_lower <- c(3,1,1,1,1)
monetary_upper <- c(3,3,3,3,2)

segment <- rfm_segment(rfm_results,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)

ggplot(data=segment, aes(x=reorder(segment, segment, function(x)-length(x)),fill=segment))+
  geom_bar()+
  xlab("sku segments")+
  ylab("number of sku")+
  geom_text(stat="count", aes(label=..count..), vjust=0.5)+
  scale_fill_manual(values=c("green4",
                             "grey40",
                             "purple",
                             "red",
                             "steelblue1"))



market_orders <- read.csv("market_orders.csv")
market_orders$lastorder <- as.Date(market_orders$lastorder)

rfm_o_results <- rfm_table_order(
  data = market_orders,
  customer_id = id,
  revenue = paidprice,
  order_date = lastorder, 
  analysis_date = as.Date("2022-05-24"),
  recency_bins = 3,
  frequency_bins = 3,
  monetary_bins = 3
)

rfm_heatmap(rfm_o_results) 
rfm_bar_chart(rfm_o_results)
rfm_rm_plot(rfm_o_results)

segment_names <- c("Champions", "Promising", "Need Attention", "Lost", "Loyal")

recency_lower <- c(3,3,2,1,3)
recency_upper <- c(3,3,2,1,3)
frequency_lower <- c(2,1,1,1,2)
frequency_upper <- c(3,2,3,3,3)
monetary_lower <- c(3,1,1,1,1)
monetary_upper <- c(3,3,3,3,2)

segment_o <- rfm_segment(rfm_o_results,
                       segment_names,
                       recency_lower,
                       recency_upper,
                       frequency_lower, 
                       frequency_upper, 
                       monetary_lower,
                       monetary_upper)

colnames(segment_o)[2] <- c("segment_o")

ggplot(data=segment_o, aes(x=reorder(segment_o, segment_o, function(x)-length(x)),fill=segment_o))+
  geom_bar()+
  xlab("buyer segments")+
  ylab("number of buyer")+
  geom_text(stat="count", aes(label=..count..), vjust=0.5)+
  scale_fill_manual(values=c("green4",
                             "grey40",
                             "purple",
                             "red",
                             "steelblue1"))

