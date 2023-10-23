library("sqldf")
library(tidyverse)
library(RODBC)

db_conn <- odbcConnect("myDatabase", rows_at_time = 1)

# Question 1: Which geographic regions generate the most sales?

# Join SalesTerritory "Group" a.k.a region with general SalesOrderHeader record
# I used SalesOrderHeader TotalDue instead of SalesOrderDetail LineTotal but I verified that 
# TotalDue also reflects sales and my result matches the example on the assignment.
salesQuery <- "SELECT Sales.SalesOrderHeader.TotalDue, Sales.SalesTerritory.[Group] as 'geographic_region'
FROM Sales.SalesTerritory
LEFT JOIN Sales.SalesOrderHeader
ON Sales.SalesOrderHeader.TerritoryID = Sales.SalesTerritory.TerritoryID"

sales_df <- sqlQuery(db_conn, salesQuery, stringsAsFactors = FALSE)
head(sales_df)

# Check for nulls

summary(sales_df)
unique(sales_df$geographic_region)

# Plot as a pie chart

# group by region
# code adapted from sparkbyexamples.com, see link below
grouped_sales <- aggregate(sales_df$TotalDue, by = list(sales_df$geographic_region), FUN = sum)
grouped_sales

# rename columns
grouped_sales <- rename(grouped_sales, "geographic_region" = "Group.1")
grouped_sales <- rename(grouped_sales, "total_sales" = "x")

# calculate %
grouped_sales$percentage <- with(grouped_sales, round((total_sales/(22173618+89228792+11814376))*100), 0)

# create labels for pie chart
# string combining function from r-bloggers.com, see link below
grouped_sales$labels <- with(grouped_sales, paste(geographic_region,", ",as.character(percentage),"%", sep=""))
grouped_sales

# create vectors for pie() function
vector_labels = grouped_sales[["labels"]]
vector_count = grouped_sales[["total_sales"]]

# create pie chart
# code adapted from r-graph-gallery.com, see link below
pie(vector_count, labels=vector_labels)

# ggplot version from r-graph-gallery.com, see link below
# compute the position of labels
# code taken from r-graph-gallery
grouped_sales<- grouped_sales %>% 
  arrange(desc(labels)) %>%
  mutate(prop = total_sales / sum(grouped_sales$total_sales) *100) %>%
  mutate(ypos = cumsum(prop) - 0.6*prop)

# create pie chart
ggplot(grouped_sales, aes(x="", y=prop, fill=labels)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes(y = ypos, label = labels), color = "white", size=3) +
  scale_fill_brewer(palette="Set1") +
  labs(title="Total Regional Sales")

# the region that generates the most sales is North America at 72%

# Question 2: What are the most profitable products?

productQuery <- "SELECT Production.Product.Name, Sales.SalesOrderDetail.OrderQty,
Production.Product.StandardCost, Sales.SalesOrderDetail.LineTotal
FROM Sales.SalesOrderDetail
LEFT JOIN Production.Product
ON Sales.SalesOrderDetail.ProductID = Production.Product.ProductID"

product_df <- sqlQuery(db_conn, productQuery, stringsAsFactors = FALSE)


product_df$Profit <- with(product_df, (LineTotal - StandardCost*OrderQty))


# another group by method adapted from stackoverflow,
# but adjusted due to warnings of summarise_each() and funs() being deprecated
grouped_product <- product_df %>%
  group_by(Name) %>%
  summarise(across(everything(), list(sum = sum)))

# sort by profit & grab top 10 values top plot
grouped_product <- grouped_product[order(grouped_product$Profit_sum,decreasing=TRUE),]
profitable_prods <- head(grouped_product, 10)

# grab only the columns I want to plot & rename
sub <- profitable_prods %>% 
  select(1,4,5)
  
sub <- rename(sub, "Total Sales" = "LineTotal_sum") %>% 
  rename("Total Profit" = "Profit_sum")

# melt the data so Total Proft and Sales can be plotted as
# a stacked bar chart
# code adapted from stackoverflow.com
product_long <- tidyr::pivot_longer(sub, cols=c('Total Sales', 'Total Profit'), 
                                    names_to='Variable', values_to='Value')

# plot as stacked bar graph
# code adapted from same stackoverflow thread as above
# scale_fill_manual specification found on learn.saylor.org
# scale_x_continuous adapted from rpubs.com
ggplot(product_long, aes(x = Value, y = Name, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Most Profitable Products",
       x = "",
       y = "",
       fill = "") +
  scale_fill_manual(values = c("Total Profit" = "orange", "Total Sales" = "blue")) +
  theme_light() +
  theme(legend.position='bottom') + 
  scale_x_continuous(labels = scales::comma)

# the most profitable product is "Mountain-200 Black, 42."

top_prod <- profitable_prods[1,1]
as.character(top_prod)

# Code Sources
# Grouping 1: https://sparkbyexamples.com/r-programming/group-by-count-in-r/
# Grouping 2: https://stackoverflow.com/questions/8212699/group-by-multiple-columns-and-sum-other-multiple-columns
# ggplot pie chart: https://r-graph-gallery.com/piechart-ggplot2.html
# pie() pie chart: https://r-graph-gallery.com/131-pie-plot-with-r.html
# paste(): https://www.r-bloggers.com/2022/08/r-program-to-concatenate-two-strings/
# melting/plotting two numerical vars in a bar chart: https://stackoverflow.com/questions/42820677/ggplot-bar-plot-side-by-side-using-two-variables
# custom bar colours: https://learn.saylor.org/mod/book/view.php?id=58485&chapterid=45033
# continuous x axis: https://rpubs.com/techanswers88/remove-scientific-notation-in-ggplot
