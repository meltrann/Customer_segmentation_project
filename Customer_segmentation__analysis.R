
install.packages("skimr")

library(rio)
library(tidyverse)
library(dplyr)
library(skimr)
library(broom)
library(ggplot2)
library(tidyquant)


#Import the dataset
customers_tbl<- rio::import("Mall_Customers.csv")

#Exploratory Data Analysis
# View data
glimpse(customers_tbl)

head(customers_tbl)

# Summary of the dataset
skim(customers_tbl)

summary(customers_tbl)


# QA Check - Check for any duplicates 
duplicate_rows <- customers_tbl%>% 
    filter(duplicated(customers_tbl))

# QA Check  - Check for Outliers
customers_outliers <- customers_tbl %>%
    select(CustomerID,`Annual Income (k$)`,`Spending Score (1-100)`) 



# Create Box Plots for Gross Billings and Units Sold
customers_outliers %>%
    ggplot(aes(CustomerID, `Annual Income (k$)`)) +
    geom_boxplot(outlier.shape = NA) +  
    geom_jitter(aes(y = `Annual Income (k$)`), width = 0.2, alpha = 0.5) +
    theme_tq()

groupon_outliers %>%
    ggplot(aes(Segment, `Units Sold`)) +
    geom_boxplot(outlier.shape = NA) +  # Hide outlier points
    geom_jitter(aes(y = `Units Sold`), width = 0.2, alpha = 0.5) +
    theme_tq()

# QA Check 5 - Check for correlation between Units Sold and Gross Billings  
ggplot(data = groupon, aes(x =`Units Sold`, y = `Gross Billings`, color = Segment)) +
    geom_point(size = 3) +
    labs(title = "Correlation Between Units Sold and Gross Billings",
         x = "Units Sold",
         y = "Gross Billings") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    facet_wrap(~ Segment)


#Preprocessing
customers_seg_tbl <- customers_tbl %>%
    select(CustomerID, `Annual Income (k$)`, `Spending Score (1-100)`)


#Create a function to perform k-means clustering
kmeans_fxn <- function(centers = 3) 
    {
    customers_seg_tbl %>%
    select(-CustomerID) %>%
    kmeans(centers = centers, nstart = 100)
}

# Create a tibble with a range of center values and apply the kmeans_fxn 
kmeans_fxn_tbl <- tibble(centers = 1:15) %>%
    mutate(k_means = centers %>% map(kmeans_fxn)) %>%
    mutate(glance = k_means %>% map(glance))

# Create a Skree Plot to find the optimal value of K 
kmeans_fxn_tbl%>%
    unnest(glance) %>%
    select(centers, tot.withinss) %>%
    ggplot(aes(centers,tot.withinss))+
    geom_point(size = 4, color = "#001f5f")+
    geom_line(size = 1,color = "#001f5f")+
    ggrepel::geom_label_repel(aes(label = centers))+
    scale_y_continuous(labels = scales::comma)+
    theme_tq()+
    labs(
        title = "Skree Plot",
        subtitle = "Determine the optimal number of clusters in the dataset"
    )


# Performing K-Means 
kmeans_customers_obj <- customers_seg_tbl %>%
    select(-CustomerID)%>%
    kmeans(centers = 5, nstart = 100)

kmeans_customers_obj$centers

#Assign the cluster assignment for each brand
kmeans_5_clusters<- kmeans_customers_obj %>% augment(customers_seg_tbl) %>%
    select(CustomerID, .cluster) %>%
    left_join(customers_seg_tbl, by = "CustomerID")
    

#Create visual
kmeans_visual <- kmeans_5_clusters %>%
   
    ggplot(aes(x = `Annual Income (k$)`, y = `Spending Score (1-100)`, color = factor(.cluster)))+
    
    # Geometries
    geom_point(size=2) +
    
    # Formatting
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation Based on Annual Income and Spending Score",
        subtitle = "Using K-Means Clustering with 5 Clusters",
        color = "Customer Cluster"
    ) +
    theme(legend.position = "right")

-----------------
#Age variable distribution 
customers_tbl %>%
select(CustomerID,Gender,Age) %>%
ggplot(aes(Age, fill = Gender)) +
geom_density(alpha = 0.5) +
scale_fill_tq() +
theme_tq() +
theme(legend.position = "bottom") +
    labs(
        title = "Distribution of Age by Gender",
        subtitle = "Compare the age distribution between male and female customers"
    ) 


----------------------

# Determine quantile values for income
quantiles <- customers_tbl %>%
    pull(`Annual Income (k$)`) %>%
    quantile(probs = c(0, 0.25, 0.50, 0.75, 1))

# Create a new column 'income_bin' based on quantiles
customers_income_groups <- customers_tbl %>%
    mutate(income_bin = case_when(
        `Annual Income (k$)` <= quantiles[2] ~ "low",
        `Annual Income (k$)` <= quantiles[4] ~ "medium",
        TRUE ~ "high"
    )) %>%
    group_by(Gender,income_bin) %>%
    summarize(count = n()) %>%
    ungroup()


# Define the desired order of levels
order_levels <- c("low", "medium", "high")

# Define custom colors for fill
custom_colors <- c("Male" = "#2C3E50", "Female" = "#CCBE93")

# Create a bar chart 
customers_income_groups %>%
    mutate(income_bin = factor(income_bin, levels = order_levels)) %>%
    ggplot(aes(x = income_bin, y = count, fill = Gender)) +
    geom_col(position = "dodge", width = 0.7) +  
    scale_fill_manual(values = custom_colors)+
    theme_tq()


labs(x = "Category 2",
     y = "Revenue",
     title = "Revenue by Category 2" ,
     subtitle = "In millions of dollars", 
     caption = "Source : Business Science University") +


    
    
#Create a histogram
customers_tbl %>%
    gather(key = "variable", value = "value", Age, `Annual Income (k$)`, `Spending Score (1-100)`) %>%
    ggplot(aes(x = value, fill = variable)) +
    geom_histogram(binwidth = 5, alpha = 0.7, position = "identity", color = "white") +
    scale_fill_manual(values = c("Age" = "#2C3E50", "Annual Income (k$)" = "#18BC9C", "Spending Score (1-100)" = "#CCBE93")) +
    facet_wrap(~ variable, scales = "free") +
    labs(fill = "") +
    theme_tq()


-----------------------------------
    #Preprocessing
    customers_seg_tbl <- customers_tbl %>%
    select(CustomerID, Age, `Spending Score (1-100)`)


#Create a function to perform k-means clustering
kmeans_fxn <- function(centers = 3) 
{
    customers_seg_tbl %>%
        select(-CustomerID) %>%
        kmeans(centers = centers, nstart = 100)
}

# Create a tibble with a range of center values and apply the kmeans_fxn 
kmeans_fxn_tbl <- tibble(centers = 1:15) %>%
    mutate(k_means = centers %>% map(kmeans_fxn)) %>%
    mutate(glance = k_means %>% map(glance))

# Create a Skree Plot to find the optimal value of K 
kmeans_fxn_tbl%>%
    unnest(glance) %>%
    select(centers, tot.withinss) %>%
    ggplot(aes(centers,tot.withinss))+
    geom_point(size = 4, color = "#001f5f")+
    geom_line(size = 1,color = "#001f5f")+
    ggrepel::geom_label_repel(aes(label = centers))+
    scale_y_continuous(labels = scales::comma)+
    theme_tq()+
    labs(
        title = "Skree Plot",
        subtitle = "Determine the optimal number of clusters in the dataset"
    )


# Performing K-Means 
kmeans_customers_obj <- customers_seg_tbl %>%
    select(-CustomerID)%>%
    kmeans(centers = 4, nstart = 100)

kmeans_customers_obj$centers

#Assign the cluster assignment for each brand
kmeans_4_clusters<- kmeans_customers_obj %>% augment(customers_seg_tbl) %>%
    select(CustomerID, .cluster) %>%
    left_join(customers_seg_tbl, by = "CustomerID")



#Create visual
kmeans_visual <- kmeans_4_clusters %>%
    
    ggplot(aes(x = Age, y = `Spending Score (1-100)`, color = factor(.cluster)))+
    
    # Geometries
    geom_point(size=2) +
    # Formatting
    theme_tq() +
    scale_color_tq() +
    labs(
        title = "Customer Segmentation Based on Age and Spending Score",
        subtitle = "Using K-Means Clustering with 4 Clusters",
        color = "Customer Cluster"
    ) +
    theme(legend.position = "right")

#Box plot to look at distribution
# Box Plot

summary(customers_tbl$`Spending Score (1-100)`)

summary(customers_tbl$`Annual Income (k$)`)

customers_tbl %>%
    
    ggplot(aes(`Spending Score (1-100)`)) +
    geom_boxplot() +
    coord_flip() +
    theme_tq()




customers_tbl %>%
    gather(key = "variable", value = "value", `Spending Score (1-100)`, `Annual Income (k$)`) %>%
    ggplot(aes(x = variable, y = value, fill = variable)) +
    geom_boxplot() +
    scale_fill_manual(values = c("#18BC9C", "#CCBE93")) + 
    labs(fill = "") +
    theme_tq() +
    labs(
        title = "",
        subtitle = "",
    )
    
