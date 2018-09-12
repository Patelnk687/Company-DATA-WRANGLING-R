#will be used to filter data
library (dplyr)

#used to clean my data
library (tidyr)

#easy way for me to find where my data is stored
refine<-read.csv(file.choose(),header=T)

#i can see the raw data
refine

#makes everything in data table under company to lowercase
refine_lower<-mutate (refine, company=tolower(company))
refine_lower


#this gives the the different types of names i will have to make into 4
distinct(select(refine_lower, company))
unique(refine_lower$company)

#refines the names into similar names for the same company (http://astrostatistics.psu.edu/su07/R/html/base/html/grep.html)
refine_lower$company <- gsub(glob2rx("*ps*"),"phillips",refine_lower$company)
refine_lower$company <- gsub(glob2rx("*ak*$"),"akzo",refine_lower$company)
refine_lower$company <- gsub(glob2rx("*van*$"),"van houten",refine_lower$company)
refine_lower$company <- gsub(glob2rx("*uni*$"),"unilever",refine_lower$company)


#YES We have to made it into 4
distinct(select(refine_lower, company))

# Seperate product_code and product_number
refine_lower <- refine_lower %>% separate(Product.code...number, c("product_code", "product_number"), sep = "-")


 # We are makeing names of the product categories 
temp_prodcodes <- c("p","x","v","q")
temp_categories  <- c("Smartphone", "Laptop", "TV", "Tablet")
temp_df <- tbl_df(data_frame(temp_prodcodes, temp_categories))
print(temp_df)

# join on product code
refine_lower <- inner_join(refine_lower, temp_df, by = c("product_code" = "temp_prodcodes"))
refine_lower

# rename temp_categories column
refine_lower <- rename(refine_lower, product_category = temp_categories)

#4: Add full address for geocoding by address, city, country
refine_lower$full_address<- paste(refine_lower$address,refine_lower$city,refine_lower$country, sep=",")
refine_lower


#5: Create dummy variables for company and product category
refine_lower <-refine_lower %>% mutate(company_phillips = ifelse(company == "phillips", 1, 0),
         company_akzo = ifelse(company == "akzo", 1, 0), 
         company_van_houten = ifelse(company == "van houten", 1, 0),
         company_unilever = ifelse(company == "unilever", 1, 0),
         product_smartphone = ifelse(product_category == "Smartphone", 1, 0),
         product_tv = ifelse(product_category == "TV", 1, 0),
         product_laptop = ifelse(product_category == "Laptop", 1, 0),
         product_tablet = ifelse(product_category == "Tablet", 1, 0))


refine_lower

