library(ggplot2)
library(dplyr)
library(tidyverse)

data <- read.csv("SF_listings.csv")
head(listings)
str(listings)
length(listings)


#Question 1
#Property Type
#Linear regression to predict property type’s avg. rating
lm1 <- lm(data= data, avg_rating ~ property_type.1.)
summary(lm1)

#bar graph for avg. rating

data |>
  group_by(property_type.1.) |>
  summarise(avg_rating = mean(avg_rating, na.rm = T)) |> # Calculate average ratings by
  property type
ggplot(
  aes(x = property_type.1.,
      y = avg_rating,
      fill = property_type.1.)) + # Use property_type.1. to color bars
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = round(avg_rating, 2)), # Add text labels showing rounded average ratings
    vjust =-0.5 # Position the text slightly above the bars
  ) +
  scale_fill_manual(
    values = c(
      "Home" = "skyblue",
      "Hotel" = "lightgreen",
      "Unique" = "violet",
      "Apartment" = "lightpink" # Add color for Apartment
    )
  ) +
  labs(
    x = "Property Type",
    y = "Average Rating",
    title = "Average Rating by Property Type",
    fill = "Property Type" # Legend title for the fill
  ) +
  theme_minimal()

#Linear regression to predict property type’s revenue

lm2 <- lm(data= data, Revenue ~ property_type.1.)
summary(lm2)

#Bar graph for revenue


# Calculate the estimated revenue for each property type
coefficients_data <- data.frame(
  property_type = c("Apartment (Baseline)", "Home", "Hotel", "Unique"),
  estimate = c(
    79502,
    79502 + (-7819),
    79502 + (-40751),
    79502 + 35611
  )
)


# Round the estimates for clarity
 coefficients_data$estimate <- round(coefficients_data$estimate, 0)

# Create the bar chart
 ggplot(coefficients_data, aes(x = property_type, y = estimate, fill = property_type)) +
 geom_bar(stat = "identity") +
 geom_text(aes(label = estimate), vjust =-0.5) + # Add text labels with estimates
 scale_fill_manual(
 values = c(
 "Apartment (Baseline)" = "lightpink",
 "Home" = "skyblue",
 "Hotel" = "lightgreen",
 "Unique" = "violet"
 )
 ) +
 labs(
 x = "Property Type",
 y = "Estimated Revenue",
 title = "Estimated Revenue by Property Type",
 fill = "Property Type"
 ) +
 theme_minimal()
 
 
 
 #Room Type
 #Linear regression to predict room type’s avg. rating

 lm3 <- lm(data= data, avg_rating ~ room_type)
 summary(lm3)
 
 #Bar graph for room type avg. rating

 coefficients_data <- data.frame(
   room_type = c("Entire Home/Apartment (Baseline)", "Hotel Room", "Private Room", "Shared
 Room"),
   estimate = c(4.767373, 4.767373- 0.387123, 4.767373- 0.085099, 4.767373- 0.102373) #
   Baseline + effects
 )
 
 
 # Create the bar chart
 
 ggplot(coefficients_data, aes(x = room_type, y = estimate, fill = room_type)) +
   geom_bar(stat = "identity") +
   geom_text(aes(label = round(estimate, 2)), vjust =-0.5) + # Add text labels showing
   coefficients
 scale_fill_manual(
   values = c(
     "Entire Home/Apartment (Baseline)" = "skyblue",
     "Hotel Room" = "lightcoral",
     "Private Room" = "gold",
     "Shared Room" = "lightgreen"
   )
 ) +
   labs(
     x = "Room Type",
     y = "Average Rating",
     title = "Effect of Room Type on Average Rating",
     fill = "Room Type"
   ) +
   theme_minimal()
 
 
 #Linear regression to predict room type’s revenue
 lm4 <- lm(data= data, Revenue ~ room_type)
 summary(lm4)
 #Bar graph for room type revenue
 coefficients_data <- data.frame(
   room_type = c("Entire Home/Apartment (Baseline)", "Hotel Room", "Private Room", "Shared
 Room"),
   estimate = c(89939, 89939- 84491, 89939- 57921, 89939- 75033) # Baseline + effects
 )
 
 
 # Create the bar chart
 
 ggplot(coefficients_data, aes(x = room_type, y = estimate, fill = room_type)) +
   geom_bar(stat = "identity") +
   geom_text(aes(label = round(estimate, 0)), vjust =-0.5) + # Add text labels showing
   coefficients
 scale_fill_manual(
   values = c(
     "Entire Home/Apartment (Baseline)" = "skyblue",
     "Hotel Room" = "lightcoral",
     "Private Room" = "gold",
     "Shared Room" = "lightgreen"
   )
 ) +
   labs(
     x = "Room Type",
     y = "Revenue Estimate",
     title = "Effect of Room Type on Revenue",
     fill = "Room Type"
   ) +
   theme_minimal()
 
 
 #multiple regression, with property type as the control to the relationship between avg rating and
 #room type
 
 lm5 <- lm(data= data, avg_rating ~ room_type + property_type.1.)
 summary(lm5)
 #Bar graph that compares avg.rating for room type and controlled property type on the

#Relationship between avg. rating and room type
 
 summary(lm(data= data, avg_rating ~ room_type + property_type.1.))
 summary(lm(data= data, avg_rating ~ room_type))
 PropertyControl <- data.frame(
   room_type = c("Entire Home/Apartment", "Private", "Shared", "Hotel Room"),
   avg_rating_with_property = c(4.77, 4.68, 4.67, 4.38),
   avg_rating_without_property = c(4.73, 4.61, 4.59, 4.34)
 )
 
 
 data_long <- PropertyControl %>%
   pivot_longer(
     cols = c(avg_rating_with_property, avg_rating_without_property),
     names_to = "rating_type",
     values_to = "avg_rating"
   )
 
 
 ggplot(data_long, aes(x = room_type, y = avg_rating, fill = rating_type)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
   geom_text(
     aes(label = round(avg_rating, 1)), # Display the rounded average rating
     position = position_dodge(width = 0.8), # Ensure text is aligned with the bars
     vjust =-0.5, # Adjust the vertical position of the labels (above the bars)
     color = "black", # Color of the text
     size = 4 # Size of the text
   ) +
   labs(
     title = "Average Ratings by Room Type",
     x = "Room Type",
     y = "Average Rating",
     fill = "Rating Type"
   ) +
   scale_fill_manual(
     values = c("avg_rating_with_property" = "lightblue", "avg_rating_without_property" =
                  "lightgreen"),
     labels = c("Room Type", "Room Type Adjusting for Property")
   ) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
 #multiple regression, with property type as the control to the relationship between revenue and
 #room type
 
 lm6 <- lm(data= data, Revenue ~ room_type + property_type.1.)
 summary(lm6)
 
 #Bar graph that compares revenue for room type and controlled property type on the
 #Relationship between revenue and room type
 
 summary(lm(data= data, Revenue ~ room_type + property_type.1.))
 summary(lm(data= data, Revenue ~ room_type))
 PropertyControl <- data.frame(
   room_type = c("Entire Home/Apartment", "Private", "Shared", "Hotel Room"),
   revenue_with_property = c(89939, 32018, 14906, 5448),
   revenue_without_property = c(87351, 28926, 14465, 18600)
 )
 
 
 
 data_long <- PropertyControl %>%
   pivot_longer(
     cols = c(revenue_with_property, revenue_without_property),
     names_to = "revenue_type",
     values_to = "revenue"
   )
 ggplot(data_long, aes(x = room_type, y = revenue, fill = revenue_type)) +
   geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
   geom_text(
     aes(label = round(revenue, 1)), # Display the rounded average rating
     position = position_dodge(width = 0.8), # Ensure text is aligned with the bars
     vjust =-0.5, # Adjust the vertical position of the labels (above the bars)
     color = "black", # Color of the text
     size = 4 # Size of the text
   ) +
   labs(
     title = "Revenue by Room Type",
     x = "Room Type",
     y = "Revenue",
     fill = "Revenue Type"
   ) +
   scale_fill_manual(
     values = c("revenue_with_property" = "lightblue", "revenue_without_property" = "lightgreen"),
     labels = c("Room Type", "Room Type Adjusting for Property")
   ) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
 Accommodation
 #linear regression for accommodation and performance
 airbnb$accommodates2 <- ifelse(airbnb$accommodates == 1, 1, ifelse(
   airbnb$accommodates == 0, "NA", ifelse(
     airbnb$accommodates == 2, 2, ifelse(
       airbnb$accommodates == 3, 3, ifelse(
         airbnb$accommodates == 4, 4, ifelse(
           airbnb$accommodates == 5, 5, ifelse(
             airbnb$accommodates == 6, 6, "7-16")))))))
 summary(lm(data = airbnb, avg_rating ~ accommodates2))
 summary(lm(data = airbnb, Revenue ~ accommodates2))
 #bar chart- Average Rating by No. of People a Listing Accommodates
 airbnb |>
   filter(accommodates2 != "NA", !is.na(accommodates2)) |> # Remove rows where
   accommodates2 is "0" or NA
 group_by(accommodates2) |>
   summarise(avg_rating = mean(avg_rating, na.rm = T)) |> # Calculate average ratings
   ggplot(aes(x = accommodates2, y = avg_rating, fill = accommodates2)) + # color bars
   geom_bar(stat = "identity") +
   geom_text(
     aes(label = round(avg_rating, 2)), # Add text labels showing rounded average ratings
     vjust =-0.5, # Position the text slightly above the bars
     color = "black" # Use contrasting text color for better visibility
   ) +
   scale_fill_manual(
     values = c("1" = "skyblue", "2" = "lightgreen", "3" = "violet", "4" = "lightpink", "5" = "gold",
                "6" = "lightcoral", "7-16" = "purple"
     )
   ) +
   labs(
     x = "No. of People", y = "Average Rating",
     title = "Estimated Average Rating by No. of People a Listing Accommodates",
     fill = "No. of People" # Legend title for the fill
   ) +
   ylim(0, max(airbnb$avg_rating, na.rm = T) * 1.05) + # Increase the y-axis scale by 20%\
   theme_minimal() +
   theme(
     plot.title = element_text(size = 12, margin = margin(b = 20)) # Reduce font size and add
     space below the title
   )
 
 
 #bar chart- Estimated Revenue by No. of People a Listing Accommodates
 coefficients_data <- data.frame(
   accommodates2 = c("1", "2", "3", "4", "5", "6", "7-16"),
   estimate = c(30742, 30742 + 27556, 30742 + 25703, 30742 + 51101, 30742 + 70960,
                30742 + 107880, 30742 + 55854 )
 )
 
 # Round the estimates for clarity
 coefficients_data$estimate <- round(coefficients_data$estimate, 0)
 # Create the bar chart
 ggplot(coefficients_data, aes(x = accommodates2, y = estimate, fill = accommodates2)) +
   geom_bar(stat = "identity") +
   geom_text(aes(label = estimate), vjust =-0.5, size = 3) + # Add text labels with estimates
   scale_fill_manual(
     values = c("1" = "skyblue", "2" = "lightgreen", "3" = "violet", "4" = "lightpink", "5" = "gold",
                "6" = "lightcoral", "7-16" = "purple" )
   ) +
   labs(
     x = "No. of People", y = "Estimated Revenue",
     title = "Estimated Revenue by No. of People a Listing Accommodates",
     fill = "No. of People" # Legend title for the fill
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 12, margin = margin(b = 20)) # Reduce font size and add
     space below the title
   )
 
 Neighborhoods
 #linear regression for neighborhood and performance
 summary(lm(data = airbnb, avg_rating ~ grouped_neighborhood_2))
 summary(lm(data = airbnb, Revenue ~ grouped_neighborhood_2))
 #control for accommodates
 summary(lm(data = airbnb, avg_rating ~ grouped_neighborhood_2 + accommodates2))
 summary(lm(data = airbnb, Revenue ~ grouped_neighborhood_2 + accommodates2))
 
 #bar chart- Estimated Average Rating by Neighborhood
 airbnb |>
   filter(grouped_neighborhood_2 != "Santa Cruz County",
          grouped_neighborhood_2 != "Monterey County",
          grouped_neighborhood_2 != "East Palo Alto",
          grouped_neighborhood_2 != "North Bay" ) |> # Remove rows
   group_by(grouped_neighborhood_2) |>
   summarise(avg_rating = mean(avg_rating, na.rm = T)) |> # Calculate average ratings
   ggplot(
     aes(x = reorder(grouped_neighborhood_2,-avg_rating),
         y = avg_rating,
         fill = grouped_neighborhood_2)) + # color bars
   geom_bar(stat = "identity") +
   geom_text(
     aes(label = round(avg_rating, 2)), # Add text labels showing rounded average ratings
     vjust =-0.5, # Position the text slightly above the bars
     color = "black" # Use contrasting text color for better visibility
   ) +
   scale_fill_manual(
     values = c(
       "Downtown San Francisco" = "#FF5733", # Vibrant red-orange
       "Eastern San Francisco" = "#33FF57", # Bright green
       "Central San Francisco" = "#5733FF", # Deep blue
       "Southeastern San Francisco" = "#FFC300", # Golden yellow
       "Western and Seaside San Francisco" = "#FF33A1", # Hot pink
       "Southern San Francisco" = "#33FFF5", # Aqua blue
       "Northern San Francisco" = "#8D33FF", # Purple
       "Peninsula" = "#FF8C33",
       # Sunset orange
       "East Bay" = "#33D4FF",
       "South Bay" = "#A1FF33",
       # Sky blue
       # Lime green
       "Unincorporated area" = "#FF3367" # Rose red
     )
   ) +
   labs(
     x = "Neighborhood",
     y = "Average Rating",
     title = "Estimated Average Rating by Neighborhood",
     fill = "Neighborhood" # Legend title for the fill
   ) +
   ylim(0, max(airbnb$avg_rating, na.rm = T) * 1.05) + # Increase the y-axis scale by 5%
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 60, hjust = 1),
         legend.position = "none" # Remove the legend
   )
 
 #bar chart- Estimated Revenue by Neighborhood
 coefficients_data <- data.frame(
   neighborhood = c("Downtown San Francisco",
                    "Eastern San Francisco",
                    "Central San Francisco",
                    "Southeastern San Francisco",
                    "Western and Seaside San Francisco",
                    "Southern San Francisco", "Northern San Francisco" ),
   estimate = c(
     42730,
     42730 + 135659,
     42730 + 108241,
     42730 + 26157,
     42730 + 23603,
     42730 + 23211,
     42730 + 49925 )
 )
 # Round the estimates for clarity
 coefficients_data$estimate <- round(coefficients_data$estimate, 0)
 
 # Create the bar chart
 ggplot(coefficients_data, aes(x = reorder(neighborhood,-estimate), y = estimate, fill =
                                 neighborhood)) +
   geom_bar(stat = "identity") +
   geom_text(aes(label = estimate), vjust =-0.5, size = 3) + # Add text labels with estimates
   scale_fill_manual(
     values = c(
       "Downtown San Francisco" = "#FF5733", # Vibrant red-orange
       "Eastern San Francisco" = "#33FF57", # Bright green
       "Central San Francisco" = "#5733FF", # Deep blue
       "Southeastern San Francisco" = "#FFC300", # Golden yellow
       "Western and Seaside San Francisco" = "#FF33A1", # Hot pink
       "Southern San Francisco" = "#33FFF5", # Aqua blue
       "Northern San Francisco" = "#8D33FF" # Purple )
     ) +
       labs(
         x = "Neighborhood",
         y = "Estimated Revenue",
         title = "Estimated Revenue by Neighborhood",
         fill = "Estimated Revenue" # Legend title for the fill
       ) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 60, hjust = 1),
             legend.position = "none" # Remove the legend
       )
     
 #Question 2
     #linear regression for host type and average rating- 3 models
     airbnb$host_type1 <- ifelse(airbnb$property_type.1. == "Hotel", 0, ifelse(
       airbnb$host_listings_count <= 3, "1", 0))
     airbnb$host_type2 <- ifelse(airbnb$property_type.1. == "Hotel", 0, ifelse(
       airbnb$host_listings_count == 1, "1", ifelse(airbnb$host_listings_count == 2, "2",
                                                    ifelse(airbnb$host_listings_count == 3, "3", ">=4"))))
     airbnb$host_type3 <- ifelse(airbnb$property_type.1. == "Hotel", 0, ifelse(
       airbnb$host_listings_count >= 1 & airbnb$host_listings_count <= 3, "1-3",
       ifelse(airbnb$host_listings_count >= 4 & airbnb$host_listings_count <= 6, "4-6",
              ifelse(airbnb$host_listings_count >= 7 & airbnb$host_listings_count <= 9, "7-9", ">=10"))))
     lm1 <- lm(data = airbnb, avg_rating ~ host_type1)
     lm2 <- lm(data = airbnb, avg_rating ~ host_type2)
     lm3 <- lm(data = airbnb, avg_rating ~ host_type3)
     summary(lm1)
     summary(lm2)
     summary(lm3)
     
     #anova comparing 3 models
     anova(lm1, lm2, lm3)
     #linear regression for host type and revenue- 3 models
     lm4 <- lm(data = airbnb, Revenue ~ host_type1)
     lm5 <- lm(data = airbnb, Revenue ~ host_type2)
     lm6 <- lm(data = airbnb, Revenue ~ host_type3)
     summary(lm4)
     summary(lm5)
     summary(lm6)
     
     #anova comparing 3 models
     anova(lm4, lm5, lm6)
     #bar chart- host type and average rating
     airbnb$host_type2 <- ifelse(airbnb$property_type.1. == "Hotel", "Hotel", ifelse(
       airbnb$host_listings_count == 1, "1", ifelse(airbnb$host_listings_count == 2, "2",
                                                    ifelse(airbnb$host_listings_count == 3, "3", ">=4"))))
     #reorder the sequence of labels
     airbnb$host_type2 <- factor(airbnb$host_type2,
                                 levels = c("1", "2", "3", ">=4", "Hotel"))
     #create the bar chart
     airbnb |>
       group_by(host_type2) |>
       summarise(avg_rating = mean(avg_rating, na.rm = T)) |> # Calculate average ratings
       ggplot( aes(x = host_type2, y = avg_rating,
                   fill = host_type2)) + # color bars
       geom_bar(stat = "identity") +
       geom_text(
         aes(label = round(avg_rating, 2)), # Add text labels showing rounded average ratings
         vjust =-0.5, # Position the text slightly above the bars
         color = "black" # Use contrasting text color for better visibility
       ) +
       scale_fill_manual(
         values = c(
           "1" = "lightcoral",
           "2" = "purple",
           "3" = "yellow",
           ">=4" = "lightgreen",
           "Hotel" = "lightpink" )
       ) +
       labs(
         x = "No. of Listings/Host Type",
         y = "Average Rating",
         title = "Average Rating by No. of Listings/Host Type",
         fill = "No. of Listing/Host Type" # Legend title for the fill
       ) +
       ylim(0, max(airbnb$avg_rating, na.rm = T) * 1.05) + # Increase the y-axis scale by 20%
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 60, hjust = 1),
             legend.position = "none" # Remove the legend
       )
     
     #bar chart- host type and revenue
     airbnb$host_type3 <- ifelse(airbnb$property_type.1. == "Hotel", 0, ifelse(
       airbnb$host_listings_count >= 1 & airbnb$host_listings_count <= 3, "1-3",
       ifelse(airbnb$host_listings_count >= 4 & airbnb$host_listings_count <= 6, "4-6",
              ifelse(airbnb$host_listings_count >= 7 & airbnb$host_listings_count <= 9, "7-9", ">=10"))))
     coefficients_data <- data.frame(
       host_type3 = c(">=10", "1-3", "4-6", "7-9", "Hotel"),
       estimate = c(15586, 15586 + 94580, 15586 + 43188, 15586 + 47770, 15586 + 23165),
       stringsAsFactors = FALSE # Ensure it's not automatically converted to factors
     )
     #reorder the sequence of labels
     coefficients_data$host_type3 <- factor(coefficients_data$host_type3,
                                            levels = c("1-3", "4-6", "7-9", ">=10", "Hotel"))
     # Round the estimates for clarity
     coefficients_data$estimate <- round(coefficients_data$estimate, 0)
     # Create the bar chart
     ggplot(coefficients_data, aes(x = host_type3, y = estimate, fill = host_type3)) +
       geom_bar(stat = "identity") +
       geom_text(aes(label = estimate), vjust =-0.5, size = 3) + # Add text labels with estimates
       scale_fill_manual(
         values = c(
           "1-3" = "lightcoral",
           "4-6" = "lightgreen",
           "7-9" = "lightblue",
           "Hotel" = "lightpink",
           ">=10" = "gold" )
       ) +
       labs(
         x = "No. of Listings/Host Type",
         y = "Estimated Revenue",
         title = "Estimated Revenue by No. of Listings/Host Type",
         fill = "No. of Listings/Host Type" # Legend title for the fill
       ) +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 60, hjust = 1),
             legend.position = "none" # Remove the legend
       )
     
     
     Question 3
     airbnb <- read.csv('AirBnB Project- SF_Listings.csv')
     str(airbnb)
     # Run the linear regression model with all amenities impact on revenue
     amenities_revenue <- lm(data = airbnb,
                             Revenue ~ wifi + hangers + `hair.dryer` + `long.term.stays.allowed` +
                               kitchen + essentials + refrigerator + microwave + `self.check.in` +
                               `free.street.parking` + `free.parking.on.premises` +
                               `paid.parking.off.premises`+`paid.parking.on.premises` + tv + washer + `dedicated.workspace`
                             + dishwasher + `security.cameras.on.property` + `room.darkening.shades` + `pets.allowed` +
                               `outdoor.furniture` + `air.conditioning`)
     summary(amenities_revenue)
     # Data for positive impacts
     positive_impact <- data.frame(
       Amenity = c("Hair.Dryer", "Free.Street.Parking", "Dishwasher", "Washer", "Long.Term.Stay"),
       Impact = c(48121, 35887, 30162, 24786, 24715)
     )
     # Data for negative impacts
     negative_impact <- data.frame(
       Amenity = c("Dedicated.Workspace", "Air.Conditioning", "TV"),
       Impact = c(-39179,-41866,-26237)
     )
     # Plot for positive impacts
     positive_plot <- ggplot(positive_impact, aes(x = Amenity, y = Impact, fill = Amenity)) +
       geom_bar(stat = "identity") +
       labs(
         title = "Positive Impact of Amenities on Revenue",
         x = "Amenity",
         y = "Impact on Revenue"
       ) +
       theme_minimal()
     print(positive_plot)
     
     # Plot for negative impacts
     negative_plot <- ggplot(negative_impact, aes(x = Amenity, y = Impact, fill = Amenity)) +
       geom_bar(stat = "identity") +
       labs(
         title = "Negative Impact of Amenities on Revenue",
         x = "Amenity",
         y = "Impact on Revenue"
       ) +
       theme_minimal()
     print(negative_plot)
     
     
     # Run the linear regression model with all amenities impact on customer satisfaction
     amenities_cust.satisfaction <- lm(data = airbnb,
                                       avg_rating ~ wifi + hangers + `hair.dryer` + `long.term.stays.allowed` +
                                         kitchen + essentials + refrigerator + microwave + `self.check.in` +
                                         `free.street.parking` + `free.parking.on.premises` + `paid.parking.off.premises`
                                       +
                                         dishwasher +
                                         `paid.parking.on.premises` + tv + washer + `dedicated.workspace` +
                                         `security.cameras.on.property` + `room.darkening.shades` + `pets.allowed` +
                                         `outdoor.furniture` + `air.conditioning`)
     summary(amenities_cust.satisfaction)
     
     
     # Data for customer satisfaction impacts
     positive_impact_cs <- data.frame(
       Amenity = c("Hair Dryer", "Refrigerator", "Self Check-In",
                   "Free Street Parking", "Dishwasher", "Outdoor Furniture"),
       Impact = c(0.158831, 0.074368, 0.049947, 0.081947, 0.054776, 0.074121)
     )
     negative_impact_cs <- data.frame(
       Amenity = c("WiFi", "Essentials", "TV", "Pets Allowed", "Air Conditioning"),
       Impact = c(-0.057406,-0.112752,-0.050696,-0.044525,-0.042689)
     )
     

     # Plot for positive impacts on customer satisfaction
     positive_cs_plot <- ggplot(positive_impact_cs, aes(x = Amenity, y = Impact, fill = Amenity)) +
       geom_bar(stat = "identity") +
       labs(
         title = "Positive Impact of Amenities on Customer Satisfaction",
         x = "Amenity",
         y = "Impact on Customer Satisfaction"
       ) +
       theme_minimal()
     print(positive_cs_plot)
     # Plot for negative impacts on customer satisfaction
     negative_cs_plot <- ggplot(negative_impact_cs, aes(x = Amenity, y = Impact, fill = Amenity)) +
       geom_bar(stat = "identity") +
       labs(
         title = "Negative Impact of Amenities on Customer Satisfaction",
         x = "Amenity",
         y = "Impact on Customer Satisfaction"
       ) +
       theme_minimal()
     print(negative_cs_plot)
     
     # Run the linear regression model with all amenities impact on price
     amenities_price <- lm(data = airbnb,
                           price ~ wifi + hangers + `hair.dryer` + `long.term.stays.allowed` +
                             kitchen + essentials + refrigerator + microwave + `self.check.in` +
                             `free.street.parking` + `free.parking.on.premises` + `paid.parking.off.premises` +
                             `paid.parking.on.premises` + tv + washer + `dedicated.workspace` + dishwasher +
                             `security.cameras.on.property` + `room.darkening.shades` + `pets.allowed` + `outdoor.furniture`
                           + `air.conditioning`)
     summary(amenities_price)

     # Different effects of amenities across neighborhoods on revenue
     amenities_neigh_rev <- lm(data = data, Revenue ~ wifi* grouped_neighborhood_2 + hangers *
                                 grouped_neighborhood_2 + hair.dryer* grouped_neighborhood_2 + kitchen*
                                 grouped_neighborhood_2 + essentials* grouped_neighborhood_2 + free.street.parking*
                                 grouped_neighborhood_2 + free.parking.on.premises * grouped_neighborhood_2 + tv*
                                 grouped_neighborhood_2 + washer* grouped_neighborhood_2 + dedicated.workspace*
                                 grouped_neighborhood_2 + dishwasher* grouped_neighborhood_2 + pets.allowed*
                                 grouped_neighborhood_2 + air.conditioning* grouped_neighborhood_2 , na.action =
                                 na.omit)
     summary(amenities_neigh_rev)
     # Different effects of amenities across neighborhoods on ratings
     amenities_neigh_ratings <- lm(data = data, avg_rating ~ wifi* grouped_neighborhood_2 +
                                     hangers* grouped_neighborhood_2 + hair.dryer* grouped_neighborhood_2 + kitchen*
                                     grouped_neighborhood_2 + essentials* grouped_neighborhood_2+ free.street.parking*
                                     grouped_neighborhood_2 + free.parking.on.premises * grouped_neighborhood_2 + tv*
                                     grouped_neighborhood_2 + washer* grouped_neighborhood_2 + dedicated.workspace*
                                     grouped_neighborhood_2 + dishwasher* grouped_neighborhood_2 + pets.allowed*
                                     grouped_neighborhood_2 + air.conditioning* grouped_neighborhood_2 , na.action =
                                     na.omit)
     
     summary(amenities_neigh_ratings)
     
     # Mentions of amenities in reviews
     amenities <- c("wifi", "wi-fi", "wi fi", "hangers", "hair dryer", "kitchen", "essentials", "parking", "tv",
                    "washer", "workspace", "dishwasher", "pets", "air conditioning", "refrigerator",
                    "microwave", "self check in", "security camera", "pets", "air conditioner")
    
      amenity_counts <- data.frame(Amenity = amenities, Count = 0)
     for (i in 1:length(amenities))
     {amenity <- amenities[i]
     count <- sum(str_detect(tolower(data$comments), amenity))
     amenity_counts$Count[i] <- count}
     # Bar graph for amenities’ count
     
     amenities <- c("WiFi", "Hangers", "Hair dryer", "Kitchen", "Essentials", "Parking", "Tv", "Washer",
                    "Workspace", "Dishwasher", "Pets", "AC", "Refrigerator", "Microwave", "Self check-in",
                    "Security camera")
     
     mentions <- c(112, 1, 5, 709, 55, 886, 112, 76, 4, 12, 8, 5, 32, 77, 2, 1)
     data <- data.frame(Amenities = amenities, Mentions = mentions)
     ggplot(data, aes(x = reorder(Amenities,-Mentions), y = Mentions, fill = Mentions)) +
       geom_bar(stat = "identity") +
       scale_fill_gradientn(colors = rainbow(7)) +
       labs(x = "Amenities", y = "Mentions") +
       theme_minimal() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       geom_text(aes(label = Mentions), vjust =-0.5)

 
