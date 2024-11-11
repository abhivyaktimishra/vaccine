rm(list = ls())
directory <- "C:/Users/mabhi/OneDrive/Desktop/R Work/"
#load the data set 
VaccineStateTotal <- read.csv(paste0(directory,"statetotal.csv"))
#summarize the data 
stargazer(VaccineStateTotal, type = "text")
str(VaccineStateTotal)
#scatterplot of vaccinations received in a private and public health facility ---------------
VaccineStateTotal %>% ggplot(aes(y=vaccinepublic,x=vaccineprivate,label=states)) +
  geom_point() +
  stat_cor(method = "spearman", aes(label = ..r.label..)) + ylab("Vaccines Recieved in Private Centres") +
  xlab("Vaccines Recieved in Public Centres")+
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),
                   point.padding = unit(0.5, 'lines'),max.overlaps = 10) 


#barplot vaccines recieved in public and private centres ------------
average_vaccine_public<-round(mean(VaccineStateTotal$vaccinepublic),2)
average_vaccine_private<-round(mean(VaccineStateTotal$vaccineprivate),2)

vaccine_average<-data.frame (Institution = c("Public", "Private"),
                               value=c(average_vaccine_private, average_vaccine_public)
)

vaccine_average %>% 
  ggplot(aes(y=value,x=Institution,fill=Institution)) +
  geom_col(colour="black",width=0.3,    
           position=position_dodge(0.6)) + ylab("Percentage") +
  xlab("Institution") + geom_text(aes(label=value),vjust=-0.2,position = position_dodge(width = 1))

#scatter plot of women literacy and vaccination of the child in a public institution ------
VaccineStateTotal %>% ggplot(aes(y= women15.49literate,x=vaccinepublic,label=states)) +
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..),label.x=20,label.y=90) + ylab("Literacy Level of the Women") +
  xlab("Vaccination Rate of the Children in a Public Health Facility")+
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),
                   point.padding = unit(0.5, 'lines'),max.overlaps = 10) 

#scatter plot of women literacy and vaccination of the child in a private institution ------------
VaccineStateTotal %>% ggplot(aes(y= women15.49literate,x=vaccineprivate,label=states)) +
  geom_point() +
  stat_cor(method = "pearson", aes(label = ..r.label..),label.x=20,label.y=90) + ylab("Literacy Level of the Women") +
  xlab("Vaccination Rate of the Children in a Private Health Facility")+
  geom_label_repel(aes(fontface = 'bold'),box.padding = unit(0.25, 'lines'),
                   point.padding = unit(0.5, 'lines'),max.overlaps = 10) 

#barplot for various vaccines according to NFHS 5 ----------
average_BCG<-round(mean(VaccineStateTotal$BCG),2)
average_3_doses_polio<-round(mean(VaccineStateTotal$X3polio),2)
average_DPT <- round(mean(VaccineStateTotal$DPT),2)
average_hepb <- round(mean(VaccineStateTotal$hepb),2)

vaccine_average<-data.frame (Protection_Against= c("BCG", "Polio", "DPT", "Hep"),
                             value=c(average_BCG, average_3_doses_polio, average_DPT, average_hepb)
)

vaccine_average %>% 
  ggplot(aes(y=value,x=Protection_Against,fill=Protection_Against)) +
  geom_col(colour="black",width=0.6,    
           position=position_dodge(0.6)) + ylab("Percentage") +
  xlab("Disease against which there is protection") + geom_text(aes(label=value),vjust=-0.2,position = position_dodge(width = 1))

VaccineStateTotal4 <- read.csv(paste0(directory,"statetotal4.csv"))
VaccineStateTotal5 <- read.csv(paste0(directory,"statetotal5.csv"))

#barbell chart NFHS 4 and NFHS 5---------
VaccineArrow <- read.csv(paste0(directory, "vaccinearrow.csv"))
VaccineArrow %>% 
  pivot_longer(cols = -states, names_to = "NFHS", values_to = "percentage" )%>%
  ggplot(aes(x = percentage, y= states, color=NFHS)) +
  geom_line(color="black") + geom_point() 

#stacked bar graph ------
VaccineStack <- read.csv(paste0(directory, "stack.csv"))
VaccineStack %>%
  pivot_longer(cols = -states, names_to = "Institution", values_to = "percentage")%>%
ggplot(aes(label = states, fill=states, y= percentage , x= Institution)) + 
  geom_bar(position="fill", stat="identity", color="black")+
  geom_text(aes(label = states), 
            position = position_fill(vjust = 0.5), # Center labels in each stack
            color = "black", size = 3) + theme(legend.position = "none") # Hide the legend 
theme_minimal()

VaccineStack %>% pivot_longer(cols = -states, names_to = "Institution", values_to = "percentage")%>%
  ggplot(aes(label = Institution, fill= Institution, 
                           y= states, x = percentage)) +
geom_bar(position = "fill", stat = "identity") +   geom_text(aes(label=percentage),  position = position_fill(vjust = 0.5), # Center labels in each stack
                                                             color = "black", size = 1.5)
 











vstack <- read.csv(paste0(directory, "vaccinestack.csv"))
  vstack %>% arrange(vaccine_private)

ggplot(vstack, aes(x = reorder(states, vaccine_private))) +
  geom_bar(aes(y = vaccinepublic, fill = "Public"), stat = "identity") +
  geom_bar(aes(y = vaccine_private, fill = "Private"), stat = "identity", position = "stack") +
  coord_flip() +  # Make the bars horizontal
  scale_fill_manual(values = c("Public" = "skyblue", "Private" = "orange")) +
  labs(x = "States", y = "Proportion", fill = "Vaccine Type") +
  theme_minimal()






















######## Extra -------------------------------
scale_fill_manual(values = c('#750906', "#199826", "#123467", "#722261","#623281", "#621215",
                               "#923180", "#023201", "#224336", "#511167", "#290039", "#562218",
                               "#712321", "#623249", "#862322", "#231901", "#454337", "#722228",
                               "#172316", "#011118", "#232890", "#332852", "#321871", "#623172",
                               "#913217", "#081211", "#819016", "#911213", "#992129", "#712119",
                               "#823120", "#452316", "#123197", "#231919", "#234761", "#123971"))



#stacked bar graph

# Sample data
centre_type<- data.frame(
  #center_type = rep(c("Public", "Private"), each = 12),
  #state = rep(c("Andaman & Nicobar Islands", "Andhra Pradesh", "Arunachal Pradesh",
 "Assam", "Bihar", "Chandigarh", "Chhattisgarh", "Dadra and Nagar Haveli & Daman and Diu",
 "Goa", "Gujarat", "Haryana", "Himachal Pradesh", "Jammu & Kashmir",
   "Jharkhand", "Karnataka", "Kerala", "Ladakh", "Lakshadweep",
  "Madhya Pradesh", "Maharastra", "Manipur", "Meghalaya", "Mizoram",
 "Nagaland", "NCT of Delhi", "Odisha","Puducherry", "Punjab", 
  "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", "Uttar Pradesh",
  "Uttarakhand", "West Bengal"), times = 2),
  percentage = c(20, 15, 25, 10, 30, 25, 20, 15, 25, 15)
)

# Create a stacked bar chart
ggplot(data, aes(x = center_type, y = percentage, fill = states)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Vaccine Distribution by Center Type with States",
    x = "Vaccine Center Type",
    y = "Percentage",
    fill = "State"
  ) +
  theme_minimal()
# Normalize percentages within each center_type
data <- data %>%
  group_by(center_type) %>%
  mutate(normalized_percentage = vaccine / sum(percentage) * 100)

# Plot the normalized stacked bar chart
ggplot(data, aes(x = center_type, y = normalized_percentage, fill = state)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Vaccine Distribution by Center Type with States Stacked (Normalized to 100%)",
    x = "Vaccine Center Type",
    y = "Normalized Percentage",
    fill = "State"
  ) +
  theme_minimal()



#working on the arrowplot 
#arrow_reshape<- VaccineArrow %>% select(states,fullvaccination4,fullvaccination5) %>% melt() %>% 
  #mutate(year=ifelse(variable=="NFHS 4","NFHS 5"))

#w06 <- arrow_reshape %>%
  #filter(year=="2006") 
#w16 <- arrow_reshape %>%
  #filter(year=="2016") %>% mutate(state = fct_reorder(state, -value))

#ggplot(VaccineArrow)+
  
  #geom_segment(data = VaccineArrow,
               #aes(x = , y = state,
                   #yend = fullvaccination5, xend = fullvaccination4),
               #color = "#aeb6bf",
               #linewidth= 2.5,
               #alpha = .5) +
  #geom_point(aes(x = value, y = state, color = year), size = 4, show.legend = TRUE) +
  #ggtitle("Full Vaccination") + theme() +xlab("Full Vaccination according to Vaccine Card and Mother's Recall")+
  #ylab("States")

VaccineStack %>%
  ggplot(aes(x = states, y = percentage, fill = Institution)) +
  geom_bar(position = "fill", stat = "identity", color = "black") +
  geom_text(aes(label = Institution), position = position_fill(vjust = 0.5), 
            color = "black", size = 3) +
  theme(legend.position = "none") # Hide the legend

names(VaccineStack)
VaccineStack %>%
  ggplot(aes(x = states, y = percentage, fill = Institution))
print(n=72)
