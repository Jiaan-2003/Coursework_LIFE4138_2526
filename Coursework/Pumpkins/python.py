###########################################
#     Title: Pumpkin Challenge Report     #
#       Author: Jiaan Randhawa-Heer       # 
#            Dataset number: 6            # 
###########################################
#pip install pandas #installs required package if needed
#pip install seaborn #installs required package if needed
#pip install matplotlib #installs required package if needed

#imports required packages
import pandas as pd #imports pandas
import seaborn as sns #imports seaborn
import matplotlib.pyplot as plt #imports matplotlib

# Q1. Read in pumpkins.csv
pumpkins = pd.read_csv("Pumpkins/pumpkins_datasets/pumpkins_06.csv") #reads in and loads allocated pumpkin dataset

print(pumpkins.head()) #checks the top data in the set to see if it has loaded

# Q2. Identify the heaviest pumpkin (variety, location, year)
heaviest_pumpkin = pumpkins.sort_values(by="weight_lbs", ascending=False).head(1) #sorts weight value in the data from heighest to lowest

heaviest_pumpkin = heaviest_pumpkin[["weight_lbs", "variety", "city", "state_prov", "country", "id"]] #selects necessary columns from the data

print(heaviest_pumpkin) #prints the required information

# Q3. Convert pounds -> kilograms (weight_kg column)
def convert_lbs_to_kg(x): #define a function to convert pounds (lbs) to kilograms (kg)
    kg = x * 0.453592 #numerical formula for conversion
    return kg 

pumpkins["weight_kg"] = convert_lbs_to_kg(pumpkins["weight_lbs"]) #uses the function to convert all values from the lbs column to a new kg column

print(pumpkins.head()) #prints the top of the dataset to check confirmation of the new kg column

# Q4. Add a weight_class column (light/medium/heavy)
pumpkins_sorted_weights_kg = pumpkins.sort_values(by="weight_kg", ascending=True) #sorts the kg weight column to be ordered

print(pumpkins_sorted_weights_kg["weight_kg"].head()) #prints the top weights to help determine weight classification bands
print(pumpkins_sorted_weights_kg["weight_kg"].tail()) #prints the end weights to help determine weight classification bands

def weight_classification(x): #defines a function for weight classification
    if x <100: 
        return "light" #returns light in the weight class column if weights are under 100kg
    elif x < 500:
        return "medium" #returns medium in the weight class column if weights are below 500kg, but still 100kg or above
    else:
        return "heavy" #returns heavy in the weight class column if weights are 500kg or above

pumpkins["weight_class"] = pumpkins["weight_kg"].apply(weight_classification) #produces the weight class column by applying the weight classification function to the weights_kg column

print(pumpkins[["weight_kg", "weight_class"]].head(20)) #prints the top 20 results to check the weight_class column correctly aligns to weight_kg

# Q5. Plot estimated vs actual weight (coloured by class)
pumpkins = pumpkins.dropna(subset=["est_weight"]) #removes missing or NA values from the estimated weight column in the dataset
print(pumpkins.isnull().sum()) #checks the dataset columns to see if missing values from estimated weight have been removed

def est_weight_convert_lbs_to_kg(x): #defines a previously used conversion of lbs to kg, but for estimated weight this time
   kg = x * 0.453592 #numerical formula for conversion
   return kg 

pumpkins["est_weight_kg"] = est_weight_convert_lbs_to_kg(pumpkins["est_weight"]) #creates a new column of estimated weights in kg to be used with actual weights in kg that were classified into approriate bands

print(pumpkins.head()) #prints the top of the dataset to check confirmation of the new estimated weight in kg column

sns.scatterplot(data = pumpkins, x = "weight_kg", y = "est_weight_kg", #seaborn scatterplot that applies required data and sets axis labels
            hue = "weight_class", alpha = 0.6 ) #hue applies weight category differences and alpha sets some transparency for the clustered plots

plt.xlabel("Actual Weight (kg)") #labels the x axis (plt is matplotlib.pyplot)
plt.ylabel("Estimated Weight (kg)") #labels the y axis
plt.title("Estimated Pumpkin Weights vs Actual Pumpkin Weights (kg) (coloured by class)") #titles the plot
plt.legend(title="Weight Class") #provides a legend to show colour assignments for weight class bands
plt.show()

# Q6. Filter for three countries and save (pumpkins_filtered.csv)
pumpkins["country"].unique() #unique function prints an array of all the different countries for the selection of three

three_filtered_countries = ["United Kingdom", "Canada", "United States"] #selects three countries to be filtered
pumpkins_filtered = pumpkins[pumpkins["country"].isin(three_filtered_countries)] #filters the pumpkins dataset to only use data from the three selected countries
pumpkins_filtered.to_csv("pumpkins_filtered.csv", index=False) #saves the filtered data to a new CSV file
pumpkins_filtered["country"].unique() #uses unique to check the data has been saved and check it only contains the three selected countries

# Q7. Summarise mean weights by country and variety
filtered_country_mean = pumpkins_filtered.groupby("country")["weight_kg"].mean() #uses groupby to group country with weight in kg, and calculates the mean of each country's pumpkin weight (kg) 
print(filtered_country_mean) #shows the filtered countries with their corresponding mean pumpkin weights in kg

filtered_mean_with_countryvariety = pumpkins_filtered.groupby(["country", "variety"])["weight_kg"].mean() #uses groupby to group data by country and variety, also calculates the mean weight based on the kg weight column 
print(filtered_mean_with_countryvariety) #shows the top and end of the newly summarised data with country, variety and mean (kg)

filtered_mean_df = filtered_mean_with_countryvariety.reset_index() #converts mean data into a dataframe that can be sorted

countryvariety_lowest_mean = filtered_mean_df.sort_values(by = "weight_kg", ascending = True).head(1) #sorts the mean values in order of weight (kg), going from lowest to highest
print(countryvariety_lowest_mean) #prints the lowest mean weight (kg) along with the country and variety

# Q8. Create a boxplot for the three countries
def convert_lbs_to_kg_filteredlist(x): #defines a previously used conversion of lbs to kg, but for the filtered data, this time
   kg = x * 0.453592 #numerical formula for conversion
   return kg 
  
pumpkins_filtered.loc[:, "weight_kg"] = convert_lbs_to_kg_filteredlist(pumpkins_filtered["weight_lbs"]) #creates a new column in filtered for the converted weights from lbs to kg
 
sns.boxplot(data = pumpkins_filtered, x = "country", y = "weight_kg", color = "orange", linewidth = 2) #seaborn boxplot that loads required data, sets axis labels, colours boxes orange and sets linewidth

plt.title("Boxplot of Three Selected Countries' Pumpkin Weights (kg)") #titles the boxplot (matplotlib)
plt.xlabel("Country") #labels the x axis 
plt.ylabel("Weight (kg)") #labels the y axis
plt.show() #displays the plot

# Q9. Create a faceted plot by variety
faceted_variety_plot = sns.catplot(data = pumpkins_filtered, x = "country", y = "weight_kg", col = "variety", kind = "violin", #faceted seaborne plot that sets data for axis and column, also sets the kind of plots to be violin plots.
    col_wrap = 3, height = 4) #sets the wrap so plots appear in rows of 3 when possible and sets the height of each plot to 4

faceted_variety_plot.set_titles("{col_name}") #labels each plot with variety
faceted_variety_plot.set_axis_labels("Country", "Weight (kg)") #labels the x and y axis of each plot
plt.show() #displays the faceted violin plots
