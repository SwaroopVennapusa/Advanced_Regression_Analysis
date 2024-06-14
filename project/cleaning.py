import pandas as pd

# Replace 'your_file.xlsx' with the path to your Excel file
file_path = '/home/swaroop/Downloads/Assignments/STP530/project/23-testcar-2023-10-25.xlsx'

# Read the Excel file
df = pd.read_excel(file_path)

##### For extracting all the fuel types

# # Extract unique values from the 'Test Fuel Type Description' column
# unique_values = df['Test Fuel Type Description'].unique()

# # Convert to a list and print
# unique_values_list = unique_values.tolist()
# # Assuming unique_values_list contains the unique values from the Excel column
# for value in unique_values_list:
#     print(value)

##### For mileage specific to fuel

# fuel_types = ['Cold CO Premium (Tier 2)']

# # Filter the DataFrame for the specified fuel types
# filtered_df = df[df['Test Fuel Type Description'].isin(fuel_types)]

# # Extract the values from 'RND_ADJ_FE' field
# rnd_adj_fe_values = filtered_df['RND_ADJ_FE'].tolist()

# # Print each value on a new line
# for value in rnd_adj_fe_values:
#     print(value)



# 2. Create a new DataFrame with specific columns, excluding certain fuel types
columns_of_interest = ["RND_ADJ_FE", "Vehicle Manufacturer Name", "Rated Horsepower", 
                       "Equivalent Test Weight (lbs.)", "Drive System Description", 
                       "Test Fuel Type Description"]
filtered_df = df.loc[~df["Test Fuel Type Description"].isin(["Electricity", "Hydrogen 5"]), columns_of_interest]

# 3. Rename the column "RND_ADJ_FE" to "Fuel Economy(MPG)"
filtered_df.rename(columns={"RND_ADJ_FE": "Fuel Economy(MPG)"}, inplace=True)

# 4. Remove rows where "Fuel Economy(MPG)" is NA
filtered_df.dropna(subset=["Fuel Economy(MPG)"], inplace=True)

# 5. List all rows with NA values in any column
rows_with_na = filtered_df[filtered_df.isna().any(axis=1)]

# Print rows with NA values in any column
print("Rows with NA values in any column:")
print(rows_with_na)

output_file = 'clean_data.xlsx'  # Replace with your desired output file name
filtered_df.to_excel(output_file, index=False)