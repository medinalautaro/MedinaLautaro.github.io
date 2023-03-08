import pandas as pd

# Read the csv file and store it.
df = pd.read_csv("Fitabase_Data_4.12.16-5.12.16\weightLogInfo_merged.csv")

# Alter the file with pandas datetime function in order to change the format
df.Date = pd.to_datetime(df.Date)
# Printing the first rows to check them
print(df.head(6))

# Save the file. "Index = False" is to eliminate the id column created by the read_csv function.
df.to_csv("Fitabase_Data_4.12.16-5.12.16\weightLogInfo_merged.csv", index=False)

