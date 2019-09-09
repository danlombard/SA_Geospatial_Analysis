from bs4 import BeautifulSoup
import urllib.request
from requests import get
import re
import itertools
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import numpy as np
sns.set()

# REQUIRED FEATURES
# Enter listing url
#    Grab info from respective field in html
#    Scrape for Brackenfell and Durbanville
#    Property24 unscrapable, hence scrap privateproperty.za

headers = ({'User-Agent':
            'Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-en) AppleWebKit/533.19.4 (KHTML, like Gecko) Version/5.0.3 Safari/533.19.4'})

url = "https://www.crimestatssa.com/provincesbycrime.php"
response = get(url, headers=headers)

# These will be the fields to populate for our dataset

province = []
crime = []
total_crime = []
prices = []
areas = []
zone = []
condition = []
descriptions = []
urls = []


html_soup = BeautifulSoup(response.text, 'html.parser')

crime_container = html_soup.find_all('p')
second = crime_container[0]
print(second.find_all('span')[0].text)


stat_container = html_soup.find_all('table', class_="standard-table")
first = stat_container[0]
print(first.find_all('td',class_="block-cell block-cell-grey")[1].text)


for i in range(0, 38, 1):
    crime_container = html_soup.find_all('p')
    second = crime_container[i]
    crime.append(second.find_all('span')[0].text)

    stat_container = html_soup.find_all('table', class_="standard-table")
    first = stat_container[i]
    for j in range(0, 9, 1):
        total_crime.append(first.find_all('td',class_="block-cell block-cell-grey")[j].text)




crime_df = pd.DataFrame(np.array(total_crime).reshape(38, 9)).T
crime_df.columns = crime

print(crime_df)

crime_df.to_csv('PropertyWebScrape')
