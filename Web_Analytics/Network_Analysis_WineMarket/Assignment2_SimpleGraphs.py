
import pandas as pd  # data frame objects
import os
import itertools  # for combinations
import networkx as nx  # graphs/networks
import matplotlib.pyplot as plt  # 2D plotting
import numpy as np  # fast processing of arrays

os.chdir('C:\\Users\\JDoig\Google Drive\Northwestern\PA452\Assignment2')

wine_data = pd.read_csv("WineData_timecategory.csv")

print("\nContents of web_attribute_data ---------------")
# examine the structure of this data frame
print(wine_data)

# keep user_id and web_area_name, drop the web_area_id
test_frame = wine_data[['USerID', 'category']]
print(test_frame.head())
# number of visits to web areas regardless of user
web_area_visits = test_frame.pivot_table(index = 'category', aggfunc = len)

# bar chart for top 20 areas in terms of visits
top_web_area_chart = web_area_visits[:20].plot(kind = 'bar', legend = False, \
    grid = False, title = 'Top Web Areas - WineShop')
top_web_area_chart.set_ylabel('Number of Visits')
top_web_area_chart.set_xlabel('')

# keep user_id and web_area_name, drop the web_area_id
test_frame = wine_data[['USerID', 'category']]
print(test_frame.head())
# number of visits to web areas regardless of user
web_area_visits = test_frame.pivot_table(index = 'category', aggfunc = len)

# bar chart for top 20 areas in terms of visits
top_web_area_chart = web_area_visits[:20].plot(kind = 'bar', legend = False, \
    grid = False, title = 'Top Web Areas - WineShop')
top_web_area_chart.set_ylabel('Number of Visits')
top_web_area_chart.set_xlabel('')


# keep user_id and web_area_name, drop the web_area_id
test_frame = wine_data[['totasecs', 'category']]
print(test_frame.head())
# number of visits to web areas regardless of user
web_area_visits = test_frame.pivot_table(index = 'category', aggfunc =  'mean')

# bar chart for top 20 areas in terms of visits
top_web_area_chart = web_area_visits[:20].plot(kind = 'bar', legend = False, \
    grid = False, title = 'Mean time spent on Page - WineShop')
top_web_area_chart.set_ylabel('Seconds')
top_web_area_chart.set_xlabel('')

