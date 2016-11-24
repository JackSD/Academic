from __future__ import division, print_function

import pandas as pd  # data frame objects
import numpy as np  # fast processing of arrays

# we will draw upon Harrington's modeling code for association rules
# Harrington, P. (2012). Machine learning in action. 
#     Shelter Island, N.Y.: Manning. [ISBN-13: 9781617290183]
# open-source code from:  www.manning.com/MachineLearninginAction  
# Chapter 11 of this book contains Python programs for the apriori algorithm
# place Harrington's association rule modeling code in a directory called mlia
from mlia import apriori  # this brings in methods for the apriori algorithm

wine_data = pd.read_csv("WineData.csv")

# frequency table for areas visited
print(wine_data.pivot_table(index = 'Page', aggfunc = 'count'))

# create list of unique user identifiers
user_list = list(wine_data['ID'].unique())

# create list of unique area identifiers
area_list = list(wine_data['Page'].unique())

area_list_of_lists = []  # initialize list of lists as needed for associaiton rules
# work with one user at a time
for user in user_list:
    # gather subset data frame for this user
    this_user_data = wine_data[wine_data['ID'] == user]
    print('\n',this_user_data)
    # get list of areas for this user
    user_area_list = list(this_user_data['Page'])
    print(user_area_list)
    # add this user's list of sites to the list of lists
    area_list_of_lists.append(user_area_list)
    
# examine the structure of the list of lists
print(type(area_list_of_lists)) 
print(area_list_of_lists)
    
# set paramteters for apriori algorithm as implemented in mlia
alpha = 0.03  # required level of support for apriori item sets
beta = 0.1  # required level of confidence for apriori rules
max_other_items =  2  # maximum other items in reported rule set
# frozen set is an immutable set
# choose an item of interest... in the Microsoft case this will be an area 
# in this demo, we are using merely a single letter to represent an area/item
item_of_interest = frozenset(['Shiraz'])  # character string of item of interest

# run the apriori algorithm for association rules
L, suppData = apriori.apriori(area_list_of_lists, minSupport = alpha)

print('Identified rules with support = ', alpha, 'and confidence = ', beta)
rules = apriori.generateRules(L, suppData, minConf = beta)
print(rules)

# search across the rule set starting with smaller sets
# and moving to larger and larger sets until no rules
# exist that satisfy the requirement of including item_of_interest
# here we print to the console/screen
n_other_items = 1  # initialize reporting at one other item
while n_other_items <= max_other_items:
    print('\nRules with ', n_other_items, ' other item(s)')
    for item in L[n_other_items]:        
        if item.intersection(item_of_interest): print(item)
    n_other_items = n_other_items + 1    

# repeat the printing of rules to a plain text file
with open('report_association_rules_demo.txt', 'w') as file_out:
    n_other_items = 1  # initialize reporting at one other item
    while n_other_items <= max_other_items:
        file_out.write('\nRules with ' + str(n_other_items) + ' other item(s)')
        file_out.write('\r\n') # windows line termination
        # file_out.write('\n') # Linux/Unix/Mac OSX line termination
        for item in L[n_other_items]:        
            if item.intersection(item_of_interest): 
                file_out.write(str(item))
                file_out.write('\r\n') # windows line termination
                # file_out.write('\n') # Linux/Unix/Mac OSX line termination
        n_other_items = n_other_items + 1    
