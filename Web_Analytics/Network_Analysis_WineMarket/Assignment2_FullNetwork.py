
import pandas as pd  # data frame objects
import os
import itertools  # for combinations
import networkx as nx  # graphs/networks
import matplotlib.pyplot as plt  # 2D plotting
import numpy as np  # fast processing of arrays

os.chdir('C:\\Users\\JDoig\Google Drive\Northwestern\PA452\Assignment2')

wine_data = pd.read_csv("WineData.csv")

print("\nContents of web_attribute_data ---------------")
# examine the structure of this data frame
print(wine_data)

# create list of unique user identifiers
user_list = list(wine_data['ID'].unique())
print('\n\nuser_list ',user_list)

# create list of unique area identifiers
area_list = list(wine_data['Page'].unique())
print('\n\narea_list: ',area_list)

my_network = nx.Graph()  # initialize graph object
# work with one user at a time
# to create pairs of areas for links/edges in network
for user in user_list:
    # gather subset data frame for this user
    this_user_data = wine_data[wine_data['ID'] == user]
    print('\n',this_user_data)
    # get list of areas for this user
    user_area_list = list(this_user_data['Page'])
    print(user_area_list)
    # create all combinations of two areas for this user
    area_pairs = list(itertools.combinations(user_area_list, 2))
    # add edges to graph object
    my_network.add_edges_from(area_pairs)
    
# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

# plot the network with spring layout
fig = plt.figure()
nx.draw_spring(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

# plot the degree distribution of the network (area frequencies)
fig = plt.figure()
plt.hist(nx.degree(my_network).values())
plt.xlabel('Node Degree')
plt.ylabel('Frequency')
plt.show()

#looking at the network for a single, typical node
my_network = nx.Graph()  # initialize graph object
# gather subset data frame for this user
this_user_data = wine_data[wine_data['ID'] == 8]
print('\n',this_user_data)
# get list of areas for this user
user_area_list = list(this_user_data['Page'])
print(user_area_list)
# create all combinations of two areas for this user
area_pairs = list(itertools.combinations(user_area_list, 2))
# add edges to graph object
my_network.add_edges_from(area_pairs)
    
# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

# plot the network with spring layout
fig = plt.figure()
nx.draw_spring(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

#now sine thce graph is directed, simplify by only add the 




#Prediction: using a collab filtering model


