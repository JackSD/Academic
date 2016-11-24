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

#building graph with only the actual navigation

my_network = nx.Graph()  # initialize graph object
my_network = nx.from_pandas_dataframe(wine_data, 'combined','previousPage')


# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

# plot the network with spring layout
fig = plt.figure()
nx.draw_spring(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()


this_user_data = wine_data[wine_data['USerID'] == 77]
my_network = nx.Graph()  # initialize graph object
my_network = nx.from_pandas_dataframe(this_user_data, 'combined','previousPage')

# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

this_user_data = wine_data[wine_data['USerID'] == 55]
my_network = nx.Graph()  # initialize graph object
my_network = nx.from_pandas_dataframe(this_user_data, 'combined','previousPage')

# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()


this_user_data = wine_data[wine_data['USerID'] == 5]
my_network = nx.Graph()  # initialize graph object
my_network = nx.from_pandas_dataframe(this_user_data, 'combined','previousPage')

# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

this_user_data = wine_data[wine_data['USerID'] == 10]
my_network = nx.Graph()  # initialize graph object
my_network = nx.from_pandas_dataframe(this_user_data, 'combined','previousPage')

# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()

this_user_data = wine_data[wine_data['USerID'] == 16]
my_network = nx.Graph()  # initialize graph object
my_network = nx.from_pandas_dataframe(this_user_data, 'combined','previousPage')

# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()


this_user_data = wine_data[wine_data['USerID'] == 17]
my_network = nx.Graph()  # initialize graph object
my_network = nx.from_pandas_dataframe(this_user_data, 'combined','previousPage')

# plot the network with default layout
fig = plt.figure()
nx.draw(my_network, node_size = 10, node_color = 'yellow', with_labels=True)
plt.show()