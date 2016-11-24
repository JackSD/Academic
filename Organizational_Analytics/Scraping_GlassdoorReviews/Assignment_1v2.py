from selenium import webdriver
from bs4 import BeautifulSoup

import time
import os 

# this is configuration: Company base URL, Company name, and number of reviw pages to scrape set here
Company='NetSuite'
baseurl = "https://www.glassdoor.com"
baseCompanyurl = "https://www.glassdoor.com/Reviews/NetSuite-Reviews-E11153"
filename = 'NetSuite_review_output.csv'
PageCount = 3

my_file_object = open(filename, 'w')
my_file_object.writelines("Company,Review_URL,Review_Date,Review_Title,Pros,Cons,Advice_mgmt,Rating_Categories,Rating_scores\n")  
my_file_object.close() 
mydriver = webdriver.Firefox()

# now looping through the review pages
for i in range( 0 , PageCount):
    #open page in Selenium
    #print(baseurl)
    #this is constructing the review list URL 
    if i==0: suffix ="" 
    else: suffix="_P" + str(i+1)
    CurrentReviewPage = baseCompanyurl + suffix + '.htm'
    mydriver.get(CurrentReviewPage)
    #get links (from B-soup)
    time.sleep(20) 
    html1 = mydriver.page_source
    soup = BeautifulSoup(html1, "lxml")
    #drill down to all reviews found on the page
    for tag in soup.find_all('a','reviewLink'):
        print(tag)
         #follow the link
        link = tag.get('href', None)
        mydriver.get(baseurl + link)
        #sleeps are needed to allow the JS to execute
        time.sleep(20) 
        html2 = mydriver.page_source
        soup2 = BeautifulSoup(html2, "lxml")
        #find and store the review content
        TITLE = soup2.find('title').get_text()
        #try/except blocks added as BeautifulSoup seems to truncate some tags randomly
        try:
            PROS = soup2.find('p','pros noMargVert notranslate truncateThis wrapToggleStr').get_text()
        except:
            PROS =''
        try:
            CONS = soup2.find('p','cons noMargVert notranslate truncateThis wrapToggleStr').get_text()
        except:
            CONS =''
        try:
            ADV = soup2.find('p','adviceMgmt noMargVert notranslate truncateThis wrapToggleStr').get_text()        
        except:
            ADV =''
        try:
            DATE = soup2.find('time','date subtle small').get_text()
        except:
            DATE =''
        #this is getting out the rating data (1-5). TODO: store more elegantly
        try:
            RATINGS = soup2.find('div','subRatings module')
            RATINGSnames = RATINGS.find_all('div','minor')
            rnames = ''
            for r in RATINGSnames:
                rnames += r.get_text()  + '-'
            
            RATINGSscores = RATINGS.find_all('span','notranslate notranslate_title gdBars gdRatings med ')
            scores = ''
            for r in RATINGSscores:
                scores += r.get('title') + '-'
        except:
            scores =''
            ranmes =''
        
        #finally, store the data in a CSV
        my_file_object = open(filename, 'a')
        my_file_object.writelines(Company +','+ link+','+ DATE +','+TITLE+','+PROS +','+ CONS +','+ ADV+','+rnames +','+ scores + '\n')  
        my_file_object.close()
              


  