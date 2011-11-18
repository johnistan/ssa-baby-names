
import BeautifulSoup
import mechanize
import csv


g = csv.writer(open('top_baby_names.csv', 'wb'))#got the have the 'wb' to deal with extra white space
g.writerow(("Year","Rank","Male","Female"))#Set file headers
for year in range(1880, 2010):#Years to loop over change to what ever range you want
    br = mechanize.Browser()
    br.set_handle_robots(False)#To best the robots.txt file. Don't really know if this is kosher but it works
    br.open("http://www.ssa.gov/oact/babynames/") #Main Page
    br.select_form(name = "popnames")#HTML Form
    br["year"]= str(year)
    br["top"]=["1000"]
    #print br.form #Very helpful in navigating what the hell you are selecting when building
    response = br.submit().read()
    
    response1 = response.split("Rank",1)[1]#Only grab the HTML you want to deal with. Probably not need 
    soup = BeautifulSoup.BeautifulSoup(response1)
    col = soup.findAll('td')
    
    for i in range(0, len(col)-5, 3): #-5 to get rid of the merged row and links at the foot of the page makes it a little fragile but fuck it it works. 
        #Also you itterate over 3 because the table width is three. If you added percentages or other options you would have to make it bigger.
        if i == 0:
            rank = col[0].string
            male = col[1].string
            female = col[2].string
            record = (str(year),rank, male , female) 
            #print ",".join(record)
            g.writerow(record)
        else:
            rank = col[i].string
            male = col[i + 1].string
            female = col[i + 2].string
            record = (str(year),rank, male , female)
            #print ",".join(record)
            g.writerow(record)
    