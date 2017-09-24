


from urllib.request import urlopen
from bs4 import BeautifulSoup
import re

html = urlopen("http://allrecipes.com/search/results/?wt=banana%20bread&sort=re")

soup = BeautifulSoup(html, 'lxml')




for link in soup.find("section", {"id": "grid"}).findAll("a", href=re.compile("^(/recipie/)"))
	print(link)