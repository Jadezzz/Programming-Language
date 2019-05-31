import urllib.request
import re
import collections
import numpy as np
import matplotlib.pyplot as plt

author = input('Input author:')
query_author = author.replace(" ","+")
url = 'https://arxiv.org/search/?query="' + query_author + '"&searchtype=author&abstracts=hide&size=200'
content = urllib.request.urlopen(url)
html_str = content.read().decode('utf-8')

result_pattern = '<li class="arxiv-result"[\s\S]*?</li>'
result = re.findall(result_pattern, html_str)
if len(result) == 0:
    print('Sorry, your query for author: "' + author + '" produced no results.')
    quit()

co_author_dict = collections.defaultdict(int)
year_dict = collections.defaultdict(int)

authors_pattern = '<a href="[\s\S]*?</a>'
date_pattern = '<span class="has-text-black-bis has-text-weight-semibold">originally announced</span>[\s\S]*'

for r in result:

    author_flag = False

    authors_list = r.split('<p class="authors">')[1].split('</p>')[0]
    
    authors_names = re.findall(authors_pattern, authors_list)
    for name in authors_names:
        full_name = name.split('>')[1].split('</a')[0].strip()
        if full_name.lower() == author.lower():
            author_flag = True

        if full_name[0:2] == 'et':
            continue
        
    if author_flag:
        
        for name in authors_names:
            full_name = name.split('>')[1].split('</a')[0].strip()
            if full_name[0:2] == 'et':
                continue
            co_author_dict[full_name] += 1

        date_str = r.split('<p class="is-size-7">')[1].split("</p>")[0]
        date = re.findall(date_pattern, date_str)[0].split('>')[2].strip().split('.')[0]
        year = date.split(' ')[1]
        year_dict[year] += 1

if len(year_dict) == 0 or len(co_author_dict) == 0:
    print('Sorry, your query for author: "' + author + '" produced no results.')
    quit()

min_year = int(min(year_dict, key=int))
max_year = int(max(year_dict, key=int))

x = np.arange(min_year, max_year+1)
y = np.zeros_like(x)

for year, count in year_dict.items():
    y[int(year)-min_year] = count

plt.figure()
xtick = x.copy()
plt.bar(x, y, align='center', tick_label=xtick)
plt.title('Publications per year')
plt.xlabel('Year')
plt.ylabel('Publications')
plt.yticks(range(0,max(y)+1))

for name, times in sorted(co_author_dict.items()):
    if name.lower() == author.lower():
        continue
    print('[ %s ]: %d times'%(name, times))

plt.show()