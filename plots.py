import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm


def read_sextable(fp):
    men = [[0 for x in range(101)] for y in range(105)]
    women = [[0 for x in range(101)] for y in range(105)]
    next(fp)
    for line in fp:
        vals = line.split()
        year = int(vals[0])
        age = int(vals[1])
        men[age][year-2011] = int(vals[2])
        women[age][year-2011] = int(vals[3])
    return men, women

def table_age_grouped(table):
    t = [[0 for x in range(101)] for y in range(20)]
    for i in range(19):
        for j in range(5):
            for year in range(101):
                t[i][year] = t[i][year] + table[i*5+j][year]
    for age in range(95,105):
        for year in range(101):
           t[19][year] = t[19][year] + table[age][year]
    return t

def table_tot(table):
    t = [0 for x in range(101)]
    for i in range(101):
        for j in range(105):
            t[i] = t[i] + table[j][i]
    return t

tmen = []
twomen = []

for i in range(1,201):
    data = open("result/"+str(i)+"/sextable_92094.txt",'r')
    men, women = read_sextable(data)
    tmen.append(table_tot(men))
    twomen.append(table_tot(women))
    data.close()

print(len(tmen))

years = range(2011,2112)

#cmap105 = cm.get_cmap('viridis',105)
#cmap20 = cm.get_cmap('viridis',20)

#plt.figure(1)
#for i,x in enumerate(men):
#    plt.plot(years,x, color=cmap105(i))
#plt.title('Age vs Time')
#plt.show()

#men20 = table_age_grouped(men)

#plt.figure(2)
#for i,x in enumerate(men20):
#    plt.plot(years,x,color=cmap20(i))
#plt.title('Age Groups vs Time')
#plt.show

#tm = table_tot(men)
#tw = table_tot(women)

plt.figure(3)
for i in range(200):
    plt.plot(years,tmen[i],'b')
    plt.plot(years,twomen[i],'r')
plt.title('Total men/women vs Time')
plt.show()
  
