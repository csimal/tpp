import numpy as np
import matplotlib.pyplot as plt

def read_sextable(fp):
    men = np.zeros((105,101))
    women = np.zeros((105,101))
    next(fp)
    for line in fp:
        vals = line.split()
        year = int(vals[0])
        age = int(vals[1])
        men[age,year-2011] = int(vals[2])
        women[age,year-2011] = int(vals[3])
    return men, women

def table_age_grouped(table):
    t = np.zeros((20,101))
    for i in range(19):
        for j in range(5):
            for year in range(101):
                t[i,year] = t[i,year] + table[i*5+j,year]
    for age in range(95,105):
        for year in range(101):
           t[19,year] = t[19,year] + table[age,year]
    return t


inscodes = ['92094']#['93090','93088','93056','93022','93018','93014','93010','92142','92141','92140','92138','92137','92114','92101','92097','92094','92087','92054','92048','92045','92035','92006','92003','91143','91142','91141','91120','91114','91103','91072','91064','91059','91054','91034','91030','91015','91013','91005']
age_groups = ['0.4','5.9','10.14','15.19','20.24','25.29','30.34','35.39','40.44','45.49','50.54','55.59','60.64','65.69','70.74','75.79','80.84','85.89','90.94','95.']

year = 2030
i = year-2011

for ins in inscodes:
    data = open("result/"+str(1)+"/sextable_"+ins+".txt",'r')
    men, women = read_sextable(data)
    tmen = table_age_grouped(men)
    twomen = table_age_grouped(women)
    data.close()

    bm = tmen[:,i]
    bw = twomen[:,i]

    fig, ax = plt.subplots()
    index = np.arange(20)
    barwidth = 0.4

    pm = plt.bar(index, bm, barwidth, color='b', label='Hommes')
    pw = plt.bar(index + barwidth, bw, barwidth, color='r', label='Femmes')
    plt.xlabel("Groupe d'âge")
    plt.ylabel("Nombre d'individus")
    plt.title("Population totale par groupe d'âge en "+str(year)+" (INS:"+ins+')')
    plt.xticks(index + barwidth /2, age_groups, fontsize=5)
    plt.legend()
    plt.savefig('age_groups_'+str(year)+'_'+ins+'.png', dpi=150)
    plt.close()
