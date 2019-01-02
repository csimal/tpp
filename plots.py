import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm

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

def table_tot(table):
    t = np.zeros(101)
    for i in range(101):
        t[i] = table[:,i].sum()
    return t

tmen = np.zeros((200,101))
twomen = np.zeros((200,101))

inscodes = ['93090','93088','93056','93022','93018','93014','93010','92142','92141','92140','92138','92137','92114','92101','92097','92094','92087','92054','92048','92045','92035','92006','92003','91143','91142','91141','91120','91114','91103','91072','91064','91059','91054','91034','91030','91015','91013','91005']
years = range(2011,2112)

varm = np.zeros(38)
varw = np.zeros(38)

for j,ins in enumerate(inscodes):
    for i in range(200):
        data = open("result/"+str(i+1)+"/sextable_"+ins+".txt",'r')
        men, women = read_sextable(data)
        tmen[i] = table_tot(men)
        twomen[i] = table_tot(women)
        data.close()

    varm[j] = np.var(tmen[:,100])
    varw[j] = np.var(twomen[:,100])

    median_m = np.median(tmen,0)
    median_w = np.median(twomen,0)

    quant_m = np.quantile(tmen,[0.25,0.75],0)
    quant_w = np.quantile(twomen,[0.25,0.75],0)

    plt.figure(2)
    lm, = plt.plot(years,tmen[0],'b', label='Hommes')
    lw, = plt.plot(years,twomen[0],'r', label='Femmes')
    for i in range(1,200):
        plt.plot(years,tmen[i],'b')
        plt.plot(years,twomen[i],'r')
    mm, = plt.plot(years,median_m,'c--', label='Médiane Hommes')
    mw, = plt.plot(years,median_w,'--', label='Médiane Femmes', color='#f4d0ec')
    qm, = plt.plot(years,quant_m[0],'c:', label='Quartiles Hommes')
    plt.plot(years,quant_m[1],'c:')
    qw, = plt.plot(years,quant_w[0],':', label='Quartiles Femmes', color='#f4d0ec')
    plt.plot(years,quant_w[1],':', color='#f4d0ec')
    plt.title("Evolution du nombres d'hommes et de femmes (INS:"+ins+')')
    plt.legend(handles=[lm, lw, mm, mw, qm, qw])
    plt.xlabel('temps [années]')
    plt.ylabel('Nombre de personnes')
    plt.savefig('stability_'+ins+'.png', dpi=150)
    plt.close()


