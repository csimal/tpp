import numpy as np

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

def table_tot(table):
    t = np.zeros(101)
    for i in range(101):
        t[i] = table[:,i].sum()
    return t

inscodes = ['93090','93088','93056','93022','93018','93014','93010','92142','92141','92140','92138','92137','92114','92101','92097','92094','92087','92054','92048','92045','92035','92006','92003','91143','91142','91141','91120','91114','91103','91072','91064','91059','91054','91034','91030','91015','91013','91005']

table = np.zeros((38,2,200,101))

for i, ins in enumerate(inscodes):
    for j in range(200):
        data = open("result/"+str(j+1)+"/sextable_"+ins+".txt",'r')
        men, women = read_sextable(data)
        table[i,0,j] = table_tot(men)
        table[i,1,j] = table_tot(women)
        data.close()

np.save('sextable_tot',table)
