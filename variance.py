import numpy as np
#import scipy as sp
import matplotlib.pyplot as plt

inscodes = ['93090','93088','93056','93022','93018','93014','93010','92142','92141','92140','92138','92137','92114','92101','92097','92094','92087','92054','92048','92045','92035','92006','92003','91143','91142','91141','91120','91114','91103','91072','91064','91059','91054','91034','91030','91015','91013','91005']

year = 2111
k = year-2011

stdm = np.zeros(38)
stdw = np.zeros(38)

table = np.load('sextable_tot.npy')

for i,_ in enumerate(inscodes):
    stdm[i] = np.std(table[i,0,:,k], dtype=np.float64)
    stdw[i] = np.std(table[i,1,:,k], dtype=np.float64)

print("coefficient of correlation: "+str(np.corrcoef(stdm,stdw)))

fig, ax = plt.subplots()
index = np.arange(38)
barwidth = 0.4

pm = plt.bar(index, stdm, barwidth, color='b', label = 'Hommes')
pw = plt.bar(index+barwidth, stdw, barwidth, color='r', label='Femmes')
plt.xlabel("Commune")
plt.ylabel('Ecart type')
plt.title("Ecart type par commune en "+str(year))
plt.xticks(index+barwidth /2, inscodes, rotation='vertical', fontsize=6)
plt.legend()
plt.show()

