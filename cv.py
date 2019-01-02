import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt

inscodes = ['93090','93088','93056','93022','93018','93014','93010','92142','92141','92140','92138','92137','92114','92101','92097','92094','92087','92054','92048','92045','92035','92006','92003','91143','91142','91141','91120','91114','91103','91072','91064','91059','91054','91034','91030','91015','91013','91005']

year = 2111
k = year-2011

stdm = np.zeros(38)
stdw = np.zeros(38)
meanm = np.zeros(38)
meanw = np.zeros(38)
qm = np.zeros((2,38))
qw = np.zeros((2,38))
codm = np.zeros(38)
codw = np.zeros(38)

table = np.load('sextable_tot.npy')

for i,_ in enumerate(inscodes):
    stdm[i] = np.std(table[i,0,:,k])
    stdw[i] = np.std(table[i,1,:,k])
    meanm[i] = np.mean(table[i,0,:,k])
    meanw[i] = np.mean(table[i,0,:,k])
    qm[:,i] = np.quantile(table[i,0,:,k],[0.25,0.75])
    qw[:,i] = np.quantile(table[i,1,:,k],[0.25,0.75])

cvm = stdm/meanm
cvw = stdw/meanw
codm[:] = (qm[1,:]-qm[0,:])/(qm[0,:]+qm[1,:])
codw[:] = (qw[1,:]-qw[0,:])/(qw[0,:]+qw[1,:])


plt.figure(1)
n, bins, patches = plt.hist(table[15,1,:,k],50)
plt.xlabel("Nombre d'individus")
plt.ylabel("Effectif")
plt.title("Histogramme de la population en "+str(year)+' - '+inscodes[15])
plt.show()
plt.close()

fig, ax = plt.subplots()
index = np.arange(38)
barwidth = 0.4

pm = plt.bar(index, cvm, barwidth, color='b', label = 'Hommes')
pw = plt.bar(index+barwidth, cvw, barwidth, color='r', label='Femmes')
plt.xlabel("Commune")
plt.ylabel('Coefficient de Variation')
plt.title("Coefficient de Variation par genre et commune en "+str(year))
plt.xticks(index+barwidth /2, inscodes, rotation='vertical', fontsize=6)
plt.legend()
plt.show()

fig, ax = plt.subplots()
index = np.arange(38)
barwidth = 0.4

pm = plt.bar(index, codm, barwidth, color='b', label = 'Hommes')
pw = plt.bar(index+barwidth, codw, barwidth, color='r', label='Femmes')
plt.xlabel("Commune")
plt.ylabel('Coefficient de Dispersion')
plt.title("Coefficient de Dispersion par genre et commune en "+str(year))
plt.xticks(index+barwidth /2, inscodes, rotation='vertical', fontsize=6)
plt.legend()
plt.show()

