import numpy as np
#import scipy as sp
import matplotlib.pyplot as plt

inscodes = ['93090','93088','93056','93022','93018','93014','93010','92142','92141','92140','92138','92137','92114','92101','92097','92094','92087','92054','92048','92045','92035','92006','92003','91143','91142','91141','91120','91114','91103','91072','91064','91059','91054','91034','91030','91015','91013','91005']

year = 2111
k = year-2011

table = np.load('sextable_tot.npy')

plt.figure(1)
plt.plot(range(1,201), table[15,1,:,100])
plt.xlabel('Graine')
plt.ylabel('Total femmes en '+str(year))
plt.show()
