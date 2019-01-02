import numpy as np
import matplotlib.pyplot as plt

data = open('Taux_fecondite.txt','r')

next(data)
next(data)

age = []
p = []

for i in range(34):
    line = next(data)
    vals = line.split(',')
    age.append(int(vals[0]))
    p.append(float(vals[1]))

data.close()

plt.figure(1)
plt.plot(age,p)
plt.ylabel('Taux de fécondité')
plt.xlabel('Age')
plt.title("Taux de fécondité en fonction de l'âge")
plt.savefig('taux_fecondite.png')
plt.show()

data = open('Taux_deces.txt','r')

next(data)

deathrates = np.zeros((2,105))

for line in data:
    vals = line.split()
    i = int(vals[0])
    deathrates[0,i] = float(vals[1])
    deathrates[1,i] = float(vals[2])

ages = range(105)

plt.figure(2)
m, = plt.plot(ages, deathrates[0,:], 'b', label='Hommes')
w, = plt.plot(ages, deathrates[1,:], 'r', label='Femmes')
plt.ylabel('Taux de décès')
plt.xlabel('Age')
plt.title("Taux de décès en fonction de l'âge")
plt.legend(handles = [m,w])
plt.savefig('taux_deces.png')
plt.show()
