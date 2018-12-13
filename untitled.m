clc
clear

fid = fopen('erreur_ipf_92094.txt', 'r');
A = fscanf(fid, '%f', [1 Inf]);
fclose(fid);

figure
plot(2:32,A(2:32),'o')