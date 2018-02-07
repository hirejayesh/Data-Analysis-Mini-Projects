data = load('DataForKmeans.mat');
data = data.Data ;
[C V] = mykmeans(data, 7);
C
%k_random_index = randperm(size(data,1))
