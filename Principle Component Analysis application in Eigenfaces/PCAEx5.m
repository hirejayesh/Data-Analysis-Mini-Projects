X = load('pcadata.mat');
%X is in format X:[50*2 double]
X = X.X;

plot(X(:,1),X(:,2),'bo');
hold on
xlim([0 7]);
ylim([2 8]);
[Xmu, mu] = subtractMean(X);
[U, S]=myPCA(Xmu);

%New_U is U with added mean to plot graph
New_U =zeros(size(U));
for i =1:size(U,1)
    New_U(i,:) = U(i,:) + mu;
end

plot(New_U(:,1),'r-');
plot(New_U(:,2),'g-');
title('Data points and their two principal components');

disp(U(:,1));

K = 1;
Z = projectData(Xmu, U, K);
disp(Z(1:3,:));

Xrec = recoverData(Z, U, K, mu);

plot(X(:,1),X(:,2),'bo');
hold on
xlim([0 7]);
ylim([2 8]);
plot(Xrec(:,1),Xrec(:,2),'r*');
title('Data points and their reconstruction');


%PART 2: pcafaces
X = load('pcafaces.mat');
X = X.X;
displayData(X(1:100, :));
[Xmu, mu] = subtractMean(X);
[U, S]=myPCA(Xmu);
K = 200;
Z = projectData(Xmu, U, K);
Xrec = recoverData(Z, U, K, mu);

%plotting original and recovered faces
subplot(2,2,1);
displayData(X(1:100, :));
title('Original faces');

subplot(2,2,2);
displayData(Xrec(1:100, :));
title('Recovered faces');
