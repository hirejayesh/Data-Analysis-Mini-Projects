%data loaded and passed as an agrgument as below:
%data = load('DataForKmeans.mat');
%data = data.Data ;
%[C V] = mykmeans(data, 7);
function [C, V] = mykmeans(data, k)
    %preallocating centroid vector
    C = zeros(k,2);
    k_random_index = randperm(size(data,1),k);
    %intializing centroid
    C = data(k_random_index,:);
    %preallocating V vector
    V = zeros(size(data,1),1);
    C2 = zeros(k,2);
    flag = 1;
    while(~isequal(C,C2))
        %flag used to avoid C assigned to C2(zero vector) at first iteration
        %from 2nd iteration onwards, C = C2
        if(flag ==0) 
            C = C2;
        end
        for i=1:size(data,1)
            %calculating nearest centroid
            [~, I]= min(pdist2(data(i,:),C));
            V(i) = I;
        end
        %new centroids calculation
        for i=1:k
            xi = data(V==i,:);
            count = size(xi,1);
            C2(i,:) = (1/count) * sum(xi);
        end
        flag = 0;
    end
    color = 'kbrgycmw'; 
    for i=1:k
        x = data(V==i,1);
        y = data(V==i,2);
        plot(x,y, strcat(color(i),'x'));
        x1 = C(i,1);
        x2 = C(i,2);
        %plot centroid as large red diamonds
        plot(x1,x2,'d','markerfacecolor','r','markersize',10);
        hold on
    end
end

