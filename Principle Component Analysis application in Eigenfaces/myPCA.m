
function [U, S]= myPCA(Xmu)
    S = eig(cov(Xmu));
    S_sort = sort(S,'descend');
    [U, ~] = eig(cov(Xmu));
    
    %U_new is sorted U respective to sorted eighenvalue in descending order
    U_new = zeros(size(U));
    for i = 1:numel(S)
        U_new(:,i) = U(:,S==S_sort(i));
    end
    
    U = U_new;
    S = S_sort;
end
