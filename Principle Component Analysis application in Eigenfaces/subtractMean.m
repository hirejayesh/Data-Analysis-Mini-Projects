function [Xmu, mu] = subtractMean(X)
    Xmu = zeros(size(X));
    mu = mean(X);
    for i =1:size(X,1)
        Xmu(i,:) = X(i,:) - mu;
    end
end
