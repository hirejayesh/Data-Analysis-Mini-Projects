
function Z = projectData(Xmu, U, K)
    Z = zeros(size(Xmu,1), K);
    for i = 1:size(Xmu,1)
        for j = 1:K
            Z(i,j) = dot(Xmu(i,:), transpose(U(:,K)));
        end
    end
end
