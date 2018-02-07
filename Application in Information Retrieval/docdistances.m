function docdistances()
    fid1 = fopen('RedRidingHood.txt');
    fid2 = fopen('PrincessPea.txt');
    fid3 = fopen('Cinderella.txt');
    fid4 = fopen('CAFA1.txt');
    fid5 = fopen('CAFA2.txt');
    fid6 = fopen('CAFA3.txt');
    
    words1 = textscan(fid1, '%s');
    unique1 = unique(words1{1});
    words2 = textscan(fid2, '%s');
    unique2 = unique(words2{1});
    words3 = textscan(fid3, '%s');
    unique3 = unique(words3{1});
    words4 = textscan(fid4, '%s');
    unique4 = unique(words4{1});
    words5 = textscan(fid5, '%s');
    unique5 = unique(words5{1});
    words6 = textscan(fid6, '%s');
    unique6 = unique(words6{1});
    
    %creating cell array of all words of 6 documents
    total ={words1{1},words2{1}, words3{1}, words4{1}, words5{1}, words6{1}};
    
    unique_all = union(unique1,unique2);
    unique_all = union(unique_all, unique3);
    unique_all = union(unique_all, unique4);
    unique_all = union(unique_all, unique5);
    unique_all = union(unique_all, unique6);
    
    %initializing tf-idf matrix
    tf = zeros(6,length(unique_all));
    %currently storing only frequency of words in tf vector
    for i =1:6
        for j = 1:numel(unique_all)
            for m = 1:numel(total{i})
                if strcmp(unique_all{j}, total{i}(m))
                    tf(i,j) = tf(i,j) + 1;
                end
            end
        end
    end
    
    %calculting vector of frequency of words appearing in no. of docs
    doc_freq = zeros(numel(unique_all),1);
    for j =1:numel(unique_all)
        for m = 1:6
            if(tf(m,j)~= 0)
                doc_freq(j) = doc_freq(j)+1;
            end
        end
    end
    
    %calculating vector of inverse doc frequency
    X = log10(6./doc_freq);
    
    %calculating actual tf-idf vector
    for i = 1:6
        
        tf(i,:) = tf(i,:).* transpose(X);
    end
    
    %initializing cosine_distance matrix
    cos_dist= zeros(6,6);
    for i = 1:6
        for j = 1:6
            cos_dist(i,j) = pdist2(tf(i,:),tf(j,:),'cosine');
        end
    end
    
    %plotting the graph
    imagesc(cos_dist);
    colorbar;
    colormap('gray');
    a = {'RRH','PPea','Cinde','CAFA1','CAFA2','CAFA3'};
    set(axes, 'XTicklabels',a);
    set(axes, 'YTicklabels',a);
end
