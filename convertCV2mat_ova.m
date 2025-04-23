clearvars

folder = '/media/emba/emba-2/ML_BOKI/NeuroMiner';
load([folder filesep 'TrainInd.mat']);
load([folder filesep 'TestInd.mat']);
load([folder filesep 'TrainLabel.mat']);
load([folder filesep 'TestLabel.mat']);
load([folder filesep 'cvin.mat']);

p2  = size(cv1_all, 1);
cv2 = size(cv1_all, 2);
p1  = size(cv1_all{1}.TrainInd, 1);
cv1 = size(cv1_all{1}.TrainInd, 2);

Train_cv2     = cell(p2,cv2);
Test_cv2      = cell(p2,cv2);
class_cv2     = cell(p2,cv2);
class_new_cv2 = cell(p2,cv2);

for p = 1:p2
    for f = 1:cv2
        %% Outer Loop
        % create TrainInd and TestInd
        tr_ind = double(squeeze(Train_cv2_raw(p,f,:)));
        ts_ind = double(squeeze(Test_cv2_raw(p,f,:)));
        tr_label = double(squeeze(Train_cv2_label(p,f,:)));
        ts_label = double(squeeze(Test_cv2_label(p,f,:)));
        Train_cv2{p,f} = tr_ind;
        Test_cv2{p,f} = ts_ind;
        %% create the group comparisons
        class = {};
        class_new = {};
        % ASD-COMP vs. ALL : 1 vs. (2 + 3)
        tr_label_tmp = tr_label;
        tr_label_tmp(tr_label_tmp==2 | tr_label_tmp==3) = -1;
        ts_label_tmp = ts_label;        
        ts_label_tmp(ts_label_tmp==2 | ts_label_tmp==3) = -1;
        class{1,1}.groups = 1;
        class{1,1}.groupdesc = 'ASD-COMP vs ALL';
        class{1,1}.ind   = tr_ind;
        class{1,1}.label = tr_label_tmp;
        class_new{1,1}.groups = 1;
        class_new{1,1}.groupdesc = 'ASD-COMP vs ALL';
        class_new{1,1}.ind = (1:length(ts_label_tmp)).'; 
        class_new{1,1}.label = ts_label_tmp;
        % BPD-COMP vs. ALL : 2 vs. (1 + 3)
        tr_label_tmp = tr_label;
        tr_label_tmp(tr_label_tmp==1 | tr_label_tmp==3) = -1;
        tr_label_tmp(tr_label_tmp==2) = 1;
        ts_label_tmp = ts_label;        
        ts_label_tmp(ts_label_tmp==1 | ts_label_tmp==3) = -1;
        ts_label_tmp(ts_label_tmp==2) = 1;
        class{2,1}.groups = 2;
        class{2,1}.groupdesc = 'BPD-COMP vs ALL';
        class{2,1}.ind   = tr_ind;
        class{2,1}.label = tr_label_tmp;
        class_new{2,1}.groups = 2;
        class_new{2,1}.groupdesc = 'BPD-COMP vs ALL';
        class_new{2,1}.ind = (1:length(ts_label_tmp)).'; 
        class_new{2,1}.label = ts_label_tmp;
        % COMP-COMP vs. ALL : 3 vs. (1 + 2)
        tr_label_tmp = tr_label;
        tr_label_tmp(tr_label_tmp==1 | tr_label_tmp==2) = -1;
        tr_label_tmp(tr_label_tmp==3) = 1;
        ts_label_tmp = ts_label;        
        ts_label_tmp(ts_label_tmp==1 | ts_label_tmp==2) = -1;
        ts_label_tmp(ts_label_tmp==3) = 1;
        class{3,1}.groups = 3;
        class{3,1}.groupdesc = 'COMP-COMP vs ALL';
        class{3,1}.ind   = tr_ind;
        class{3,1}.label = tr_label_tmp;
        class_new{3,1}.groups = 3;
        class_new{3,1}.groupdesc = 'COMP-COMP vs ALL';
        class_new{3,1}.ind = (1:length(ts_label_tmp)).'; 
        class_new{3,1}.label = ts_label_tmp;

        %% Inner Loop
        cv1_temp = cv1_all{p,f};
        Train_cv1_temp = cell(p1,cv1);
        Test_cv1_temp = cell(p1,cv1);
        for i = 1:3
            class{i,1}.TrainInd = cell(p1,cv1);
            class{i,1}.TestInd = cell(p1,cv1);
            class{i,1}.TrainLabel = cell(p1,cv1);
            class{i,1}.TestLabel = cell(p1,cv1);
        end
        
        for p_in = 1:p1
            for f_in = 1:cv1
                tr_in_ind = double(squeeze(cv1_temp.TrainInd(p_in,f_in,:)));
                ts_in_ind = double(squeeze(cv1_temp.TestInd(p_in,f_in,:)));
                tr_in_label = double(squeeze(cv1_temp.TrainLabel(p_in,f_in,:)));
                ts_in_label = double(squeeze(cv1_temp.TestLabel(p_in,f_in,:)));
                Train_cv1_temp{p_in,f_in} = tr_in_ind;
                Test_cv1_temp{p_in,f_in} = ts_in_ind;
                %% Group comparisons
                % ASD-COMP vs. ALL : 1 vs. (2 + 3)
                class{1,1}.TrainInd{p_in,f_in} = tr_in_ind;
                class{1,1}.TestInd{p_in,f_in} = ts_in_ind;
                tr_in_label_tmp = tr_in_label;
                ts_in_label_tmp = ts_in_label;
                tr_in_label_tmp(tr_in_label_tmp==2 | tr_in_label_tmp==3) = -1;
                ts_in_label_tmp(ts_in_label_tmp==2 | ts_in_label_tmp==3) = -1;
                class{1,1}.TrainLabel{p_in,f_in} = tr_in_label_tmp;
                class{1,1}.TestLabel{p_in,f_in} = ts_in_label_tmp;
                % BPD-COMP vs. ALL : 2 vs. (1 + 3)
                class{2,1}.TrainInd{p_in,f_in} = tr_in_ind;
                class{2,1}.TestInd{p_in,f_in} = ts_in_ind;
                tr_in_label_tmp = tr_in_label;
                ts_in_label_tmp = ts_in_label;
                tr_in_label_tmp(tr_in_label_tmp==1 | tr_in_label_tmp==3) = -1;
                ts_in_label_tmp(ts_in_label_tmp==1 | ts_in_label_tmp==3) = -1;
                tr_in_label_tmp(tr_in_label_tmp==2) = 1;
                ts_in_label_tmp(ts_in_label_tmp==2) = 1;
                class{2,1}.TrainLabel{p_in,f_in} = tr_in_label_tmp;
                class{2,1}.TestLabel{p_in,f_in} = ts_in_label_tmp;
                % COMP-COMP vs. ALL : 3 vs. (1 + 2)
                class{3,1}.TrainInd{p_in,f_in} = tr_in_ind;
                class{3,1}.TestInd{p_in,f_in} = ts_in_ind;
                tr_in_label_tmp = tr_in_label;
                ts_in_label_tmp = ts_in_label;
                tr_in_label_tmp(tr_in_label_tmp==1 | tr_in_label_tmp==2) = -1;
                ts_in_label_tmp(ts_in_label_tmp==1 | ts_in_label_tmp==2) = -1;
                tr_in_label_tmp(tr_in_label_tmp==3) = 1;
                ts_in_label_tmp(ts_in_label_tmp==3) = 1;
                class{3,1}.TrainLabel{p_in,f_in} = tr_in_label_tmp;
                class{3,1}.TestLabel{p_in,f_in} = ts_in_label_tmp;
            end
        end
        cv1_all{p,f}.TrainInd = Train_cv1_temp;
        cv1_all{p,f}.TestInd = Test_cv1_temp;
        fields = {'TrainLabel','TestLabel'};
        cv1_all{p,f} = rmfield(cv1_all{p,f},fields);
        % class_cv2{p,f} = cell(1,1);
        % class_cv2{p,f}{1,1} = class;
        class_cv2{p,f} = class;
        class_new_cv2{p,f} = cell(1,1);
        % class_new_cv2{p,f}{1,1} = class_new;
        class_new_cv2{p,f} = class_new;
    end
end

cv = {};
cv.TrainInd = Train_cv2;
cv.TestInd = Test_cv2;
cv.class = class_cv2;
cv.cvin = cv1_all;
cv.classnew = class_new_cv2;

save([folder filesep 'CVstruct_BOKI_ova.mat'], 'cv')