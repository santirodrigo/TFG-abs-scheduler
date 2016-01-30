function [matrix_m_LG matrix_t_LG matrix_a_LG matrix_m_MB matrix_t_MB matrix_a_MB] = error_process(sats, golden, tasks, nreps, resultsfiles)
for i = 1:nreps
    DELIMITER = '-';
    HEADERLINES = 1;

    % Import the file
    newData = importdata(resultsfiles(i,:), DELIMITER, HEADERLINES);
    data = newData.data;
    data_m_LG(i,:) = data(:,1);
    data_t_LG(i,:) = data(:,2);
    data_a_LG(i,:) = data(:,3);
    data_m_MB(i,:) = data(:,4);
    data_t_MB(i,:) = data(:,5);
    data_a_MB(i,:) = data(:,6);
end
error_m_LG=std(data_m_LG);
error_t_LG=std(data_t_LG);
error_a_LG=std(data_a_LG);
error_m_MB=std(data_m_MB);
error_t_MB=std(data_t_MB);
error_a_MB=std(data_a_MB);
for i = 1:sats
    for j = 1:golden
        for k = 1:tasks
            matrix_m_LG(i, j, k) = error_m_LG(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_t_LG(i, j, k) = error_t_LG(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_a_LG(i, j, k) = error_a_LG(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_m_MB(i, j, k) = error_m_MB(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_t_MB(i, j, k) = error_t_MB(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_a_MB(i, j, k) = error_a_MB(k + ((j-1)+(i-1)*golden)*tasks);
        end
    end
end