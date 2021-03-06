function [matrix_m_LG matrix_t_LG matrix_a_LG matrix_m_MB matrix_t_MB matrix_a_MB] = res_process(sats, golden, tasks, nreps, resultsfiles)
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
prom_m_LG=(sum(data_m_LG)-max(data_m_LG)-min(data_m_LG))/(nreps-2);
prom_t_LG=(sum(data_t_LG)-max(data_t_LG)-min(data_t_LG))/(nreps-2);
prom_a_LG=(sum(data_a_LG)-max(data_a_LG)-min(data_a_LG))/(nreps-2);
prom_m_MB=(sum(data_m_MB)-max(data_m_MB)-min(data_m_MB))/(nreps-2);
prom_t_MB=(sum(data_t_MB)-max(data_t_MB)-min(data_t_MB))/(nreps-2);
prom_a_MB=(sum(data_a_MB)-max(data_a_MB)-min(data_a_MB))/(nreps-2);
for i = 1:sats
    for j = 1:golden
        for k = 1:tasks
            matrix_m_LG(i, j, k) = prom_m_LG(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_t_LG(i, j, k) = prom_t_LG(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_a_LG(i, j, k) = prom_a_LG(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_m_MB(i, j, k) = prom_m_MB(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_t_MB(i, j, k) = prom_t_MB(k + ((j-1)+(i-1)*golden)*tasks);
            matrix_a_MB(i, j, k) = prom_a_MB(k + ((j-1)+(i-1)*golden)*tasks);
        end
    end
end