a = [];
k_cte = 1;
epsylon = 0.0001;
for i = 0:20 % i = DL
    for j = 0:20 % j = RT
        for k = 0:20 % k = t
            if k < i
                lambda = epsylon;
            else
                lambda = k_cte * (k - i);
            end
            if k < j
                gamma = epsylon;
            else
                gamma = (k - j);
            end
            a = [a;[lambda,gamma,1+exp(lambda/gamma)]];
        end
    end
end