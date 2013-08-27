function J = cost(X, Y, Theta)

H = X * Theta';
J = sum((H - Y) .^ 2) / 2 * length(Theta);
