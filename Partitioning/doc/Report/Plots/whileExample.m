function whileExample 
	clc; clear;

	x = -2:6;
	y = [];
	for xi = x; y = [y evaluate(xi)]; end

	hold on;
	axis equal;

	plot(x, y, '-b');
	ylabel('y');
	xlabel('x');

	function y = evaluate(x)
		i = 1;
		ix = [0, 2, 4, inf];

		while (x >= ix(i))
			i = i + 1;
		end

		fprintf('%d, %d\n', x, i) 

		c = [0, 0, 4, 0];
		m = [0, 1, -1, 0];

		y = c(i) + m(i)*x;
	end
end

