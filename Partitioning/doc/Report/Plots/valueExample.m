function valueExample
	clc; clear;

	x = -4:4;
	y = [];
	for xi = x; y = [y evaluate(xi)]; end

	hold on;
	axis equal;

	plot(x, y, '-b');
	ylabel('y');
	xlabel('x');

	function y = evaluate(x)
		i = 2;
		if (x < -1) i = 1; end
		if (x > 1)  i = 3; end

		c = [-1, 0, -1];
		m = [-1, 0, 1];

		y = c(i) + m(i)*x;
	end
end

