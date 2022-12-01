#include <iostream>

int divide(int n, int x, int divisor)
{
	std::cout << "x: " << x << ", divisor: " << divisor << '\n';
	if(divisor == 1) return x;
	else if(x % divisor == 0) return divide(n, x, divisor - 1);
	else return divide(n, n + x, n);
}

int main(int argc, char** argv)
{
	int n{std::stoi(argv[1])};
	int result{divide(n, n, n)};
	std::cout << "Result: " << result << '\n';
}
