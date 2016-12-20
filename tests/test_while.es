class test_while
{
	int abc()
	{
		int i;
		i = 0;
		int sum;
		sum = 0;
		while(i<5)
		{
			sum = sum + i;
			i = i + 1;
		}
		return sum;
	}

	int main()
	{
		int a;
		class test_while obj;
		print_int(obj.abc());
		return 0;
	}
}
