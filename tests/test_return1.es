class test_return
{
	int main()
	{
		int a;
		class work obj;
		print_int(obj.handle());
		return 0;
	}
}

class work
{
	int handle()
	{
		return 1;	
	}
}
