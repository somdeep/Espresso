class test_return
{
	int main()
	{
		int a;
		class work obj;
		print_int(obj.handle());
		return 1;
	}
	
	String a()
	{
		return "a";
	}
}

class work
{
	int handle()
	{
		return 1;	
	}
}
