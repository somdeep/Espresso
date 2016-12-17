class test_return
{
	int abc()
	{
		while(1<2 || 2>4)
		{
			break;
			return 3;
		}
		return 2;
	}

	int main()
	{
		int a;
		class test_return obj;
		print_int(obj.abc());
		return 1;
	}
	
	String a()
	{
		return "a";
	}
}

