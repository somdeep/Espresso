class test_return
{
	int abc()
	{
		int j;
		for(j=1;j<10;j=j+1)
		{
			int i;
			if (j == 5) {

			   break;
			}
		}
		return j;
	}

	int main(int a)
	{
		int a;
		class test_return obj;
		print_int(obj.abc());
		return 0;
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
