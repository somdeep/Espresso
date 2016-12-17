
class work
{
	int a;	
	void main()
	{
		
		int b;
		int c;
		int d;
		String disp;
		disp = "abc";
		//print_string(disp);
		print_string("\n");
		int[10] arr;
		arr[0] = 1;
		c=(arr[0]);
		arr[1] = 2;
		arr[3]= 3;
		for(d=0;d<10;d=d+1)
		{
			arr[d] = d;
			print_int(arr[d]);
			print_string("\n");
		}
		for(d=0;d<5;d=d+1)
		{
			print_int(arr[d]);
			print_string("\n");
		}
		class animal obj;
		print_int(obj.work());	
	}		
}

class animal
{
	int b;
	int[10] arr;

	int work()
	{
		this.b = 500;
		return this.b;
	}
}
