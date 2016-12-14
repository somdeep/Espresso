
class work
{
	int a;

	int test() {
	    return 100;
	}
	
	void main()
	{
		
		int b;
		int c;
		int d;
		class animal an;
		this.a = test();
		String msg;
		msg = "this.a=";
		print_string(msg);
		print_int(this.a);
		print_string("\n");
		
		an.i = 5;
		this.a = an.perform(an.i);
		msg = "value from perform=";
		print_string(msg);
		print_int(this.a);
		print_string("\n");
	}
}


class animal
{
	char b;
	bool x;
	int i;
	
	int perform(int x)
	{
		this.i = x + this.i;
		return this.i*2;
	}
}


