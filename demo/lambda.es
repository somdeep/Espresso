class Test {

      void print_int_array(int [5] arr, int len) {
      	   int i;
	   for(i = 0; i < len; i = i + 1) {
	   	 print_int(arr[i]);
		 print_string(" ");
	   }
	   print_string("\n");
      }

      int main() {
      	  class Sort s;
	  int [5] arr;
	  int [5] asc;
	  int [5] desc;
	  int i;

	  lambda : bool comp (int x, int y) { if (x < y) { return true; } return false; }
	  
	  print_string("........... original array ..........\n");
	  for(i = 0; i < 5; i = i + 1) {
	  	arr[i] = i % 3;
		print_int(arr[i]);
		print_string (" ");
	  }

	  print_string("\n..........sort descending............\n");
	  asc = s.sort(s, arr, comp);
	  print_int_array(asc, 5);

	  
      	  return 0;
      }

}


class Sort {
      int [5] a;
      int [5] sort(class Sort self, int [5] nums, lambda comp) {
      	  self.a = nums;
	  int i;
	  int j;
	  int temp;
	  int x;
	  int y;

	  // bubble sort
	  for(i = 0; i < 5; i = i + 1) {
	  	for(j = 0; j < 4 - i; j = j + 1) {
		      bool val;
		      x = self.a[j];
		      y = self.a[j + 1];
		      if (#comp(x,y) == true) {
		      	 temp = self.a[j];
			 self.a[j] = self.a[j + 1];
			 self.a[j + 1] = temp;
		      }
		}
	  }
	  
	  return self.a;
      }

}



