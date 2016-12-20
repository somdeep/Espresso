class Beer {
      void print_line1(int num_bottles) {
      	   if (num_bottles >= 2 && num_bottles <= 99) {
	      print_int(num_bottles);
	      print_string(" bottles of beer on the wall,");
	      print_int(num_bottles);
	      print_string(" bottles of beer.\n");
	      return ;
	   }
	   if (num_bottles == 1) {
	      print_int(num_bottles);
	      print_string(" bottle of beer on the wall,");
	      print_int(num_bottles);
	      print_string(" bottle of beer.\n");
	      return ;	      
	   }
	   if (num_bottles == 0) {
	      print_string("No more bottles of beer on the wall, no more bottles of beer.\n");
	      return ;
	   }
      }

      void print_line2(int num_bottles) {
      	   if (num_bottles == 99 ) {
	      print_string("Go to the store and buy some more, 99 bottles of beer on the wall.\n");
	   } else {
	     print_string ("Take one down and pass it around, ");
	     if (num_bottles == 1) {
	     	 print_int(num_bottles);
	     	 print_string(" bottle of beer on the wall.\n");
	      	 return ;
	     }
	     if (num_bottles == 0) {
	        print_string(" no more bottles of beer on the wall.\n");
		return ;
	     }
	     if (num_bottles >= 2 && num_bottles <= 98) {
	     	print_int(num_bottles);
	     	print_string(" bottles of beer on the wall.\n");
	     }
	   }
      }

      int main() {
      	  int num_bottles;
	  num_bottles = 99;
	  while(num_bottles >= 0) {
	     print_line1(num_bottles);
	     num_bottles = num_bottles - 1;
	     if (num_bottles < 0) {
	     	num_bottles = 99;
	     }
	     print_line2(num_bottles);
	     if (num_bottles == 99) {
	     	break;
	     }
	     print_string("\n");
	  }
      	  return 0;
      }
}


