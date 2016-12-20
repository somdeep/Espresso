class Node
{
	class Node left;
	class Node right;
	int val;

	void init(class Node self,class Node null)
	{
		self.left = null;
		self.right = null;		
		self.val = 1;
		
	}
}


class Tree
{
	

	void main()
	{
		class Node root;
		class Node null;
		null.val = -1;
		root.init(root,null);
		root.val = 1;
		//print_int(root.val);
		class Node root_left;
		root_left.init(root_left,null);
		root.left = root_left;
		class Node root_right;
		root_right.init(root_right,null);
		root.right = root_right;
		root_left.val = 2;
		root_right.val = 3;
		class Node root_far_left;
		class Node root_far_right;
		root_far_left.init(root_far_left,null);
		root_far_right.init(root_far_right,null);
		root_far_left.val = 4;
		root_far_right.val = 5;
		root_left.left = root_far_left;
		root_right.right = root_far_right;
		//print_int(root_left.val);
		print_string("\n");
		this.traverse(root);

		class Node test;
		test = root_left;
		/*if(test == root.left)
		{
			print_string("Equal");
		}	*/			

	}

	
	void traverse(class Node root)
	{
		class Node x;
		x=root;
		
		if(x.val == -1)
		{
			return;
		}
		traverse(x.left);	
		print_int(x.val);
		traverse(x.right);
	}



}
