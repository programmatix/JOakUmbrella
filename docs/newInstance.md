        String s = new String("hello");

         0: new           #2                  // class java/lang/String
         3: dup
         4: ldc           #3                  // String hello
         6: invokespecial #4                  // Method java/lang/String."<init>":(Ljava/lang/String;)V
         
For core types like String, the closest in Java is newInstance(\[args\]) I think.  Approaches:

1. Enter a special scanning mode on new, gathering arguments to pass to newInstance.
2. Don't use the JVM at all, and create all objects even String in our own JVM.  Scary...

For types in the code being handled by our JVM, could:

1. Somehow turn the JVMClassFile into a proper Java Class.  Not sure if this is possible, and would mean code in that class would also be execute by the actual JVM. Kinda defeats the point...
2. Manage the memory and classes ourselves.

Managing memory and classes would need:

* malloc enough space for the class.  Can malloc?  What is enough space?
No direct malloc equiv.  Closest is new byte[size].
JNI is possible hack.  E.g. https://stackoverflow.com/questions/32988798/memory-allocated-in-native-code-between-jni-calls  
* Call <init>.
* Pass around a reference to it.  What form?
* Allow calling funcs on that instance with invokevirtual.