
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class TEMP {

};

class A {
  x : String <- "???";
  y : Int <- 1;
  init(z : Int, temp : Int) : Int { z + temp + 0 };
  kknd() : Bool { isvoid 0 };
};

class B inherits A{
  dump(x : Int, y : Int) : Int  { {x; y;} };
};

class C inherits B {
};

class D inherits A {

};

class E {

};

class Main {
  x : B ;
  main(): Object { case new C of 
    xx : A => (new IO).out_string("xx\n");
    ee : E => (new IO).out_string("ee\n");
    ff : TEMP => (new IO).out_string("ff\n");
    gg : Object => (new IO).out_string("gg\n");
    hh : IO => (new IO).out_string("hh\n");
    esac
 };
};

