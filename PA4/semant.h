#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include <map>
#include <list>
#include "cool-tree.h"
#include "cool-tree.handcode.h"
//#include "stringtab.h"
#include "/usr/class/include/PA4/stringtab.h"
//#include "symtab.h"
#include "/usr/class/include/PA4/symtab.h"
//#include "list.h"
#include "/usr/class/include/PA4/list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  bool CheckAncestor(Symbol ancestor, Symbol child, Symbol class_name);
  std::list<Symbol> GetInheritPath(Symbol now, Symbol class_name);
  Symbol GetCommonAncestor(Symbol class_A, Symbol class_B, Symbol class_name);
  std::map<Symbol, Class_> sym2class;
  void CheckErrorOrExit();
};


#endif

