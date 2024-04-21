#include <assert.h>
#include <ostream>
#include <stdio.h>
#include <map>
#include <vector>
#include "emit.h"
#include "cool-tree.h"
#include "/usr/class/include/PA5/symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0
#define STRING_TAG 4
#define INT_TAG    2
#define BOOL_TAG   3
#define MAX_BASIC_TAG_NUM 3

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassEnv;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;

   std::map<Symbol, int> class2tag;
   std::vector<CgenNodeP> classnodes;
   

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_prototype();
   void code_class_nametab();
   void code_class_objtab();
   void code_dispatch_tab();

   void code_class_init();
   void code_method_def();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);

   void build_class_info();
   void build_class_tag();

public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   CgenNodeP get_class_node(Symbol name);
   std::map<Symbol, int> get_class2tag(){ return class2tag; }
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   std::map<Symbol, Symbol> method2class;
   std::map<Symbol, int> method2idx;
   std::vector<method_class*> all_methods;

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
   int tag;

   std::vector<CgenNodeP> get_inherit_path();
   std::vector<method_class*> get_all_methods();
   std::vector<method_class*> get_now_methods();
   std::vector<attr_class*> get_all_attr();
   std::map<Symbol, Symbol> get_method2class();
   std::map<Symbol, int> get_method2idx();
   
   void code_init(ostream & str);
   void code_methods(ostream & str);
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

enum VariableType { Param, Local, Attr };
struct VarInfo {
   VariableType type;
   int idx;
};

class CgenClassEnv : public SymbolTable<Symbol, VarInfo>{
   CgenNodeP now_class;
   VariableType attr = VariableType::Attr;
   VariableType local = VariableType::Local;
   VariableType param = VariableType::Param;
   int attr_idx = 0, local_idx = 0, param_idx = 0;
public:
   CgenClassEnv(CgenNodeP now_class) : now_class(now_class) {
      //std::cout << "first enter "<< endl;
      enterscope();
      auto now_attrs = now_class->get_all_attr();
      for(int i = 0; i < now_attrs.size(); ++i) {
         auto attr_name = now_attrs[i]->name;
         AddID(attr_name,VariableType::Attr);
      }
   }
   void AddID(Symbol sym, VariableType type){
      auto* info = new VarInfo();
      info->type = type;
      if( type == attr ){
         info->idx = attr_idx++;
      } else if ( type == local ) {
         info->idx = local_idx++;
      } else if ( type == param ) {
         info->idx = param_idx++;
      }
      //std::cout << "add begin ";
      addid(sym, info);
   }
   VarInfo GetInfo(Symbol sym){
      auto now_info = lookup(sym);
      return *now_info;
   }
   void ExitScope(){
      //std::cout << "exit."<<endl;
      auto i = tbl;
      for(Scope *j = i->hd(); j != NULL; j = j->tl()) {
	     auto s = j->hd()->get_info();
        if( s->type == attr){
         attr_idx--;
        } else if (s->type == local){
         local_idx--;
        } else if (s->type == param){
         param_idx--;
        }
        delete s;
	   }
      exitscope();
   }

   void EnterScope(){
      //std::cout << "enter" <<endl;
      enterscope();
   }

   CgenNodeP get_class(){
      return now_class;
   }
};
