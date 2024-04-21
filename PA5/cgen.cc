
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "/usr/class/assignments/PA5/cgen.h"
#include "/usr/class/include/PA5/cgen_gc.h"
#include "cool-tree.h"
#include "cool-tree.handcode.h"

#include <algorithm>
#include <iterator>
#include <functional>
#include <ostream>
#include <string>

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

int label_suffix = 0;
CgenClassTable *codegen_classtable;

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  codegen_classtable = new CgenClassTable(classes,os);
  codegen_classtable->code();
  codegen_classtable->exitscope();
  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_comment(char* message, ostream& s){
  s << "# " << message << endl;
}

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, const char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(const char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1,const char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD << Str << DISPTAB_SUFFIX;


 /***** Add dispatch information for class String ******/

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD << Int << DISPTAB_SUFFIX; 

 /***** Add dispatch information for class Int ******/

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD << Bool << DISPTAB_SUFFIX;

 /***** Add dispatch information for class Bool ******/

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = STRING_TAG /* Change to your String class tag here */;
   intclasstag =    INT_TAG /* Change to your Int class tag here */;
   boolclasstag =   BOOL_TAG /* Change to your Bool class tag here */;

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   // 
   build_class_info();

}

void CgenClassTable::code_prototype(){
  for(CgenNodeP now_node : classnodes){
    auto now_attrs = now_node->get_all_attr();

    auto now_sym = now_node->get_name();
    StringEntryP str_entry = stringtable.lookup_string(now_sym->get_string());

    str << WORD << "-1" << endl; // gc
    emit_protobj_ref(str_entry, str); str << LABEL;         // name
    str << WORD << now_node->tag << endl;                          // tag
    str << WORD << (DEFAULT_OBJFIELDS + now_attrs.size()) << endl; // size
    str << WORD ; emit_disptable_ref(str_entry, str); str << endl; // dispatch

    for(auto now_attr : now_attrs){
      //str << "# "<< now_attr->name << endl;
      if(now_attr->name == val){
        if(now_sym == Str){
          str << WORD; inttable.lookup_string("0")->code_ref(str); str << endl;
        } else {
          str << WORD << 0 << endl;
        }
      } else if (now_attr->name == str_field) {
        str << WORD << 0 << endl;
      } else {
        auto now_type = now_attr->type_decl;
        if(now_type == Int){
          str << WORD; inttable.lookup_string("0")->code_ref(str); str << endl;
        } else if (now_type == Bool){
          str << WORD; falsebool.code_ref(str); str << endl;
        } else if (now_type == Str){
          str << WORD; stringtable.lookup_string("")->code_ref(str); str << endl;
        } else { // void
          str << WORD << 0 << endl;
        }
      }
    }
    str << endl;
  }
}

void CgenClassTable::code_class_nametab(){
  str << CLASSNAMETAB << LABEL;
  for(CgenNodeP now_node: classnodes){
    auto now_sym = now_node->get_name();
    StringEntryP str_entry = stringtable.lookup_string(now_sym->get_string());

    str << WORD; str_entry->code_ref(str); str << endl;
  }
  str <<endl; // 可读性
}

void CgenClassTable::code_class_objtab(){
  str << CLASSOBJTAB << LABEL;
  
  for(CgenNodeP now_node : classnodes){
    auto now_sym = now_node->get_name();
    StringEntryP str_entry = stringtable.lookup_string(now_sym->get_string());

    str << WORD; emit_protobj_ref(str_entry, str); str << endl;
    str << WORD; emit_init_ref(str_entry, str); str << endl;
  }
}

void CgenClassTable::code_dispatch_tab(){
  for(CgenNodeP now_node : classnodes){
    auto now_sym = now_node->get_name();
    StringEntryP str_entry = stringtable.lookup_string(now_sym->get_string());

    emit_disptable_ref(str_entry, str); str << LABEL;

    auto now_methods = now_node->get_all_methods();
    auto now_method2class = now_node->get_method2class();
    for(auto now_method : now_methods){
      auto method_name = now_method->name;
      auto class_name = now_method2class[method_name];
      str << WORD; emit_method_ref(class_name, method_name, str); str << endl;
    }
  }
}

void CgenClassTable::code_class_init(){
  for (auto now_node : classnodes){
    now_node->code_init(str);
  }
}

void CgenClassTable::code_method_def(){
  for (auto now_node : classnodes){
    if(now_node->name != IO && now_node->name != Bool && now_node->name != Object
     && now_node->name != Int && now_node->name != Str)
    now_node->code_methods(str);
  }
}

void CgenClassTable::build_class_info(){
  for(auto l = nds; l; l = l->tl()){
    auto now_node = l->hd();
    //str << "# "<<now_node->get_name()->get_string()<<endl;
    classnodes.push_back(now_node);
  }
  std::reverse(classnodes.begin(), classnodes.end());
  build_class_tag();
}

void CgenClassTable::build_class_tag(){
  for(auto i = 0; i < classnodes.size(); ++i){
    classnodes[i]->tag = i;
    class2tag[ classnodes[i]->get_name() ] = classnodes[i]->tag;
  }
}



void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
	     Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
				   single_Formals(formal(arg, Str)),
				   Str, 
				   no_expr()))),
	    single_Features(method(substr, 
				   append_Formals(single_Formals(formal(arg, Int)), 
						  single_Formals(formal(arg2, Int))),
				   Str, 
				   no_expr()))),
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "coding constants" << endl;
  code_constants();

//                 Add your code to emit
//                   - prototype objects
//                   - class_nameTab
//                   - dispatch tables
//
  if (cgen_debug) cout << "coding class_nameTab" << endl;
  code_class_nametab();

  if (cgen_debug) cout << "coding class_objTab" << endl;
  code_class_objtab();
  
  if (cgen_debug) cout << "coding dispatch_tab" << endl;
  code_dispatch_tab();
  
  if (cgen_debug) cout << "coding prototype" << endl;
  code_prototype();

  if (cgen_debug) cout << "coding global_type" << endl;
  code_global_text();


  // class init and method.
  if (cgen_debug) cout << "coding class init" << endl;
  code_class_init();

  if (cgen_debug) cout << "coding method definition" << endl;
  code_method_def();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}

CgenNodeP CgenClassTable::get_class_node(Symbol name){
  for(auto now : classnodes){
    if (now->get_name() == name){
      return now;
    }
  }
  return nullptr;
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

std::vector<CgenNodeP> CgenNode::get_inherit_path(){
  std::vector<CgenNodeP> output;
  CgenNodeP now_node = this;
  while(now_node->get_name() != No_class){
    output.push_back(now_node);
    now_node = now_node->parentnd;
  }
  std::reverse(output.begin(), output.end());
  return output;
}

std::vector<method_class*> CgenNode::get_all_methods(){
  if(all_methods.empty()){
    auto inherit_path = get_inherit_path();
    for(auto now_node: inherit_path){
      auto now_features = now_node->features;
      for(auto i = now_features->first(); now_features->more(i); i = now_features->next(i)){
        auto now_feature = now_features->nth(i);
        //std::cout << "# "<<endl;
        if(now_feature->is_method()) {
          auto method_name = ((method_class*)now_feature)->name;
          if (method2idx.count(method_name) == 0){
            method2idx[method_name] = all_methods.size(); 
            all_methods.push_back((method_class*)now_feature);
          } else {
            all_methods[method2idx[method_name]] = (method_class*)now_feature;
          }
          method2class[method_name] = now_node->get_name();
        }
      }
    }
  }
  return all_methods;
}

std::vector<attr_class*> CgenNode::get_all_attr(){
  std::vector<attr_class*> output;
  auto inherit_path = get_inherit_path();
  for(auto now_node: inherit_path){
    auto now_features = now_node->features;
    for(auto i = now_features->first(); now_features->more(i); i = now_features->next(i)){
      auto now_feature = now_features->nth(i);
      
      if(!now_feature->is_method()) {
        output.push_back((attr_class*)now_feature);
      }
    }
  }
  return output; 
}

std::map<Symbol, Symbol> CgenNode::get_method2class(){
  if(method2class.empty()){
    get_all_methods();
  }
  return method2class;
}

std::map<Symbol, int> CgenNode::get_method2idx(){
  if(method2idx.empty()){
    get_all_methods();
  }
  return method2idx;
}

std::vector<method_class*> CgenNode::get_now_methods(){
  std::vector<method_class*> output;
  for(auto i = features->first(); features->more(i); i = features->next(i)){
    auto now_feature = features->nth(i);
    if(now_feature->is_method()){
      output.push_back((method_class*)now_feature);
    }
  }
  return output;
}

void CgenNode::code_init(ostream &str){
  auto now_sym = name;
  
  emit_init_ref(idtable.add_string(now_sym->get_string()), str); str << LABEL;
  emit_addiu(SP, SP, -12, str);
  emit_store(FP, 3, SP, str);
  emit_store(RA, 2, SP, str);
  emit_store(SELF, 1, SP, str);
  emit_addiu(FP, SP, 4, str);

  emit_move(SELF, ACC, str);

  if(parentnd->get_name() != No_class){
    str << JAL << parentnd->get_name() << CLASSINIT_SUFFIX << endl;
  }
  CgenClassEnv* env = new CgenClassEnv(this);
  //env->EnterScope();

  auto now_attrs = get_all_attr();
  auto now_idx = 3;
  for(auto now_attr : now_attrs){
    if(now_attr->init->is_empty()){
      if (now_attr->type_decl == Str ){
        emit_load_string(ACC, stringtable.lookup_string(""), str);
        emit_store(ACC, now_idx++, SELF, str);
      } else if(now_attr->type_decl == Int){
        emit_load_int(ACC,inttable.lookup_string ("0"), str);
        emit_store(ACC, now_idx++, SELF, str);
      } else if(now_attr->type_decl == Bool){
        emit_load_bool(ACC, BoolConst(0), str);
        emit_store(ACC, now_idx++, SELF, str);
      } else {
        emit_move(ACC, ZERO, str);
        emit_store(ACC, now_idx++, SELF, str);
      }
    } else {
      now_attr->init->code(str, env);
      emit_store(ACC, now_idx++ , SELF, str);
      if(cgen_Memmgr != GC_NOGC){
        emit_addiu(A1, SELF, 4 * now_idx, str);
        emit_gc_assign(str);
      }
    }
  }

  emit_move(ACC, SELF, str);

  emit_load(FP, 3, SP, str);
  emit_load(RA, 2, SP, str);
  emit_load(SELF, 1, SP, str);
  emit_addiu(SP, SP, 12, str);
  emit_return(str);
  str << endl;
  env->ExitScope();
  delete env;
}

void CgenNode::code_methods(ostream &str){
  CgenClassEnv* env = new CgenClassEnv(this);
  auto now_methods = get_now_methods();
  for(auto now_method : now_methods) {
    env->EnterScope();
    auto method_name = now_method->name;
    auto method_formals = now_method->formals; 
    auto formals_size = method_formals->len();
    for(auto i = method_formals->first(); method_formals->more(i); i = method_formals->next(i)){
      auto method_formal = method_formals->nth(i);
      env->AddID(method_formal->get_name(), VariableType::Param);
    }

    emit_method_ref(get_name(), now_method->name, str); str << LABEL;
    emit_addiu(SP, SP, -12 , str);
    emit_store(FP, 3 , SP, str);
    emit_store(SELF, 2, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, 4, str);
    emit_move(SELF, ACC, str);

    now_method->expr->code(str, env);

    //emit_move(ACC, SELF, str);
    emit_load(FP, 3 , SP, str);
    emit_load(SELF, 2 , SP, str);
    emit_load(RA, 1, SP, str);
    emit_addiu(SP, SP, 12 + (formals_size) * 4, str);
    emit_return(str);
    env->ExitScope();
  }
  delete env;
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

void assign_class::code(ostream &str, CgenClassEnv* env) {
  expr->code(str, env);
  auto info = env->GetInfo(name);
  if(info.type == VariableType::Attr){
    emit_store(ACC, info.idx + 3, SELF, str);
    if(cgen_Memmgr != GC_NOGC){
      emit_addiu(A1, SELF, 4 * (info.idx + 3), str);
      emit_gc_assign(str);
    }
  } else if(info.type == VariableType::Local){
    emit_store(ACC, info.idx + 1, SP, str);
    if(cgen_Memmgr != GC_NOGC){
      emit_addiu(A1, SP, 4 * (info.idx + 1), str);
      emit_gc_assign(str);
    }
  } else if(info.type == VariableType::Param){
    emit_store(ACC, info.idx + 3, FP, str);
    if(cgen_Memmgr != GC_NOGC){
      emit_addiu(A1, FP, 4 * (info.idx + 3), str);
      emit_gc_assign(str);
    }
  } else {
    emit_comment("assign error", str);
  }
}

void static_dispatch_class::code(ostream &str, CgenClassEnv* env) {
  for(auto i = actual->first(); actual->more(i); i =  actual->next(i)){
    auto param = actual->nth(i);
    param->code(str, env);
    emit_push(ACC, str);
  }

  expr->code(str, env);

  auto now_label = label_suffix++;
  emit_bne(ACC, ZERO, now_label, str);
  emit_partial_load_address(ACC, str); str << "str_const0" << endl;
  emit_load_imm(T1, get_line_number(), str);
  emit_jal("_dispatch_abort", str);

  emit_label_def(now_label, str);

  std::string disptab = type_name->get_string();
  disptab += DISPTAB_SUFFIX;
  emit_load_address(T1, disptab.c_str(), str);

  auto now_node = codegen_classtable->get_class_node(type_name);

  if(now_node == nullptr){
    str << "# error in dispatch." << endl;
    return;
  }
  auto now_idx = now_node->get_method2idx()[name];
  emit_load(T1, now_idx, T1, str);

  emit_jalr(T1, str);
}

void dispatch_class::code(ostream &str, CgenClassEnv* env) {
  for(auto i = actual->first(); actual->more(i); i =  actual->next(i)){
    auto param = actual->nth(i);
    param->code(str, env);
    emit_push(ACC, str);
  }

  expr->code(str, env);

  auto now_label = label_suffix++;
  emit_bne(ACC, ZERO, now_label, str);
  emit_partial_load_address(ACC, str); str << "str_const0" << endl;
  emit_load_imm(T1, get_line_number(), str);
  emit_jal("_dispatch_abort", str);

  emit_label_def(now_label, str);
  emit_load(T1, 2, ACC, str);
  auto now_class = env->get_class()->name;
  if(expr->get_type() != SELF_TYPE){
    now_class = expr->get_type();
  }
  // from dispatch table: correct method.
  //auto method2idx = env->get_class()->get_method2idx();
  //str << "# "<< name << endl;
  //for (auto now : method2idx){
  //  str << "# " << now.first<< ' '<< now.second << endl;
  //}
  auto now_node = codegen_classtable->get_class_node(now_class);

  if(now_node == nullptr){
    str << "# error in dispatch." << endl;
    return;
  }
  auto now_idx = now_node->get_method2idx()[name];
  emit_load(T1, now_idx, T1, str);

  emit_jalr(T1, str);
}

void cond_class::code(ostream &str, CgenClassEnv* env) {
  int then_label = label_suffix++;
  int else_label = label_suffix++;
  pred->code(str, env);
  // bool: value offset is 3.
  emit_load(T1, 3, ACC, str);
  
  emit_beqz(T1, else_label, str);
  
  then_exp->code(str, env);
  emit_branch(then_label, str);

  emit_label_def(else_label, str);
  else_exp->code(str, env);
  emit_label_def(then_label, str);
}

void loop_class::code(ostream &str, CgenClassEnv* env) {
  int label1 = label_suffix++;
  int label2 = label_suffix++;
  
  emit_label_def(label1, str);
  pred->code(str, env);
  emit_load(T1, 3, ACC, str);
  emit_beq(T1, ZERO, label2, str);
  body->code(str, env);
  emit_branch(label1, str);

  emit_label_def(label2, str);
  emit_move(ACC, ZERO, str);
}

void typcase_class::code(ostream &str, CgenClassEnv* env) {
  expr->code(str, env);

  // abort1: expr is void.
  auto abort2_label = label_suffix++;
  emit_bne(ACC, ZERO, abort2_label, str);
  emit_load_imm(T1, get_line_number(), str);
  emit_load_address(ACC, "str_const0", str);
  emit_jal("_case_abort2", str);

  emit_label_def(abort2_label, str);
  emit_load(T2, 0, ACC, str);
  
  auto finish_label = label_suffix++;
  auto abort_label = label_suffix++;

  // definition begin.
  struct TreeRange {
    int root_tag;
    int max_tag;
    branch_class* root_class;
  };
  
  auto class2tag = codegen_classtable->get_class2tag();

  std::function<int(const CgenNodeP)> get_max_tag;
  get_max_tag = [&](const CgenNodeP now_node){
    auto max_tag = std::max(0, class2tag[now_node->get_name()]);
    for(auto i = now_node->get_children(); i; i = i->tl()){
      auto now_child = i->hd();
      max_tag = std::max(max_tag, class2tag[now_child->get_name()]);
      get_max_tag(now_child);
    }
    return max_tag;
  };
  // definition end.

  std::vector<TreeRange> all_cases;
  for(auto i = cases->first(); cases->more(i); i = cases->next(i)){
    auto now_case = (branch_class*)cases->nth(i);
    auto now_type = now_case->type_decl;
    auto now_class_node = codegen_classtable->get_class_node(now_type);
    auto now_treerange = TreeRange{class2tag[now_type], get_max_tag(now_class_node), now_case};
    str << "# now_case: "<< now_type << ' ' << now_treerange.root_tag << ' '<< now_treerange.max_tag<<endl;
    all_cases.push_back(now_treerange);
  }

  std::sort(all_cases.begin(), all_cases.end(), [](const TreeRange& a, const TreeRange&b){
    return a.root_tag > b.root_tag;
  });

  for(auto i = 0; i < all_cases.size(); ++i){
    int next_label;
    if(i == all_cases.size()-1){
      next_label = abort_label;
    } else {
      next_label = label_suffix++;
    }
    auto now_treerange = all_cases[i];
    emit_blti(T2, now_treerange.root_tag, next_label, str);
    emit_bgti(T2, now_treerange.max_tag, next_label, str);

    emit_push(ACC, str);
    env->EnterScope();
    env->AddID(now_treerange.root_class->name , VariableType::Local);
    
    now_treerange.root_class->expr->code(str, env);
    
    emit_addiu(SP, SP, 4, str);
    emit_branch(finish_label, str);
    emit_label_def(next_label, str);
  }

  emit_jal("_case_abort", str);
  emit_label_def(finish_label, str);
}

void block_class::code(ostream &str, CgenClassEnv* env) {
  for(auto i = body->first(); body->more(i); i = body->next(i)){
    body->nth(i)->code(str, env);
  }
}

void let_class::code(ostream &str, CgenClassEnv* env) {

  if(init->is_empty()){
    if (type_decl == Str){
      emit_load_string(ACC, stringtable.lookup_string(""), str);
    } else if (type_decl == Int) {
      emit_load_int(ACC,inttable.lookup_string("0"), str);
    } else if (type_decl == Bool) {
      emit_load_bool(ACC, BoolConst(0), str);
    } else {
      emit_move(ACC, ZERO, str);
    }
  } else {
    init->code(str, env);
  }

  emit_push(ACC, str);
  
  env->EnterScope();
  env->AddID(identifier, VariableType::Local);

  body->code(str, env);

  emit_addiu(SP, SP, 4, str);
  env->ExitScope();
}

void plus_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  emit_push(ACC, str);

  e2->code(str, env);
  emit_jal("Object.copy", str);
  emit_load(T1, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
  emit_move(T2, ACC, str);
  
  emit_load(T1, 3, T1, str);
  emit_load(T2, 3, T2, str);

  emit_add(T1, T1, T2, str);
  emit_store(T1, 3, ACC, str);
}

void sub_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  emit_push(ACC, str);

  e2->code(str, env);
  emit_jal("Object.copy", str);
  emit_load(T1, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
  emit_move(T2, ACC, str);
  
  emit_load(T1, 3, T1, str);
  emit_load(T2, 3, T2, str);

  emit_sub(T1, T1, T2, str);
  emit_store(T1, 3, ACC, str);
}

void mul_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  emit_push(ACC, str);

  e2->code(str, env);
  emit_jal("Object.copy", str);
  emit_load(T1, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
  emit_move(T2, ACC, str);
  
  emit_load(T1, 3, T1, str);
  emit_load(T2, 3, T2, str);

  emit_mul(T1, T1, T2, str);
  emit_store(T1, 3, ACC, str);
}

void divide_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  emit_push(ACC, str);

  e2->code(str, env);
  emit_jal("Object.copy", str);
  emit_load(T1, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
  emit_move(T2, ACC, str);
  
  emit_load(T1, 3, T1, str);
  emit_load(T2, 3, T2, str);

  emit_div(T1, T1, T2, str);
  emit_store(T1, 3, ACC, str);
}

void neg_class::code(ostream &str,CgenClassEnv* env) {
  e1->code(str, env);
  emit_jal("Object.copy", str);

  // value to t1
  emit_load(T1, 3, ACC, str);
  // operate neg.
  emit_neg(T1, T1, str);
  // store to acc
  emit_store(T1, 3, ACC, str);
}

void lt_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  // e1: sp.
  emit_push(ACC, str);

  e2->code(str, env);
  // e1 --> t1.
  emit_load(T1, 1, SP, str);
  // pop.
  emit_addiu(SP, SP, 4, str);
  // a0(e2) --> t2
  emit_move(T2, ACC, str);

  emit_load(T1, 3, T1, str);
  emit_load(T2, 3, T2, str);

  emit_load_bool(ACC, BoolConst(1), str);
  emit_blt(T1, T2, label_suffix, str);
  emit_load_bool(ACC, BoolConst(0), str);

  emit_label_def(label_suffix++, str);
}

void eq_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  emit_push(ACC, str);

  e2->code(str, env);

  emit_addiu(SP, SP, 4, str);
  emit_load(T1, 0, SP, str);
  emit_move(T2, ACC, str);

  if(e1->type == Int || e1->type == Str || e1->type == Bool ){
    if(e2->type == Int || e2->type == Str || e2->type == Bool) {
      emit_load_bool(ACC, BoolConst(1), str);
      emit_load_bool(A1, BoolConst(0), str);
      emit_jal("equality_test", str);

      return;
    }
  }

  emit_load_bool(ACC, BoolConst(1), str);
  emit_beq(T1, T2, label_suffix, str);
  emit_load_bool(ACC, BoolConst(0), str);

  emit_label_def(label_suffix++, str);
}

void leq_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  emit_push(ACC, str);

  e2->code(str, env);
  emit_load(T1, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
  emit_move(T2, ACC, str);

  emit_load(T1, 3, T1, str);
  emit_load(T2, 3, T2, str);

  emit_load_bool(ACC, BoolConst(1), str);
  emit_bleq(T1, T2, label_suffix, str);
  emit_load_bool(ACC, BoolConst(0), str);

  emit_label_def(label_suffix++, str);
}

void comp_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  emit_load(T1, 3, ACC, str);

  emit_load_bool(ACC, BoolConst(1), str);
  // zer0: return 1 else 0.
  emit_beqz(T1, label_suffix, str);
  emit_load_bool(ACC, BoolConst(9), str);

  emit_label_def(label_suffix++, str);
}

void int_const_class::code(ostream& s, CgenClassEnv* env)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenClassEnv* env)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenClassEnv* env)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &str, CgenClassEnv* env) {
  if(type_name == SELF_TYPE){
    //  查查表
    emit_load_address(T1, "class_objTab", str);
    emit_load(T2, 0, SELF, str);
    emit_sll(T2, T2, 3, str);
    // T1: now_class.init
    emit_addu(T1, T1, T2, str);
    // push
    emit_push(T1, str);

    emit_load(ACC, 0, T1, str);
    emit_jal("Object.copy", str);
    // pop        
    emit_load(T1, 1, SP, str);
    emit_addiu(SP, SP, 4, str);
    
    // init
    emit_load(T1, 1, T1, str);
    emit_jalr(T1, str);
    return;
  }
  std::string type_name = this->type_name->get_string();
  std::string class_protobj = type_name + PROTOBJ_SUFFIX;
  std::string type_init = type_name + CLASSINIT_SUFFIX;
  emit_load_address(ACC, class_protobj.c_str(), str);
  emit_jal("Object.copy", str);
  emit_jal(type_init.c_str(), str);

}

void isvoid_class::code(ostream &str, CgenClassEnv* env) {
  e1->code(str, env);
  // t1: expression
  emit_move(T1, ACC, str);
  // true.
  emit_load_bool(ACC, BoolConst(1), str);
  // if zero: label1
  emit_beqz(T1 , label_suffix, str);
  emit_load_bool(ACC, BoolConst(0), str);
  emit_label_def(label_suffix++, str); 
}

void no_expr_class::code(ostream &str, CgenClassEnv* env) {
  emit_move(ACC, ZERO, str);
}

void object_class::code(ostream &str, CgenClassEnv* env) {
  auto info = env->GetInfo(name);
  if (name == self) {
    emit_comment("init.", str);
    emit_move(ACC, SELF, str);
  } else if(info.type == VariableType::Attr){
    str << "# attr, idx = " << info.idx << endl;
    emit_load(ACC , info.idx + 3, SELF, str);
    if(cgen_Memmgr != GC_NOGC){
      emit_addiu(A1, SELF, 4 * (info.idx + 3), str);
      emit_gc_assign(str);
    }

  } else if (info.type == VariableType::Local){
    str << " local, idx = " << info.idx << endl;
    emit_load(ACC, info.idx + 1, SP, str);
    if(cgen_Memmgr != GC_NOGC){
      emit_addiu(A1, SP, 4 * (info.idx + 1), str);
      emit_gc_assign(str);
    }

  } else if (info.type == VariableType::Param) {
    str << "# params, idx = " << info.idx << endl;
    emit_load(ACC, info.idx + 3, FP, str);    
    if(cgen_Memmgr != GC_NOGC){
      emit_addiu(A1, FP, 4 * (info.idx + 3), str);
      emit_gc_assign(str);
    }

  } else {
    str << "# object error." << endl;
  }
}


