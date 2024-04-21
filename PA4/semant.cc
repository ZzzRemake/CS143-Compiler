#include <cstddef>
#include <map>
#include <set>
#include <vector>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "cool-tree.h"
#include "cool-tree.handcode.h"
#include "semant.h"

#include "/usr/class/include/PA4/utilities.h"
#include "/usr/class/include/PA4/symtab.h"
//#include "utilities.h"
//#include "symtab.h"


extern int semant_debug;
extern char *curr_filename;

static ClassTable *class_table;
static SymbolTable<Symbol, Symbol> *attr_table = nullptr;

typedef SymbolTable<Symbol, method_class> MethodTable;
static std::map<Symbol, MethodTable>  method_table;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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
    out_int_param,
    out_string_param,
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

    out_string_param = idtable.add_string("out_string_param");
    out_int_param = idtable.add_string("out_int_param");
}

static 


inline void AddIOMethod(){
    method_table[IO].enterscope();
    auto now_features = class_table->sym2class[IO]->GetFeatures();
    for(auto i = now_features->first(); now_features->more(i); i = now_features->next(i)){
        Feature now_feature = now_features->nth(i);
            if (!now_feature->IsMethod())
                continue;
            std::cerr << "Class IO"<< " <= "<<now_feature->GetName()<<std::endl;
            now_feature->AddMethod(IO);
    }
}

inline bool CheckExistType(Symbol now_type){
    return class_table->sym2class.count(now_type) || now_type==Object || now_type == SELF_TYPE || 
        now_type == Int || now_type == Str || now_type == Bool || now_type ;
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    // check inherit type is correct.
    if(classes->len() == 0){
        return;
    }
    std::cerr << "Building ClassTable..."<<std::endl;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        // 防止自指
        auto class_name = classes->nth(i)->GetName();
        if (class_name == classes->nth(i)->GetParent()) {
            semant_error(classes->nth(i)) << "Error: your parent is you!"<< std::endl;
            return;
        }
        if (class_name == SELF_TYPE) {
            semant_error(classes->nth(i)) << "Error: Can't use SELF_TYPE to be parent!"<< std::endl;
            return;
        }
        if (sym2class.count(class_name)) {
            semant_error(classes->nth(i)) << "Error: You have defined the class!"<< std::endl;
            return;
        }
        if(class_name == Int || class_name == Bool || class_name == IO || class_name == Str){
            semant_error(classes->nth(i)) << "Error: You can't define basic_class "<<class_name<<"!"<< std::endl;
            return;
        }
        // build global class info: name to class itself.
        sym2class[class_name] = classes->nth(i);
    }
    // add IO
    Features io_features = new single_list_node<Feature>(
        new method_class(in_int, new nil_node<Formal>(), Int, no_expr())
    );
    io_features = new append_node<Feature>(
        io_features,
        new single_list_node<Feature>(
            new method_class(in_string, new nil_node<Formal>(), Str, no_expr())
        )
    );
    io_features = new append_node<Feature>(
        io_features,
        new single_list_node<Feature>(
            new method_class(out_int, new single_list_node<Formal>(
                new formal_class(out_int_param, Int)
            ), SELF_TYPE, no_expr())
        )
    );
    io_features = new append_node<Feature>(
        io_features,
        new single_list_node<Feature>(
            new method_class(out_string, new single_list_node<Formal>(
                new formal_class(out_string_param, Str)
            ), SELF_TYPE, no_expr())
        )
    );

    sym2class[IO] = new class__class(IO, Object, io_features, classes->nth(classes->first())->GetFilename());

    std::cerr << "finish sym2class."<<std::endl;
    // check main.
    if (sym2class.count(Main) == 0) {
        semant_error() << "Error: Main out!"<< std::endl;
        return;
    }
    std::cerr << "finish main."<<std::endl;
    // check inherit.
    std::cerr << "Checking inherit..."<<std::endl;

    bool io_flag = false;

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        std::cerr << "Checking  " << classes->nth(i)->GetName()<<":" <<std::endl;
        auto now = classes->nth(i);
        auto parent = classes->nth(i)->GetParent();
        if(now->GetName() == Main){
            std::cerr <<"Main: "<< now->GetParent() <<std::endl;
        }
        while(parent != Object && parent != classes->nth(i)->GetName()) {
            if (parent == Int || parent == Str || parent == Bool || parent == Main) {
                semant_error(classes->nth(i)) << "Error: Your Class " << now->GetName() <<" can't inherit from " << parent <<"!"<< std::endl; return;
            }
            if (sym2class.count(parent) == 0 && parent != IO) { // IO特殊情况略
                semant_error(classes->nth(i)) << "Error: Where is your parent?"<< std::endl; return;
            }
            std::cerr << now->GetName() <<" <- ";
            
            if(now->GetParent() == IO){ // 特判IO
                std::cerr << IO <<std::endl;
                io_flag = true;
                break;
            }
            
            now = sym2class[parent];
            std::cerr << now->GetName() << std::endl;
            parent = now->GetParent();
        }

        if(io_flag){ // 特判IO
            continue;
        }
        
        if (parent != Object) {
            semant_error(classes->nth(i)) << "Error: Cycle in inherit!"<< std::endl; return;
        }
        std::cerr << now->GetName() <<" <- Object" <<std::endl;
    }
    return;
}

void ClassTable::CheckErrorOrExit() {
    if (class_table->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
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
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
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
	       filename);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

void method_class::AddMethod(Symbol class_name) {
    method_table[class_name].addid(name, new method_class(copy_Symbol(name), formals->copy_list(), copy_Symbol(return_type), expr->copy_Expression()));
}

void method_class::AddAttribute(Symbol class_name) { }

void method_class::CheckExprType(Symbol class_name) {
    std::cerr << "Checking class " <<class_name<< "'s method "<<name << "..."<<std::endl;
    // check return type exist.
    if(!CheckExistType(return_type)){
        class_table->semant_error(class_table->sym2class[class_name])
         << "Error: method "<< name << "'s type "<<return_type<<" doesn't exist." << std::endl;
        return;
    }
    // fix SELF_TYPE
    auto now_return_type = return_type;
    if(return_type == SELF_TYPE){
        now_return_type = class_name;
    }
    std::set<Symbol> name_set;
    attr_table->enterscope();

    // check formal.
    std::cerr << name <<"'s formal:"<<std::endl;
    for(auto i = formals->first(); formals->more(i); i = formals->next(i)){
        auto now_formal = formals->nth(i);
        if(!CheckExistType(now_formal->GetType()) || now_formal->GetType() == SELF_TYPE){
            class_table->semant_error(class_table->sym2class[class_name])
             << "Error: formal "<< name << "'s type doesn't exist." << std::endl;
        }

        if(name_set.count(now_formal->GetName())){
            class_table->semant_error(class_table->sym2class[class_name])
             << "Error: formal "<< name << " repeat." << std::endl;
            return;
        } else {
            name_set.insert(now_formal->GetName());
        }

        if(now_formal->GetName() == self){
           class_table->semant_error(class_table->sym2class[class_name])
            << "Error: formal name can't be self." << std::endl;
        }
        attr_table->addid(now_formal->GetName(), new Symbol(now_formal->GetType()));
    }

    // check expr return.
    Symbol now_type = expr->CheckExprType(class_name);
    if(!class_table->CheckAncestor(now_return_type, now_type, class_name)){
        class_table->semant_error(class_table->sym2class[class_name])
            << "Error: " <<now_type << " can't inherit from "<< now_return_type << std::endl;
    }
    std::cerr << "Finish " << class_name << " method checking."<<std::endl;
    attr_table->exitscope();
}

void attr_class::AddMethod(Symbol class_name) { } //empty

void attr_class::AddAttribute(Symbol class_name) {
    std::cerr << "Class "<< class_name << " <== " << name << std::endl;

    if(!CheckExistType(type_decl) || type_decl == SELF_TYPE){
        class_table->semant_error(class_table->sym2class[class_name])
         << "Error: attribute "<< name << "'s type "<<type_decl<<" doesn't exist." << std::endl;
        return;
    }

    Class_ now_class = class_table->sym2class[class_name];
    if (name == self) {
        class_table->semant_error(now_class) << "Error: self can't be attribute in Class"<< class_name << "!"<<std::endl;
        return;
    }
    if (attr_table->lookup(name)) {
        class_table->semant_error(now_class) << "Error: "<< name<<" has existed in "<< class_name << "!" <<std::endl;
        return;
    }
    // may be used in type check?
    attr_table->addid(name, new Symbol(type_decl));
}

void attr_class::CheckExprType(Symbol class_name) {
    std::cerr << "Checking class " <<class_name<< "'s attr "<<name << "..."<<std::endl;
    // check type.
    if(!CheckExistType(type_decl)){
        class_table->semant_error(class_table->sym2class[class_name])
            << "Error: " <<type_decl << " doesn't exist." << std::endl;
    }
    Symbol now_type = init->CheckExprType(class_name);
    if(now_type == No_type){
        std::cerr << "class " <<class_name<< "'s attr "<<name << " no init."<<std::endl;
        return;
    }
    
    if(!class_table->CheckAncestor(type_decl, now_type, class_name)){
        class_table->semant_error(class_table->sym2class[class_name])
            << "Error: " <<now_type << " can't inherit from "<< type_decl << std::endl;
    }
}

//expression type check.

Symbol assign_class::CheckExprType(Symbol class_name) {
    std::cerr << "Check assign in "<< name<<"."<<std::endl;
    auto ltype = attr_table->lookup(name);
    if(ltype == NULL){
        class_table->semant_error(class_table->sym2class[class_name]) << "Error: variant "<< name << "'s type doesn't exist!"<<std::endl;
        return Object;
    }
    auto rtype = expr->CheckExprType(class_name);
    std::cerr << "assign: "<< *ltype <<" "<< rtype <<std::endl;
    if(!class_table->CheckAncestor(*ltype, rtype, class_name)){
        class_table->semant_error(class_table->sym2class[class_name]) << "Error: assign in"<< name << "'s rtype can't be inherited from ltype!"<<std::endl;
        return Object;
    }
    set_type(*ltype);
    return *ltype;
}

Symbol block_class::CheckExprType(Symbol class_name) {
    std::cerr << "Check block expression."<<std::endl;
    if(body->len() == 0){
        std::cerr << "Error: where is your expression?" <<std::endl;
        set_type(Object);
        return Object;
    }
    Symbol now_type;
    for(auto i = body->first(); body->more(i); i = body->next(i)){
        auto now_expr = body->nth(i);
        now_type = now_expr->CheckExprType(class_name);
    }
    set_type(now_type);
    return get_type();
}

Symbol static_dispatch_class::CheckExprType(Symbol class_name){
    std::cerr << "Check static dispatch "<< name<<"."<<std::endl;
    bool now_error = false;
    Symbol expr_type = expr->CheckExprType(class_name);
    Symbol return_type;
    if(!CheckExistType(expr_type)){
        class_table->semant_error(class_table->sym2class[class_name]) << "Error: expr_type doesn't in sym2class!"<<std::endl;
        now_error = true;
    }

    method_class* path_method = nullptr;
    // check expr_type 是否有该method，且获得其method_class以方便后续检查。
    auto inherit_path = class_table->GetInheritPath(expr_type, class_name);
    for(auto path_class_name : inherit_path){
        if(path_class_name == type_name){
            std::cerr << "find static class:"<< type_name <<"."<<std::endl;
            path_method = method_table[type_name].lookup(name);
            break;
        }
    }
    if(path_method == nullptr){
        class_table->semant_error(class_table->sym2class[class_name]) << "Error: can't find "<<type_name <<"in inherit_path!"<<std::endl;
        now_error = true;
    } else {
        return_type = path_method->GetReturnType();
        if(actual->len() != path_method->GetFormals()->len()){
            class_table->semant_error(class_table->sym2class[class_name]) << "Error: actual and path_method have different length: "<<
                actual->len() <<" " << path_method->GetFormals()->len()<<std::endl;
            now_error = true;
        }
        for(auto i = actual->first(); !now_error && actual->more(i); i = actual->next(i)){
            if(i == actual->first()) continue;
            auto actual_expr_type = actual->nth(i)->CheckExprType(class_name);
            if(!class_table->CheckAncestor(path_method->GetFormals()->nth(i)->GetType(), actual_expr_type, class_name)){
                class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: "<<actual_expr_type << "doesn't match the formal type "<< path_method->GetFormals()->nth(i)->GetType()<<"."<<std::endl;
                now_error = true;
            }
        }
    }

    if(now_error){
        return_type = Object;
    }
    std::cerr << "static dispatch return "<< return_type << std::endl;
    set_type(return_type);
    return return_type;
}

Symbol dispatch_class::CheckExprType(Symbol class_name){
    std::cerr << "Check dispatch "<< name<<"."<<std::endl;
    bool now_error = false;
    Symbol expr_type = expr->CheckExprType(class_name);
    Symbol return_type;
    if(!CheckExistType(expr_type)){
        class_table->semant_error(class_table->sym2class[class_name]) << "Error: expr_type doesn't in sym2class!"<<std::endl;
        now_error = true;
    }

    method_class* path_method = nullptr;
    Symbol self_class = nullptr;
    // check expr_type 是否有该method，且获得其method_class以方便后续检查。
    auto inherit_path = class_table->GetInheritPath(expr_type, class_name);
    for(auto path_class_name : inherit_path){
        if(method_table.count(path_class_name) && method_table[path_class_name].lookup(name)){
            path_method = method_table[path_class_name].lookup(name);
            self_class = path_class_name;
        }
    }
    if(path_method == nullptr){
        class_table->semant_error(class_table->sym2class[class_name]) << "Error: can't find "<< name <<"in method_table!"<<std::endl;
        now_error = true;
    } else {
        return_type = path_method->GetReturnType();
        if(actual->len() != path_method->GetFormals()->len()){
            class_table->semant_error(class_table->sym2class[class_name]) << "Error: actual and path_method have different length: "<<
                actual->len() <<" " << path_method->GetFormals()->len()<<std::endl;
            now_error = true;
        }
        for(auto i = actual->first(); !now_error && actual->more(i); i = actual->next(i)){
            auto actual_expr_type = actual->nth(i)->CheckExprType(class_name);
            if(!class_table->CheckAncestor(path_method->GetFormals()->nth(i )->GetType(), actual_expr_type, class_name)){
                class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: "<<actual_expr_type << "doesn't match the formal type "<< path_method->GetFormals()->nth(i)->GetType()<<"."<<std::endl;
                now_error = true;
            }
        }
    }

    if(now_error){
        return_type = Object;
    } else {
        // 其依赖于path_method
        if(return_type == SELF_TYPE){
            std::cerr << "dispatch return "<< self_class <<"(SELF_TYPE): " << std::endl;
            set_type(self_class);
            return self_class;
        } 
    }
    std::cerr << "dispatch return "<< return_type << std::endl;
    set_type(return_type);
    return return_type;
}

Symbol cond_class::CheckExprType(Symbol class_name) {
    auto pred_type = pred->CheckExprType(class_name);
    if(pred_type != Bool){
        class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: cond's predicate type "<< pred_type << " is not Bool." << std::endl;
    }
    auto then_type = then_exp->CheckExprType(class_name);
    auto else_type = else_exp->CheckExprType(class_name);
    
    auto return_type = class_table->GetCommonAncestor(then_type, else_type, class_name);
    set_type(return_type);
    return return_type;
}

Symbol loop_class::CheckExprType(Symbol class_name) {
    auto pred_type = pred->CheckExprType(class_name);
    if(pred_type != Bool){
        class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: loop's predicate type "<< pred_type << " is not Bool." << std::endl;
    }
    body->CheckExprType(class_name);
    set_type(Object);
    return Object;
}

Symbol let_class::CheckExprType(Symbol class_name){
    if(identifier == self){
        class_table->semant_error(class_table->sym2class[class_name])
            << "Error: let's define can't have self identifier." << std::endl;
    }
    auto now_type_decl = type_decl;
    if(type_decl == SELF_TYPE){
        now_type_decl = class_name;
    }
    attr_table->enterscope();
    attr_table->addid(identifier, new Symbol(type_decl));

    auto init_type = init->CheckExprType(class_name);
    if(init_type != No_type){
        if(!class_table->CheckAncestor(type_decl, init_type, class_name)){
            class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: let identifier "<< identifier << "have different type: "<<type_decl <<" and "<< init_type<< std::endl;
        }
    }
    auto return_type = body->CheckExprType(class_name);
    attr_table->exitscope();
    
    set_type(return_type);
    return return_type;
}

Symbol typcase_class::CheckExprType(Symbol class_name) {
    Symbol expr_type = expr->CheckExprType(class_name);
    Symbol return_type = Object;
    std::vector<Symbol> case_types;
    std::vector<Symbol> case_type_decls;

    for (auto i = cases->first(); cases->more(i); i = cases->next(i)){
        auto now_case = cases->nth(i);
        auto now_case_type = now_case->CheckExprType(class_name);
        case_types.push_back(now_case_type);
        case_type_decls.push_back(now_case->GetTypeDecl());
    }
    if(case_types.size() == 0){
        std::cerr<< "Warning: case have no branch!" <<std::endl;
    }
    for(size_t i = 0; i < case_type_decls.size(); ++i){
        for(size_t j = i+1; j < case_type_decls.size();++j){
            if(case_type_decls[i] == case_type_decls[j]){
                class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: branch type can't have same type!"<<std::endl;
            }
        }
    }

    for(auto case_type_decl:case_type_decls){
        if(!class_table->CheckAncestor(case_type_decl, expr_type, class_name)){
            class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: case expr_type " <<case_type_decl <<"can't inherit from branch type "<<case_type_decl << "!"<< std::endl;
        }
    }
    if(case_types.empty()){
        return Object;
    }
    return_type = case_types[0];
    for(auto case_type: case_types){
        return_type = class_table->GetCommonAncestor(return_type, case_type, class_name);
    }

    set_type(return_type);
    return return_type;
}

Symbol branch_class::CheckExprType(Symbol class_name){
    attr_table->enterscope();
    attr_table->addid(name, new Symbol(type_decl));
    auto return_type = expr->CheckExprType(class_name);
    attr_table->exitscope();
    return return_type;
}

inline Symbol BinaryExprCheck(Symbol class_name, Expression a, Expression b, Symbol now_type){
    auto left_type = a->CheckExprType(class_name);
    auto right_type = b->CheckExprType(class_name);
    if(left_type != now_type || right_type != now_type){
         class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: binary operate can't solve the classes that are not "<< now_type <<std::endl;
        return Object;
    }
    return now_type;
}


Symbol plus_class::CheckExprType(Symbol class_name){
    set_type(BinaryExprCheck(class_name, e1, e2, Int));
    return get_type();
}

Symbol sub_class::CheckExprType(Symbol class_name){
    set_type(BinaryExprCheck(class_name, e1, e2, Int));
    return get_type();
}

Symbol mul_class::CheckExprType(Symbol class_name){
    set_type(BinaryExprCheck(class_name, e1, e2, Int));
    return get_type();
}

Symbol divide_class::CheckExprType(Symbol class_name){
    set_type(BinaryExprCheck(class_name, e1, e2, Int));
    return get_type();
}

Symbol neg_class::CheckExprType(Symbol class_name){
    auto neg_type = e1->CheckExprType(class_name);
    if(neg_type != Int){
        class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: neg operate can't solve the class "<< neg_type <<" that is not Int." <<std::endl;
        return Object;
    }
    return Int;
}

Symbol lt_class::CheckExprType(Symbol class_name){
    BinaryExprCheck(class_name, e1, e2, Int);
    set_type(Bool);
    return get_type();
}

Symbol leq_class::CheckExprType(Symbol class_name){
    BinaryExprCheck(class_name, e1, e2, Int);
    set_type(Bool);
    return get_type();
}

Symbol eq_class::CheckExprType(Symbol class_name){
    auto left_type = e1->CheckExprType(class_name);
    auto right_type = e2->CheckExprType(class_name);
    if(left_type == Int || left_type == Bool || left_type == Str){
        if(left_type == right_type){
            return Bool;
        } else {
            class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: eq operate can't solve the classes that are not same." <<std::endl;
            return Object;
        }
    }
    set_type(Bool);
    return get_type();
}

Symbol comp_class::CheckExprType(Symbol class_name){
    auto comp_type = e1->CheckExprType(class_name);
    if(comp_type != Bool){
        class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: comp operate can't solve the class "<< comp_type <<" that is not Bool." <<std::endl;
        return Object;
    }
    set_type(Bool);
    return get_type();
}

Symbol int_const_class::CheckExprType(Symbol class_name){
    set_type(Int);
    return Int;
}

Symbol string_const_class::CheckExprType(Symbol class_name){
    set_type(Str);
    return Str;
}

Symbol bool_const_class::CheckExprType(Symbol class_name){
    set_type(Bool);
    return Bool;
}

Symbol new__class::CheckExprType(Symbol class_name){
    auto test_type = (type_name == SELF_TYPE ? class_name : type_name);
    if(class_table->sym2class.count(test_type) == 0){
        class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: new operate can't solve the class that doesn't exist.." <<std::endl;
        return Object;
    }
    set_type(type_name);
    return get_type();
}

Symbol isvoid_class::CheckExprType(Symbol class_name){
    e1->CheckExprType(class_name);
    set_type(Bool);
    return Bool;
}

Symbol no_expr_class::CheckExprType(Symbol class_name){
    set_type(No_type);
    return No_type;
}

Symbol object_class::CheckExprType(Symbol class_name){
    if(name == self){
        std::cerr << "Object return: self."<<std::endl;
        set_type(SELF_TYPE);
        return get_type();
    }
    Symbol * type;
    if((type = attr_table->lookup(name))){
        set_type(*type);
        return get_type();
    } else {
        class_table->semant_error(class_table->sym2class[class_name])
                    << "Error: where is your class in Object? " <<std::endl;
        set_type(Object);
        return Object;
    }
}

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 

bool ClassTable::CheckAncestor(Symbol ancestor, Symbol child, Symbol class_name) {
    if(ancestor == SELF_TYPE){
        std::cerr << "CheckAncestor input the ancestor SELF_TYPE"<<std::endl;
        return child == SELF_TYPE || child == class_name;
    }
    if(child == SELF_TYPE){
        std::cerr << "CheckAncestor input the child SELF_TYPE"<<std::endl;
        child = class_name;
    }
    if(ancestor == Int || ancestor == Bool || ancestor == Str || child == Int || child == Bool || child == Str){
        if(ancestor != child){
            semant_error(sym2class[class_name])<< "Error: ancestor and child have different basic class.";
        }
        else return true;
    }

    auto now = child;
    
    while(now != No_class) {
        if (now == ancestor) {
            return true;
        }
        now = sym2class[now]->GetParent();
    }
    return false;
}

std::list<Symbol> ClassTable::GetInheritPath(Symbol now, Symbol class_name) {
    if(now == SELF_TYPE){
        now = class_name;        
    }
    std::list<Symbol> out;
    out.push_front(now);
    if (sym2class.count(now) == 0) {
        return out;
    }

    auto parent = sym2class[now]->GetParent();
    while(parent != Object && parent != IO) {
        out.push_front(parent);
        parent = sym2class[parent]->GetParent();
    }
    if(parent == IO){
        out.push_front(IO);
        out.push_front(Object);
    }
    return out;
}


// 状如：
//   Object
//     |
//    ...
//     |
//   Least
//   /   \/
//  A     B
Symbol ClassTable::GetCommonAncestor(Symbol class_A, Symbol class_B, Symbol class_name){
    if(class_A == SELF_TYPE){
        class_A = class_name;
    }
    if(class_B == SELF_TYPE){
        class_B = class_name;
    }
    if(class_A == class_B){
        return class_A;
    }
    auto A_inherit_path = GetInheritPath(class_A, class_name);
    auto B_inherit_path = GetInheritPath(class_B, class_name);
    Symbol least_common_ancestor = Object;
    
    for(auto A_iter = A_inherit_path.begin(), B_iter = B_inherit_path.begin();
        A_iter != A_inherit_path.end() && B_iter != B_inherit_path.end();
        A_iter++, B_iter++){
        if(*A_iter == *B_iter){
            least_common_ancestor = *A_iter;    
        } else {
            break;
        }
    }
    return least_common_ancestor;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */


void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    class_table = new ClassTable(classes);

    /* some semantic analysis code may go here */
    
    class_table->CheckErrorOrExit();

    std::cerr << "Finish inherits check." << std::endl;
    std::cerr << "============="<< std::endl;

    // insert class method info
    // method是会重载的，因此似乎只要查类型和formal就行？
    std::cerr << "Building method table..."<< std::endl;

    // IO 虽然是basic类，但他的方法仍然可以被覆盖，他自己也是可以被inherit的。
    AddIOMethod();

    for (auto i = classes->first(); classes->more(i); i = classes->next(i)) {
        auto now_class = classes->nth(i);
        if(now_class->GetName() == Int || now_class->GetName() == Bool || now_class->GetName() == Str || now_class->GetName() == Bool) { // CHECK Name (basic class)
            class_table->semant_error()<<"Why you define " <<now_class->GetName() <<" class?"<<std::endl;
            break;
        }
        method_table[now_class->GetName()].enterscope();
        Features now_features = now_class->GetFeatures();
        for (auto j = now_features->first(); now_features->more(j); j = now_features->next(j)) {
            Feature now_feature = now_features->nth(j);
            if (!now_feature->IsMethod())
                continue;
            std::cerr << "Class "<< now_class->GetName() << " <= "<<now_feature->GetName()<<std::endl;
            now_feature->AddMethod(now_class->GetName());
        }
    }
    class_table->CheckErrorOrExit();

    std::cerr << "Finish method table build." << std::endl;
    std::cerr << "============="<< std::endl;

    // check method type(expect IO).
    std::cerr << "Checking method(formals) type..." <<std::endl;
    for (auto i = classes->first(); classes->more(i); i = classes->next(i)) {
        auto now_class = classes->nth(i)->GetName();
        auto inheritPath = class_table->GetInheritPath(now_class, now_class);
        Features now_features = classes->nth(i)->GetFeatures();
        
        std::cerr << "Checking class "<<now_class<<" method..."<<std::endl;
        // kknd ancestor: method
        for (auto j = now_features->first(); now_features->more(j); j = now_features->next(j)) {
            auto test_method = now_features->nth(j);
            if (!test_method->IsMethod())
                continue;
            auto now_method = (method_class*)test_method;
            auto now_formals = now_method->GetFormals();
            std::cerr << "Checking method "<< now_method->GetName() << "..."<< std::endl;
            for (auto path_class : inheritPath) {
                if (path_class == now_class) // why check itself?
                    break;
                method_class* ancestor_method = method_table[path_class].lookup(now_method->GetName());
                std::cerr << "Class "<<path_class <<"(ancestor):"<<std::endl;
                if (ancestor_method != NULL) {
                    // check return type

                    if (now_method->GetReturnType() != ancestor_method->GetReturnType()) {
                        class_table->semant_error(class_table->sym2class[now_class])
                             << "Error: ancestor class " << path_class << " have different type on " << now_method->GetName() <<"."<< std::endl;
                    }
                    //check formal
                    auto ancestor_formals = ancestor_method->GetFormals();
                    bool formal_flag = false;
                    
                    if (ancestor_formals->len() != now_formals->len()) {
                        formal_flag = true;
                        class_table->semant_error(class_table->sym2class[now_class]) 
                            << "Error: ancestor class " << path_class << " have different formal len." << std::endl;
                    }
                    for (int k = ancestor_formals->first(), l = now_formals->first();
                            !formal_flag && ancestor_formals->more(k) && now_formals->more(l);
                            k = ancestor_formals->next(k), l = now_formals->next(l)) {
                        auto ancestor_formal = ancestor_formals->nth(k);
                        auto now_formal = now_formals->nth(l);

                        std::cerr <<"Checking formal "<< now_formal <<"..."<< std::endl;
                        if (ancestor_formal->GetType() != now_formal->GetType())
                            class_table->semant_error(class_table->sym2class[now_class]) 
                                << "Error: ancestor class " << path_class << "have different formal type in formal "<<now_formal->GetName()<< "." << std::endl;
                        if (ancestor_formal->GetName() != now_formal->GetName())
                            std::cerr << "Warning: ancestor class " << path_class << " and current class "
                                <<now_class << "have different formal name in formal "<< k << "th ." << std::endl;
                    }
                }
            }
        }
    }

    class_table->CheckErrorOrExit();

    std::cerr << "Finish method(formal) type check." << std::endl;
    std::cerr << "============="<< std::endl;

    // attribute: can't repeat: attr to attr.
    // attr could have the same name with method!
    std::cerr << "Checking attribute type..." << std::endl;
    for (auto i = classes->first(); classes->more(i); i = classes->next(i)) {
        auto now_class = classes->nth(i)->GetName();
        auto inherit_path = class_table->GetInheritPath(now_class, now_class);
        Features now_features = classes->nth(i)->GetFeatures();
        
        std::cerr << "Checking class "<<now_class<<" attribute...(create attr_table)"<<std::endl;
        attr_table = new SymbolTable<Symbol, Symbol>();
        // insert all attr in a class.
        // check type while inserting.
        for (auto path_class_name : inherit_path) {
            if(path_class_name == Object){
                continue;
            }
            std::cerr << "insert "<<path_class_name << "'s attr." <<std::endl;
            attr_table->enterscope();
            auto path_features =  class_table->sym2class[path_class_name]->GetFeatures();
            for (auto j = path_features->first(); path_features->more(j); j = path_features->next(j)) {
                auto path_feature = path_features->nth(j);
                std::cerr << path_class_name <<"'s attr: "<< path_feature->GetName() <<"."<<std::endl;
                path_feature->AddAttribute(path_class_name);
            }
        }
        std::cerr << "Checking class "<<now_class<<" attribute type..."<<std::endl;
        for (auto j = now_features->first(); now_features->more(j); j = now_features->next(j)){
            auto now_feature = now_features->nth(j);
            now_feature->CheckExprType(now_class);
        }

        
        // prepare next: expression type
        for (size_t i = 1; i<inherit_path.size(); ++i){
            attr_table->exitscope();
        }
        delete attr_table;
        attr_table = nullptr;
    }

    class_table->CheckErrorOrExit();

    std::cerr << "Finish attribute type check." << std::endl;
    std::cerr << "============="<< std::endl;

}