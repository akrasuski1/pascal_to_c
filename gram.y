%{
int yylex(void);
void yyerror(const char *,...);
int yyparse(void);
extern int yylineno;
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#define fail(...)            \
	do {                     \
		printf(__VA_ARGS__); \
		exit(0);             \
	} while(0)

void* safe_malloc(int sz){
	void* m = malloc(sz);
	if(m){ return m; }
	fail("Malloc failed!\n");
}

int str_equal(char* a, char* b){
	while(*a && *b){
		if(toupper(*a) != toupper(*b)){
			return 0;
		}
		a++; b++;
	}
	return *a == 0 && *b == 0;
}

typedef struct var_entry {
	struct var_entry* next;
	char* text;
	struct recursive_type* type;
} var_entry;

enum {
	TYPE_CHR,
	TYPE_INT,
	TYPE_FLT,
	TYPE_RECORD
};

typedef struct strpair {
	char* first;
	char* second;
} strpair;

typedef struct io_entry {
	struct io_entry* next;
	char* text;
	int type;
	strpair* prec;
} io_entry;

enum {
	IO_STR,
	IO_ID
};

const char* literal_names[] = {"Char", "Integer", "Extended"};

typedef struct type_info {
	char* pascal_name;
	char* c_name;
	char* scan_fmt;
	char* print_fmt;
	int base_type;
	int is_standard;
	long long min_int, max_int;
	long double min_flt, max_flt;
} type_info;

type_info type_infos[]={
	{"BYTE",     "unsigned char", "c", "c", TYPE_INT, 0, 0,          255,         0, 0},
	{"SHORTINT", "signed char",   "c", "c", TYPE_INT, 0, -128,       127,         0, 0},
	{"INTEGER",  "int",           "d", "d", TYPE_INT, 1, -(1LL<<31), (1LL<<31)-1, 0, 0},
	{"WORD",     "unsigned int",  "u", "u", TYPE_INT, 0, 0,          65535,       0, 0},
	{"LONGINT",  "long int",      "ld","ld",TYPE_INT, 0, -(1LL<<31), (1LL<<31)-1, 0, 0},
	{"REAL",     "double",        "lf","f", TYPE_FLT, 1, 0, 0, -2.9e39, 1.7e38},
	{"SINGLE",   "float",         "f", "f", TYPE_FLT, 0, 0, 0, -1.5e45, 3.4e38},
	{"DOUBLE",   "double",        "lf","lf",TYPE_FLT, 0, 0, 0, -1e300,  1e300 },
	{"EXTENDED", "long double",   "lf","lf",TYPE_FLT, 0, 0, 0, -1e300,  1e300 },
	{"COMP",     "long double",   "lf","lf",TYPE_FLT, 0, 0, 0, -1e300,  1e300 },
	{"CHAR",     "char",          "c", "c", TYPE_CHR, 1, 0, 0, 0, 0},
};

#define TYPES_CNT (sizeof(type_infos)/sizeof(type_infos[0]))

typedef struct init_info {
	int base_type;
	char* text;
	struct init_info* ii;
	struct init_info* next;
} init_info;

typedef struct recursive_range {
	struct recursive_range* next;
	strpair* range;
} recursive_range;

#define RT_BASIC_TYPE   0
#define RT_IS_RECORD    1
#define RT_EMPTY_RECORD 2
typedef struct recursive_type {
	int rt_type;
	char* text;
	var_entry* varnames;
	recursive_range* range; // For array types.
	union {
		type_info* ti;
		struct recursive_type* rt;
	};
	struct recursive_type* next;
} recursive_type;

var_entry* all_identifiers = 0;

void print_upper(char* str){
	while(*str){
		printf("%c", toupper(*str++));
	}
}

recursive_range* make_recursive_range(recursive_range* next, strpair* pair){
	recursive_range* ret = safe_malloc(sizeof(recursive_range));
	ret->next = next;
	ret->range = pair;
	return ret;
}

strpair* make_strpair(char* first, char* second){
	strpair* ret = safe_malloc(sizeof(strpair));
	ret->first = first;
	ret->second = second;
	return ret;
}

io_entry* make_io_entry(char* text, int type, strpair* prec, io_entry* next){
	io_entry* io = safe_malloc(sizeof(io_entry));
	io->text = strdup(text);
	io->type = type;
	io->next = next;
	io->prec = prec;
	return io;
}

var_entry* make_var_entry(char* str, recursive_type* type, var_entry* next){
	var_entry* ret = safe_malloc(sizeof(var_entry));
	ret->next = next;
	ret->type = type;
	ret->text = str;
	return ret;
}

init_info* make_init(char* s, int type, init_info* ii, init_info* next){
	init_info* i = safe_malloc(sizeof(init_info));
	i->base_type = type;
	i->text = s;
	i->next = next;
	i->ii = ii;
	return i;
}

type_info* get_type_info(char* type){
	type_info* res = 0;
	for(int i = 0; i < TYPES_CNT; i++){
		if(str_equal(type_infos[i].pascal_name, type)){
			res = &type_infos[i];
		}
	}
	if(!res->is_standard){
		for(var_entry* var = all_identifiers; var; var = var->next){
			if(str_equal(var->text, type)){
				fail("type identifier expected in line %d\n", yylineno);
			}
		}
	}
	if(res){
		return res;
	}
	fail("type identifier expected in line %d\n", yylineno);
}

recursive_type* make_recursive_type(int rt_type, void* text, recursive_type* next, var_entry* varnames){
	recursive_type* rt = safe_malloc(sizeof(recursive_type));
	rt->rt_type = rt_type;
	rt->next = next;
	rt->varnames = varnames;
	rt->range = 0;
	if(rt_type == RT_BASIC_TYPE){
		rt->ti = get_type_info(text);
		rt->text = text;
	}
	else{
		rt->rt = text;
	}
	return rt;
}

recursive_type* copy_recursive_type(recursive_type* orig){
	recursive_type* rt = safe_malloc(sizeof(recursive_type));
	memcpy(rt, orig, sizeof(recursive_type));
	return rt;
}

void print_list(var_entry* n){
	int first = 1;
	while(n){
		printf("%s", first ? "" : ", ");
		print_upper(n->text);
		first = 0;
		n = n->next;
	}
}

void make_indent(int depth){
	for(int i = 0; i < depth; i++){ printf("  "); }
}

void print_type(recursive_type* rt, int depth, int as_c, var_entry* vlist){
	recursive_type* original_rt = rt;
	if(rt->rt_type == RT_IS_RECORD || rt->rt_type == RT_EMPTY_RECORD){
		make_indent(depth);
		if(as_c){
			printf("struct\n");
			make_indent(depth);
			printf("{\n");
		}
		else{
			printf("record\n");
		}
		while(rt && rt->rt_type != RT_EMPTY_RECORD){
			print_type(rt->rt, depth+1, as_c, rt->varnames);
			printf(";\n");
			rt = rt->next;
		}
		make_indent(depth);
		if(as_c){
			printf("}");
		}
		else{
			printf("end");
		}
	}
	else if(rt->rt_type == RT_BASIC_TYPE){
		while(rt){
			make_indent(depth);
			if(as_c){
				printf("%s", rt->ti->c_name);
			}
			else{
				printf("%s", rt->text);
			}
			rt = rt->next;
		}
	}
	if(as_c){
		printf(" ");
		print_list(vlist);
		recursive_range* r = original_rt->range;
		while(r){
			printf("[%s - (%s) + 1]", r->range->second, r->range->first);
			r = r->next;
		}
	}
}

void print_default_initializer(recursive_type* rt){
	if(rt->rt->rt_type != RT_BASIC_TYPE){
		printf("{ }");
	}
	else{
		printf("0.0");
	}
}

void print_init(init_info* info, int depth, recursive_type* reference_struct){
	int as_c = reference_struct != 0;
	if(!info){ return; }
	if(info->base_type == TYPE_RECORD){
		printf("%s", as_c ? "{ " : "record\n");
		var_entry* v;
		if(as_c){
			v = reference_struct->varnames;
		}
		while(info){
			if(!as_c){
				make_indent(depth+1);
				printf("%s : ", info->text);
			}
			else{
				while(!str_equal(v->text, info->text)){
					print_default_initializer(reference_struct);
					printf(", ");
					v = v->next;
					if(!v){
						reference_struct = reference_struct->next;
						if(reference_struct){
							v = reference_struct->varnames;
						}
					}
				}
			}
			print_init(info->ii, depth+1, reference_struct->rt);
			printf("%s", as_c ? (info->next ? ", " : "") : ";\n");
			info = info->next;
			if(v){
				v = v->next;
				if(!v){
					reference_struct = reference_struct->next;
					if(reference_struct){
						v = reference_struct->varnames;
					}
				}
			}
		}
		printf("%s", as_c ? " }" : "end");
	}
	else{
		printf("%s", as_c ? info->text : literal_names[info->base_type]);
	}
}

void incompatible_types(recursive_type* var_type, init_info* init){
	printf("incompatible types '");
	print_type(var_type, 0, 0, 0);
	printf("' and '");
	print_init(init, 0, 0);
	printf("' in line %d\n", yylineno);
	exit(0);
}

void check_type_compatibility(recursive_type* var_type, init_info* init){
	if(init){
		if(var_type->rt_type == RT_IS_RECORD || var_type->rt_type == RT_EMPTY_RECORD ||
				 init->base_type == TYPE_RECORD){
			if(!(var_type->rt_type == RT_IS_RECORD || var_type->rt_type == RT_EMPTY_RECORD)
					 || init->base_type != TYPE_RECORD){
				incompatible_types(var_type, init);
			}
			for(init_info* i = init; i; i = i->next){
				int found=0;
				for(recursive_type* v = var_type; v; v = v->next){
					for(var_entry* n = v->varnames; n; n = n->next){
						if(str_equal(n->text, i->text)){
							check_type_compatibility(v->rt, i->ii);
							found = 1;
						}
					}
				}
				if(!found){
					incompatible_types(var_type, init);
				}
			}
			return;
		}
		type_info* ti = var_type->ti;
		int tf = init->base_type;
		int tt = ti->base_type;
		if(tf == tt){}
		else if(tf==TYPE_INT && tt==TYPE_FLT){}
		else{
			incompatible_types(var_type, init);
		}
		int violation = 0;

		if(tf == TYPE_INT){
			long long scan;
			sscanf(init->text, "%lld", &scan);
			if(tt == TYPE_INT){
				if(scan < ti->min_int || scan > ti->max_int){
					violation = 1;
				}
			}
			if(tt == TYPE_FLT){
				if(scan < ti->min_flt || scan > ti->max_flt){
					violation = 1;
				}
			}
		}
		else if(tf == TYPE_FLT){
			long double scan;
			sscanf(init->text, "%Lf", &scan);
			if(scan < ti->min_flt || scan > ti->max_flt){
				violation = 1;
			}
		}
		
		if(violation){
			fail("constant expression violates subrange bounds in line %d\n", 
					yylineno);
		}
	}
}

void add_vars(var_entry* list, recursive_type* type){
	while(list){
		for(var_entry* id = all_identifiers; id; id = id->next){
			if(str_equal(id->text, list->text)){
				fail("identifier %s redeclared in line %d\n", list->text, yylineno);
			}
		}
		all_identifiers = make_var_entry(list->text, type, all_identifiers);
		list = list->next;
	}
}

enum{
	FN_READ,
	FN_WRITE,
	FN_WRITELN
};

void dispatch_io(char* funname, io_entry* arglist){
	int fn = -1;
	if(str_equal(funname, "Read"))   { fn = FN_READ;    }
	if(str_equal(funname, "Write"))  { fn = FN_WRITE;   }
	if(str_equal(funname, "WriteLn")){ fn = FN_WRITELN; }

	if(fn == -1){ fail("Unknown function!\n"); }
	if(fn == FN_READ){ printf("scanf(\""); }
	if(fn == FN_WRITE || fn == FN_WRITELN){ printf("printf(\""); }

	for(io_entry* io = arglist; io; io = io->next){
		if(io->type == IO_STR){
			io->text[strlen(io->text)-1]=0;
			printf("%s", io->text+1);
		}
		else{
			type_info* ti = 0;
			for(var_entry* var = all_identifiers; var; var = var->next){
				if(str_equal(var->text, io->text)){
					ti = var->type->ti;
				}
			}
			if(!ti){ fail("Undeclared variable!\n"); }
			printf("%%");
			if(io->prec){
				printf("%s.%s", io->prec->first, io->prec->second);
			}
			printf("%s", (fn == FN_READ) ? ti->scan_fmt : ti->print_fmt);
		}
	}
	if(fn == FN_WRITELN){ printf("\\n"); }
	printf("\"");
	for(io_entry* io = arglist; io; io = io->next){
		if(io->type == IO_ID){
			printf(",%s", (fn == FN_READ) ? "&" : "");
			print_upper(io->text);
		}
	}
	printf(");\n");
}

recursive_type* find_var_type(char* name){
	for(var_entry* v = all_identifiers; v; v = v->next){
		if(str_equal(v->text, name)){
			return v->type;
		}
	}
	fail("Identifier %s not found!\n", name);
}

recursive_type* find_field_type(recursive_type* type, char* name){
	while(type){
		for(var_entry* v = type->varnames; v; v = v->next){
			if(str_equal(v->text, name)){
				return v->type;
			}
		}
		type = type->next;
	}
	fail("Field %s not found!\n", name);
}

recursive_type* reduce_array(recursive_type* rt){
	if(rt->range == 0){
		return rt->rt;
	}
	recursive_type* res = copy_recursive_type(rt);
	res->range = res->range->next;
	return res;
}

%}

%union
{
	char* text;
	int ival;
	struct var_entry* list;
	struct init_info* init;
	struct io_entry* io;
	struct strpair* pair;
	struct recursive_type* type;
}

%token KWD_PROGRAM KWD_BEGIN KWD_END KWD_VAR KWD_CONST KWD_RECORD KWD_ARRAY KWD_OF
%token TOK_ASSIGN TOK_DOTDOT
%token TOK_NOT '@' TOK_POW '*' '/' TOK_DIV TOK_MOD TOK_AND
%token TOK_AS TOK_SHL TOK_SHR  '+' '-' TOK_OR
%token TOK_XOR TOK_IN TOK_ISS TOK_NEQ '<' '>' TOK_LE TOK_GE

%token <text> IDENT INT_CONST REAL_CONST CHAR_CONST TOK_OP 

%type <text> ArrayIndexConst
%type <list> IdentifierList
%type <io> NonEmptyIOFunctionArgs IOFunctionArgs
%type <pair> OptionalPrecision ArrayRange
%type <type> Type InsideRecord MaybeEmptyInsideRecord Variable ArrayInside NonArrayType
%type <init> OptionalInitializer Initializer InitializedVars

%%

Input 
	: ProgramHeader Program
	| Program
	;

ProgramHeader
	: KWD_PROGRAM IDENT OptionalProgramParameters ';'
	  { printf("/* program : %s */\n", $2); }
	;

OptionalProgramParameters
	: 
	| '(' IdentifierList ')' 
	;

IdentifierList
	: IDENT { $$ = make_var_entry($1, 0, 0); }
	| IDENT ',' IdentifierList { $$ = make_var_entry($1, 0, $3); }
	;

Program
	: {
		printf("#include <stdio.h>\n");
	  }
	  GlobalList
	  {
		printf("int main()\n");
		printf("{\n");
	  } 
	  Block '.'
	  {
		printf("  return 0;\n");
		printf("}\n");
	  }
	;

GlobalList
	: 
	| Global GlobalList
	;

Global
	: VarConst GlobalList2
	;

VarConst
	: KWD_VAR
	| KWD_CONST
	;

GlobalList2
	:
	| Global2 GlobalList2
	;

Global2
	: IdentifierList ':' Type OptionalInitializer ';'
	  { 
		add_vars($1, $3);
		check_type_compatibility($3, $4);
		print_type($3, 0, 1, $1);
		if($4){
			printf(" = ");
			print_init($4, 0, $3);
		}
		printf(";\n");
	  }
	;

NonArrayType
	: IDENT { $$ = make_recursive_type(RT_BASIC_TYPE, $1, 0, 0); }
	| KWD_RECORD MaybeEmptyInsideRecord KWD_END { $$ = $2; }
	;

Type
	: NonArrayType { $$ = $1; }
	| ArrayBegin ArrayInside { $$ = $2; } 
	;

ArrayInside
	: ArrayRange ArrayEnd NonArrayType 
	  { $$ = $3; $$->range = make_recursive_range($$->range, $1); }
	| ArrayRange ',' ArrayInside
	  { $$ = $3; $$->range = make_recursive_range($$->range, $1); }
	| ArrayRange ']' KWD_OF KWD_ARRAY '[' ArrayInside
	  { $$ = $6; $$->range = make_recursive_range($$->range, $1); }
	;

ArrayRange
	: ArrayIndexConst TOK_DOTDOT ArrayIndexConst { $$ = make_strpair($1, $3); }
	;

ArrayIndexConst
	: INT_CONST { $$ = $1; }
	| CHAR_CONST { $$ = $1; }
	;

ArrayBegin
	: KWD_ARRAY '['
	;

ArrayEnd
	: ']' KWD_OF
	;

MaybeEmptyInsideRecord
	: { $$ = make_recursive_type(RT_EMPTY_RECORD, 0, 0, 0); }
	| InsideRecord { $$ = $1; }
	;

InsideRecord
	: IdentifierList ':' Type ';' { $$ = make_recursive_type(RT_IS_RECORD, $3, 0, $1); }
	| IdentifierList ':' Type ';' InsideRecord { $$ = make_recursive_type(RT_IS_RECORD, $3, $5, $1); }
	;

OptionalInitializer
	: { $$ = 0; }
	| '=' Initializer  { $$ = $2; }
	;

Initializer
	: INT_CONST  { $$ = make_init($1, TYPE_INT, 0, 0); }
	| REAL_CONST { $$ = make_init($1, TYPE_FLT, 0, 0); }
	| CHAR_CONST { $$ = make_init($1, TYPE_CHR, 0, 0); }
	| '(' InitializedVars ')' { $$ = $2; }
	;

InitializedVars
	: IDENT ':' Initializer { $$ = make_init($1, TYPE_RECORD, $3, 0); }
	| IDENT ':' Initializer ';' { $$ = make_init($1, TYPE_RECORD, $3, 0); }
	| IDENT ':' Initializer ';' InitializedVars { $$ = make_init($1, TYPE_RECORD, $3, $5); }
	;

Block
	: KWD_BEGIN BlockInside KWD_END
	;

Statement
	: IOFunction
	| Assignment
	;

BlockInside
	: { make_indent(1); } Statement BlockInside
	| 
	;

IOFunction
	: IDENT '(' IOFunctionArgs ')' ';' { dispatch_io($1, $3); }
	;

IOFunctionArgs
	: { $$ = 0; }
	| NonEmptyIOFunctionArgs { $$ = $1; }
	;

NonEmptyIOFunctionArgs
	: CHAR_CONST { $$ = make_io_entry($1, IO_STR, 0, 0); }
	| CHAR_CONST ',' NonEmptyIOFunctionArgs { $$ = make_io_entry($1, IO_STR, 0, $3); }

	| IDENT OptionalPrecision { $$ = make_io_entry($1, IO_ID, $2, 0); }
	| IDENT OptionalPrecision ',' NonEmptyIOFunctionArgs { $$ = make_io_entry($1, IO_ID, $2, $4); }
	;

OptionalPrecision
	: { $$ = 0; }
	| ':' INT_CONST ':' INT_CONST { $$ = make_strpair($2, $4); }
	;

Assignment
	: Variable
	  { printf(" = "); } 
	  TOK_ASSIGN Expression ';' { printf(";\n"); }
	;

Variable
	: IDENT { $$ = find_var_type($1); print_upper($1); }
	| Variable '.' IDENT { $$ = find_field_type($1, $3); printf("."); print_upper($3); }
	| Variable '[' { printf("["); $<type>$ = $1; } VariableFinishArrayIndex ']' { printf("]"); $$ = $<type>3; }
	;

VariableFinishArrayIndex
	: ExprInVarIndex
	| ExprInVarIndex ',' { printf("]["); $<type>$ = $<type>0; } VariableFinishArrayIndex
	;

ExprInVarIndex
	: Expression
	  {
		if(($<type>0)->range == 0){ fail("Not an array!\n"); }
		printf(" - (%s)", ($<type>0)->range->range->first);
		$<type>0 = reduce_array($<type>0);
	  }
	;

Expression_1
	: INT_CONST { printf("%s", $1); }
	| REAL_CONST { printf("%s", $1); }
	| CHAR_CONST { printf("%s", $1); }
	| Variable
	| '(' { printf("("); } Expression ')' { printf(")"); }
	;

Expression_2
	: Expression_1 '='     { printf(" == "); } Expression_2
	| Expression_1 TOK_NEQ { printf(" != "); } Expression_2
	| Expression_1 '<'     { printf(" < ");  } Expression_2
	| Expression_1 '>'     { printf(" > ");  } Expression_2
	| Expression_1 TOK_GE  { printf(" >= "); } Expression_2
	| Expression_1 TOK_LE  { printf(" <= "); } Expression_2
	| Expression_1
	;

Expression_3
	: Expression_2 '+'     { printf(" + "); } Expression_3
	| Expression_2 '-'     { printf(" - "); } Expression_3
	| Expression_2 TOK_OR  { printf(" | "); } Expression_3
	| Expression_2 TOK_XOR { printf(" ^ "); } Expression_3
	| Expression_2
	;

Expression_4
	: Expression_3 '*'     { printf(" * ");  } Expression_4
	| Expression_3 '/'     { printf(" / ");  } Expression_4
	| Expression_3 TOK_DIV { printf(" / ");  } Expression_4
	| Expression_3 TOK_MOD { printf(" %% "); } Expression_4
	| Expression_3 TOK_AND { printf(" & ");  } Expression_4
	| Expression_3 TOK_SHL { printf(" << "); } Expression_4
	| Expression_3 TOK_SHR { printf(" >> "); } Expression_4
	| Expression_3
	;

Expression
	: TOK_NOT { printf(" !"); } Expression
	| '+' { printf(" +"); } Expression
	| '-' { printf(" -"); } Expression
	| Expression_4
	;


%%

void yyerror(const char *fmt, ...)
{
	printf("%s in line %d\n", fmt, yylineno);
}
int main() { return yyparse(); }
