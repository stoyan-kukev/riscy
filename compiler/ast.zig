const std = @import("std");
const Token = @import("tokenizer.zig").Token;

pub const Program = struct {
    decls: []const *Declaration,
};

pub const Block = struct {
    stmts: []const *Statement,
};

pub const Statement = union(enum) {
    declaration: *Declaration,
    if_stmt: *IfStmt,
    while_stmt: *WhileStmt,
    for_stmt: *ForStmt,
    switch_stmt: *SwitchStmt,
    return_stmt: *ReturnStmt,
    break_stmt: Token,
    continue_stmt: Token,
    defer_stmt: *DeferStmt,
    expression: *Expression,
    block: *Block,
};

pub const Declaration = struct {
    pub_token: ?Token,
    extern_export_token: ?Token, // extern or export
    link_section_token: ?Token, // string_literal token
    const_var_token: Token,
    name: Token,
    type_expr: ?*TypeExpr,
    align_expr: ?*Expression,
    init_expr: ?*Expression,
};

pub const IfStmt = struct {
    condition: *Expression,
    then_block: *Block,
    else_branch: ?*Statement,
};

pub const WhileStmt = struct {
    condition: *Expression,
    body: *Block,
};

pub const ForStmt = struct {
    init: ?*Statement,
    condition: ?*Expression,
    post: ?*Expression,
    body: *Block,
};

pub const DeferStmt = struct {
    keyword: Token, // defer or errdefer
    payload: *Statement,
};

pub const ReturnStmt = struct {
    keyword: Token,
    value: ?*Expression,
};

pub const SwitchStmt = struct {
    condition: *Expression,
    prongs: []const *SwitchProng,
};

pub const SwitchProng = struct {
    patterns: []const *Expression, // empty list implies 'else'
    is_else: bool,
    body: *Statement,
};

pub const TypeExpr = struct {
    prefixes: []const TypePrefix,
    core: TypeCore,
};

pub const TypePrefix = union(enum) {
    pointer: Token, // *
    optional: Token, // ?
    error_union: Token, // !
    many_pointer: Token, // [*]
};

pub const TypeCore = union(enum) {
    identifier: Token,
    struct_def: *StructDef,
    union_def: *UnionDef,
    enum_def: *EnumDef,
    array: *ArrayType,
};

pub const ArrayType = struct {
    size: *Expression,
    child_type: *TypeExpr,
};

pub const StructDef = struct {
    keyword: Token,
    is_packed: bool,
    members: []const *ContainerDecl,
};

pub const UnionDef = struct {
    keyword: Token,
    members: []const *ContainerDecl,
};

pub const EnumDef = struct {
    keyword: Token,
    backing_type: ?*TypeExpr,
    members: []const Token,
};

pub const ContainerDecl = union(enum) {
    field: *FieldDecl,
    declaration: *Declaration,
};

pub const FieldDecl = struct {
    name: Token,
    type_expr: *TypeExpr,
};

pub const Expression = union(enum) {
    binary: *BinaryExpr,
    unary: *UnaryExpr,
    primary: *PrimaryExpr,

    // Postfix chain results
    call: *CallExpr,
    index: *IndexExpr,
    field_access: *FieldAccessExpr,
    dereference: *UnaryExpr, // .*
    unwrap_optional: *UnaryExpr, // .?
    unwrap_error: *UnaryExpr, // .!
};

pub const BinaryExpr = struct {
    lhs: *Expression,
    op: Token,
    rhs: *Expression,
};

pub const UnaryExpr = struct {
    op: Token, // -, not, ~, .*, .?, .!
    operand: *Expression,
};

pub const CallExpr = struct {
    callee: *Expression,
    args: []const *Expression,
};

pub const IndexExpr = struct {
    target: *Expression,
    index: *Expression,
};

pub const FieldAccessExpr = struct {
    target: *Expression,
    field_name: Token,
};

pub const PrimaryExpr = union(enum) {
    int_literal: Token,
    string_literal: Token,
    identifier: Token,
    grouped: *Expression,
    fn_literal: *FunctionLiteral,
    builtin_call: *BuiltinCall,
    asm_block: *AsmBlock,
    struct_init: *StructInit,
};

pub const FunctionLiteral = struct {
    attribute: ?Token, // .naked, .interrupt
    params: []const *Param,
    return_type: *TypeExpr,
    body: *Block,
};

pub const Param = struct {
    name: Token,
    type_expr: *TypeExpr,
};

pub const BuiltinCall = struct {
    name: Token,
    args: []const *Expression,
};

pub const AsmBlock = struct {
    keyword: Token,
    is_pure: bool,
    template: Token, // string_literal
    constraints: ?AsmConstraints,
};

pub const AsmConstraints = struct {
    output: Token,
    input: ?Token,
};

pub const StructInit = struct {
    fields: []const *FieldInit,
};

pub const FieldInit = struct {
    name: Token,
    value: *Expression,
};
