const Token = @import("tokenizer.zig").Token;

pub const Node = struct {
    data: Data,
    tag: Tag,
    /// The token that started the parsing of this node. Used for error reporting.
    token: Token,

    pub const Tag = enum {
        declaration,
        assignment,
        block,
        if_stmt,
        while_stmt,
        for_stmt,
        switch_stmt,
        return_stmt,
        break_stmt,
        continue_stmt,
        defer_stmt,
        asm_block,
        fn_literal,
        struct_literal,
        union_literal,
        enum_literal,
        array_type,
        slice_type,
        pointer_type,
        optional_type,
        error_union_type,
    };

    pub const Data = union {
        declaration: struct {
            is_pub: bool,
            linkage: enum { @"extern", @"export", none },
            linksection_val: ?*Node,
            is_const: bool,
            type_expr: ?*Node,
            align_expr: ?*Node,
            initial_value: ?*Node,
        },
        assignment: struct {
            identifier: Token,
            assignment_expr: *Node,
        },
        block: []const *Node,
        fn_literal: struct {
            modifier: enum { naked, interrupt, none },
            param_list: []const *Node,
            return_type_expr: *Node,
            body: *Node,
        },
        struct_literal: struct {
            modifier: enum { @"packed", c_abi, none },
            field_decls: ?[]*Node, // test: Abc = 50,
            container_decls: ?[]*Node, // pub const test: Abc = 50;
        },
        union_literal: struct {
            modifier: enum { c_abi, none },
            field_decls: ?[]*Node,
            container_decls: ?[]*Node,
        },
        enum_literal: struct {
            type_expr: ?*Node,
            literal_list: []*Node,
        },
        array_type: struct {
            child_type: *Node,
            is_const: bool,
        },
        slice_type: struct {
            child_type: *Node,
            is_const: bool,
        },
        pointer_type: struct {
            child_type: *Node,
            kind: enum { normal, @"volatile", many },
            align_expr: ?*Node,
            allow_zero: bool,
            is_const: bool,
        },
        optional_type: struct {
            child_type: *Node,
        },
        error_union_type: struct {
            child_type: *Node,
        },
        if_stmt: struct {
            condition: *Node,
            then_branch: *Node,
            else_branch: *?Node,
        },
        while_stmt: struct {
            condition: *Node,
            body: *Node,
        },
        for_stmt: struct {
            iterable: *Node,
            body: *Node,
            value_capture: ?Token,
            index_capture: ?Token,
        },
        switch_stmt: struct {
            target: *Node,
            cases: []const SwitchCase,
        },
        return_stmt: ?*Node,
        break_stmt: void,
        continue_stmt: void,
        defer_stmt: struct {
            body: *Node,
        },
        asm_block: struct {
            body: *Node,
            operands: []const AsmOperand,
        },
    };

    pub const SwitchCase = struct {
        cases: []const *Node,
        body: *Node,
        is_else: bool,
    };

    pub const AsmOperand = struct {
        symbol: Token,
        constraint: Token,
        expr: *Node,
    };
};
