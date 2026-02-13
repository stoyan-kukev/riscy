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
        binary_expr,
        unary_expr,
        ptr_dereference,
        volatile_dereference,
        optional_unwrap,
        error_unwrap,
        fn_call,
        index_access,
        slice_access,
        field_access,
        root,
        struct_init,
        builtin_call,
        identifier,
        int_literal,
        string_literal,
        char_literal,
    };

    pub const Data = union {
        declaration: struct {
            is_pub: bool,
            linkage: Node.Linkage,
            linksection_val: ?*Node,
            is_const: bool,
            type_expr: ?*Node,
            align_expr: ?*Node,
            initial_value: ?*Node,
        },
        assignment: struct {
            identifier: *Node,
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
            size_expr: *Node,
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
            else_branch: ?*Node,
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
            is_errdefer: bool,
        },
        asm_block: struct {
            is_pure: bool,
            body: *Node,
            operands: []const AsmOperand,
            clobbers: []const Token,
        },
        binary_expr: struct {
            left: *Node,
            operator: Token.Tag,
            right: *Node,
        },
        unary_expr: struct {
            operator: Token.Tag,
            operand: *Node,
        },
        /// Used for .*, .~, .?, .!
        unary_suffix: struct {
            lhs: *Node,
        },
        fn_call: struct {
            lhs: *Node,
            args: []const *Node,
        },
        index_access: struct {
            lhs: *Node,
            index: *Node,
        },
        slice_access: struct {
            lhs: *Node,
            start: *Node,
            end: ?*Node,
        },
        field_access: struct {
            lhs: *Node,
            field_name: Token,
        },
        root: struct {
            decls: []const *Node,
        },
        struct_init: struct {
            fields: []const *Node,
        },
        builtin_call: struct {
            name: Token,
            args: []const *Node,
        },
        string_literal: struct {
            data: []const u8,
        },
        char_literal: struct {
            char: u8,
        },
        /// Used for literals, identifiers, etc.
        none: void,
    };

    pub const Linkage = enum {
        @"extern",
        @"export",
        none,
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
