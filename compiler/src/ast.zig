const std = @import("std");
const Token = @import("tokenizer.zig").Token;

pub const Node = struct {
    data: Data,
    tag: Tag,
    /// The token that started the parsing of this node. Used for error reporting.
    token: Token,

    pub const Linkage = enum {
        @"extern",
        @"export",
        none,
    };

    pub const StructModifier = enum {
        @"packed",
        c_abi,
        none,
    };

    pub const FnModifier = enum {
        naked,
        interrupt,
        none,
    };

    pub const UnionModifier = enum {
        c_abi,
        none,
    };

    pub const PointerKind = enum {
        normal,
        @"volatile",
        many,
    };

    pub const SwitchCase = struct {
        variants: []const *Node,
        body: *Node,
        is_else: bool,
    };

    pub const AsmOperand = struct {
        symbol: Token,
        constraint: Token,
        expr: *Node,
    };

    pub const Tag = enum {
        declaration,
        assignment,
        block,
        if_stmt,
        while_stmt,
        for_stmt,
        switch_stmt,
        range_pattern,
        return_stmt,
        break_stmt,
        continue_stmt,
        defer_stmt,
        asm_block,
        fn_literal,
        parameter,
        struct_literal,
        field_decl,
        union_literal,
        enum_literal,
        array_type,
        slice_type,
        pointer_type,
        optional_type,
        error_union_type,
        error_literal,
        unreachable_literal,
        null_literal,
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
        field_assignment,
        root,
        struct_init,
        builtin_call,
        identifier,
        int_literal,
        string_literal,
        char_literal,
        enum_member,
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
            operator: Token,
        },
        block: []const *Node,
        fn_literal: struct {
            modifier: FnModifier,
            param_list: []const *Node,
            return_type_expr: *Node,
            body: *Node,
        },
        parameter: struct {
            type_expr: *Node,
            align_expr: ?*Node,
        },
        struct_literal: struct {
            modifier: StructModifier,
            field_decls: ?[]*Node,
            container_decls: ?[]*Node,
        },
        field_decl: struct {
            type_expr: *Node,
            align_expr: ?*Node,
            default_value: ?*Node,
        },
        field_assignment: struct {
            identifier: Token,
            value: *Node,
        },
        union_literal: struct {
            modifier: UnionModifier,
            field_decls: ?[]*Node,
            container_decls: ?[]*Node,
        },
        enum_literal: struct {
            type_expr: ?*Node,
            literal_list: []*Node,
            container_decls: ?[]*Node,
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
            kind: PointerKind,
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
            value_capture: Token,
            is_value_ptr: bool,
            index_capture: ?Token,
        },
        switch_stmt: struct {
            target: *Node,
            cases: []const SwitchCase,
        },
        range_pattern: struct {
            start: *Node,
            end: *Node,
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
        none: void,
    };

    /// Helper to print indentation
    fn printIndent(writer: *std.io.Writer, indent: usize) !void {
        var i: usize = 0;
        while (i < indent) : (i += 1) {
            try writer.writeAll("  ");
        }
    }

    /// Recursively formats the AST node to the provided writer
    pub fn prettyPrint(self: *const Node, writer: *std.io.Writer, indent: usize) anyerror!void {
        try printIndent(writer, indent);
        try writer.print("{s} {{\n", .{@tagName(self.tag)});

        const next_indent = indent + 1;

        try printIndent(writer, next_indent);
        try writer.print("token: {any},\n", .{self.token});

        switch (self.tag) {
            inline else => |tag| {
                const payload_field = comptime blk: {
                    const name = @tagName(tag);

                    // Check exceptions that map to shared payloads
                    if (std.mem.eql(u8, name, "ptr_dereference") or
                        std.mem.eql(u8, name, "volatile_dereference") or
                        std.mem.eql(u8, name, "optional_unwrap") or
                        std.mem.eql(u8, name, "error_unwrap"))
                    {
                        break :blk "unary_suffix";
                    }

                    if (@hasField(Data, name)) {
                        break :blk name;
                    }

                    break :blk "none";
                };

                try printPayload(@field(self.data, payload_field), writer, next_indent);
            },
        }

        try printIndent(writer, indent);
        try writer.print("}}\n", .{});
    }

    /// Dispatches printing based on the specific type inside the union payload
    fn printPayload(payload: anytype, writer: anytype, indent: usize) anyerror!void {
        const T = @TypeOf(payload);
        const info = @typeInfo(T);

        if (T == Token) {
            try printIndent(writer, indent);
            try writer.print("{any}\n", .{payload});
            return;
        }

        switch (info) {
            .void => {},
            .pointer => |ptr_info| {
                if (T == []const u8 or T == []u8) {
                    try printIndent(writer, indent);
                    try writer.print("\"{s}\"\n", .{payload});
                } else if (ptr_info.size == .one) {
                    if (T == *Node or T == *const Node) {
                        try payload.prettyPrint(writer, indent);
                    } else {
                        try printPayload(payload.*, writer, indent);
                    }
                } else if (ptr_info.size == .slice) {
                    if (payload.len == 0) {
                        try printIndent(writer, indent);
                        try writer.print("[]\n", .{});
                    } else {
                        for (payload) |item| {
                            try printPayload(item, writer, indent);
                        }
                    }
                } else {
                    try printIndent(writer, indent);
                    try writer.print("{any}\n", .{payload});
                }
            },
            .optional => {
                if (payload) |val| {
                    try printPayload(val, writer, indent);
                } else {
                    try printIndent(writer, indent);
                    try writer.print("null\n", .{});
                }
            },
            .@"struct" => |struct_info| {
                inline for (struct_info.fields) |f| {
                    try printIndent(writer, indent);
                    try writer.print("{s}:\n", .{f.name});
                    try printPayload(@field(payload, f.name), writer, indent + 1);
                }
            },
            .@"enum" => {
                try printIndent(writer, indent);
                try writer.print(".{s}\n", .{@tagName(payload)});
            },
            .bool, .int, .comptime_int => {
                try printIndent(writer, indent);
                try writer.print("{}\n", .{payload});
            },
            else => {
                try printIndent(writer, indent);
                try writer.print("{any}\n", .{payload});
            },
        }
    }
};
