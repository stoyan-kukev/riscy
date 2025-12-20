const std = @import("std");
const Ast = @import("ast.zig");
const Token = @import("tokenizer.zig").Token;

pub const AstPrinter = struct {
    allocator: std.mem.Allocator,
    source: []const u8,
    writer: *std.Io.Writer,
    indent_level: usize = 0,

    pub fn init(allocator: std.mem.Allocator, source: []const u8, writer: *std.Io.Writer) AstPrinter {
        return .{
            .allocator = allocator,
            .source = source,
            .writer = writer,
            .indent_level = 0,
        };
    }

    fn printIndent(self: *AstPrinter) std.Io.Writer.Error!void {
        var i: usize = 0;
        while (i < self.indent_level) : (i += 1) {
            try self.writer.print("  ", .{});
        }
    }

    pub fn printProgram(self: *AstPrinter, program: Ast.Program) std.Io.Writer.Error!void {
        try self.writer.print("Program\n", .{});
        self.indent_level += 1;
        for (program.decls) |decl| {
            try self.printDeclaration(decl);
        }
        self.indent_level -= 1;
    }

    fn printDeclaration(self: *AstPrinter, decl: *Ast.Declaration) std.Io.Writer.Error!void {
        try self.printIndent();
        try self.writer.print("Declaration ({s}) {s}\n", .{
            decl.const_var_token.lexeme(self.source),
            decl.name.lexeme(self.source),
        });

        self.indent_level += 1;
        if (decl.type_expr) |t| {
            try self.printIndent();
            try self.writer.print("Type:\n", .{});
            self.indent_level += 1;
            try self.printTypeExpr(t);
            self.indent_level -= 1;
        }
        if (decl.init_expr) |e| {
            try self.printIndent();
            try self.writer.print("Init:\n", .{});
            self.indent_level += 1;
            try self.printExpression(e);
            self.indent_level -= 1;
        }
        self.indent_level -= 1;
    }

    fn printBlock(self: *AstPrinter, block: *Ast.Block) std.Io.Writer.Error!void {
        try self.printIndent();
        try self.writer.print("Block\n", .{});
        self.indent_level += 1;
        for (block.stmts) |stmt| {
            try self.printStatement(stmt);
        }
        self.indent_level -= 1;
    }

    fn printStatement(self: *AstPrinter, stmt: *Ast.Statement) std.Io.Writer.Error!void {
        switch (stmt.*) {
            .declaration => |d| try self.printDeclaration(d),
            .expression => |e| {
                try self.printIndent();
                try self.writer.print("Expression Stmt\n", .{});
                self.indent_level += 1;
                try self.printExpression(e);
                self.indent_level -= 1;
            },
            .if_stmt => |s| {
                try self.printIndent();
                try self.writer.print("If Stmt\n", .{});
                self.indent_level += 1;
                try self.printExpression(s.condition);
                try self.printBlock(s.then_block);
                if (s.else_branch) |e| {
                    try self.printStatement(e);
                }
                self.indent_level -= 1;
            },
            .while_stmt => |s| {
                try self.printIndent();
                try self.writer.print("While Stmt\n", .{});
                self.indent_level += 1;
                try self.printExpression(s.condition);
                try self.printBlock(s.body);
                self.indent_level -= 1;
            },
            .for_stmt => |s| {
                try self.printIndent();
                try self.writer.print("For Stmt\n", .{});
                self.indent_level += 1;
                if (s.init) |i| try self.printStatement(i);
                if (s.condition) |c| try self.printExpression(c);
                if (s.post) |p| try self.printExpression(p);
                try self.printBlock(s.body);
                self.indent_level -= 1;
            },
            .return_stmt => |s| {
                try self.printIndent();
                try self.writer.print("Return Stmt\n", .{});
                if (s.value) |v| {
                    self.indent_level += 1;
                    try self.printExpression(v);
                    self.indent_level -= 1;
                }
            },
            .defer_stmt => |s| {
                try self.printIndent();
                try self.writer.print("Defer Stmt ({s})\n", .{s.keyword.lexeme(self.source)});
                self.indent_level += 1;
                try self.printStatement(s.payload);
                self.indent_level -= 1;
            },
            .block => |b| try self.printBlock(b),
            .break_stmt => |_| {
                try self.printIndent();
                try self.writer.print("Break\n", .{});
            },
            .continue_stmt => |_| {
                try self.printIndent();
                try self.writer.print("Continue\n", .{});
            },
            .switch_stmt => |s| {
                try self.printIndent();
                try self.writer.print("Switch Stmt\n", .{});
                self.indent_level += 1;
                try self.printExpression(s.condition);

                for (s.prongs) |prong| {
                    try self.printIndent();
                    try self.writer.print("Prong (else={})\n", .{prong.is_else});
                    self.indent_level += 1;
                    for (prong.items) |item| {
                        try self.printIndent();
                        switch (item) {
                            .expression => |e| {
                                try self.writer.print("Item Expr:\n", .{});
                                self.indent_level += 1;
                                try self.printExpression(e);
                                self.indent_level -= 1;
                            },
                            .range => |r| {
                                try self.writer.print("Item Range:\n", .{});
                                self.indent_level += 1;
                                try self.printExpression(r.start);
                                try self.printExpression(r.end);
                                self.indent_level -= 1;
                            },
                        }
                    }
                    try self.printStatement(prong.body);
                    self.indent_level -= 1;
                }
                self.indent_level -= 1;
            },
        }
    }

    fn printExpression(self: *AstPrinter, expr: *Ast.Expression) std.Io.Writer.Error!void {
        switch (expr.*) {
            .binary => |b| {
                try self.printIndent();
                try self.writer.print("Binary ({s})\n", .{b.op.lexeme(self.source)});
                self.indent_level += 1;
                try self.printExpression(b.lhs);
                try self.printExpression(b.rhs);
                self.indent_level -= 1;
            },
            .primary => |p| try self.printPrimary(p),
            .unary => |u| {
                try self.printIndent();
                try self.writer.print("Unary ({s})\n", .{u.op.lexeme(self.source)});
                self.indent_level += 1;
                try self.printExpression(u.operand);
                self.indent_level -= 1;
            },
            .call => |c| {
                try self.printIndent();
                try self.writer.print("Call\n", .{});
                self.indent_level += 1;
                try self.printExpression(c.callee);
                for (c.args) |arg| {
                    try self.printExpression(arg);
                }
                self.indent_level -= 1;
            },
            .index => |i| {
                try self.printIndent();
                try self.writer.print("Index\n", .{});
                self.indent_level += 1;
                try self.printExpression(i.target);
                try self.printExpression(i.index);
                self.indent_level -= 1;
            },
            .field_access => |f| {
                try self.printIndent();
                try self.writer.print("FieldAccess .{s}\n", .{f.field_name.lexeme(self.source)});
                self.indent_level += 1;
                try self.printExpression(f.target);
                self.indent_level -= 1;
            },
            .dereference => |u| {
                try self.printIndent();
                try self.writer.print("Dereference .*\n", .{});
                self.indent_level += 1;
                try self.printExpression(u.operand);
                self.indent_level -= 1;
            },
            .unwrap_optional => |u| {
                try self.printIndent();
                try self.writer.print("UnwrapOptional .?\n", .{});
                self.indent_level += 1;
                try self.printExpression(u.operand);
                self.indent_level -= 1;
            },
            .unwrap_error => |u| {
                try self.printIndent();
                try self.writer.print("UnwrapError .!\n", .{});
                self.indent_level += 1;
                try self.printExpression(u.operand);
                self.indent_level -= 1;
            },
        }
    }

    fn printPrimary(self: *AstPrinter, primary: *Ast.PrimaryExpr) std.Io.Writer.Error!void {
        try self.printIndent();
        switch (primary.*) {
            .int_literal => |t| try self.writer.print("IntLiteral: {s}\n", .{t.lexeme(self.source)}),
            .string_literal => |t| try self.writer.print("StringLiteral: {s}\n", .{t.lexeme(self.source)}),
            .multiline_string_literal => |_| try self.writer.print("MultilineString\n", .{}),
            .identifier => |t| try self.writer.print("Identifier: {s}\n", .{t.lexeme(self.source)}),
            .bool_literal => |t| try self.writer.print("BoolLiteral: {s}\n", .{t.lexeme(self.source)}),
            .null_literal => |_| try self.writer.print("NullLiteral\n", .{}),
            .undefined_literal => |_| try self.writer.print("UndefinedLiteral\n", .{}),
            .enum_literal => |t| try self.writer.print("EnumLiteral: {s}\n", .{t.lexeme(self.source)}),
            .type_expr => |t| {
                try self.writer.print("TypeExpr\n", .{});
                self.indent_level += 1;
                try self.printTypeExpr(t);
                self.indent_level -= 1;
            },
            .struct_init => |s| {
                try self.writer.print("StructInit\n", .{});
                self.indent_level += 1;
                if (s.type_expr) |t| {
                    try self.printExpression(t);
                }
                for (s.fields) |f| {
                    try self.printIndent();
                    try self.writer.print(".{s} = \n", .{f.name.lexeme(self.source)});
                    self.indent_level += 1;
                    try self.printExpression(f.value);
                    self.indent_level -= 1;
                }
                self.indent_level -= 1;
            },
            .asm_block => |a| {
                try self.writer.print("AsmBlock ({s})\n", .{if (a.is_pure) "pure" else "volatile"});
                self.indent_level += 1;
                for (a.template) |t| {
                    try self.printIndent();
                    try self.writer.print("Template: {s}\n", .{t.lexeme(self.source)});
                }
                for (a.outputs) |o| {
                    try self.printIndent();
                    try self.writer.print("Output [{s}] \"{s}\":\n", .{ o.name.lexeme(self.source), o.constraint.lexeme(self.source) });
                    self.indent_level += 1;
                    try self.printExpression(o.expr);
                    self.indent_level -= 1;
                }
                for (a.inputs) |i| {
                    try self.printIndent();
                    try self.writer.print("Input [{s}] \"{s}\":\n", .{ i.name.lexeme(self.source), i.constraint.lexeme(self.source) });
                    self.indent_level += 1;
                    try self.printExpression(i.expr);
                    self.indent_level -= 1;
                }
                for (a.clobbers) |c| {
                    try self.printIndent();
                    try self.writer.print("Clobber: {s}\n", .{c.lexeme(self.source)});
                }
                self.indent_level -= 1;
            },
            .builtin_call => |b| {
                try self.writer.print("BuiltinCall @{s}\n", .{b.name.lexeme(self.source)});
                self.indent_level += 1;
                for (b.args) |arg| {
                    try self.printExpression(arg);
                }
                self.indent_level -= 1;
            },
            .fn_literal => |f| {
                try self.writer.print("FnLiteral", .{});
                if (f.attribute) |attr| {
                    try self.writer.print(" .{s}", .{attr.lexeme(self.source)});
                }
                try self.writer.print("\n", .{});

                self.indent_level += 1;

                if (f.params.len > 0) {
                    try self.printIndent();
                    try self.writer.print("Params:\n", .{});
                    self.indent_level += 1;
                    for (f.params) |p| {
                        try self.printIndent();
                        try self.writer.print("{s}: \n", .{p.name.lexeme(self.source)});
                        self.indent_level += 1;
                        try self.printTypeExpr(p.type_expr);
                        self.indent_level -= 1;
                    }
                    self.indent_level -= 1;
                }

                try self.printIndent();
                try self.writer.print("Return:\n", .{});
                self.indent_level += 1;
                try self.printTypeExpr(f.return_type);
                self.indent_level -= 1;

                try self.printBlock(f.body);
                self.indent_level -= 1;
            },
            .grouped => |e| {
                try self.writer.print("Grouped\n", .{});
                self.indent_level += 1;
                try self.printExpression(e);
                self.indent_level -= 1;
            },
        }
    }

    fn printTypeExpr(self: *AstPrinter, type_expr: *Ast.TypeExpr) std.Io.Writer.Error!void {
        try self.printIndent();
        switch (type_expr.core) {
            .identifier => |t| try self.writer.print("TypeIdentifier: {s}\n", .{t.lexeme(self.source)}),
            .array => |a| {
                try self.writer.print("ArrayType\n", .{});
                self.indent_level += 1;
                try self.printExpression(a.size);
                try self.printTypeExpr(a.child_type);
                self.indent_level -= 1;
            },
            .fn_type => |_| try self.writer.print("FnType\n", .{}),
            else => try self.writer.print("ComplexType\n", .{}),
        }
    }
};
