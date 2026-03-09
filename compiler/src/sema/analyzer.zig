const std = @import("std");

const Token = @import("../tokenizer.zig").Token;
const Node = @import("../ast.zig").Node;
const Type = @import("type.zig").Type;
const TypeInterner = @import("type.zig").TypeInterner;

pub const Symbol = struct {
    name: Token,
    type: ?*Type,
    /// Used for error reporting
    decl_node: *Node,
    is_pub: bool,
    is_const: bool,
};

pub const Scope = struct {
    parent: ?*Scope,
    map: std.StringHashMap(*Symbol),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, parent: ?*Scope) Scope {
        return .{
            .parent = parent,
            .map = .init(allocator),
            .allocator = allocator,
        };
    }

    pub fn lookup(scope: *Scope, name: []const u8) ?*Symbol {
        if (scope.map.get(name)) |symbol| {
            return symbol;
        } else if (scope.parent) |parent| {
            return parent.lookup(name);
        }

        return null;
    }

    pub fn declare(scope: *Scope, name: []const u8, symbol: *Symbol) !void {
        if (scope.lookup(name) != null) {
            return error.DeclAlreadyDeclared;
        }

        try scope.map.put(name, symbol);
    }
};

pub const Analyzer = struct {
    arena: std.mem.Allocator,
    source: []const u8,
    root_scope: *Scope,
    current_scope: *Scope,
    /// Maps an expression node (like `1 + 2`) to its evaluated Type.
    type_map: std.AutoHashMap(*Node, *Type),
    /// Maps an identifier usage node (like `x`) to the Symbol it refers to.
    symbol_map: std.AutoHashMap(*Node, *Symbol),
    /// Cache to ensure every unique type only exists in memory exactly once.
    type_interner: TypeInterner,
    /// The return type of the function we are currently inside. null if global.
    expected_return_type: ?*Type = null,
    /// How many loops deep we are. 0 means we are not in a loop.
    loop_depth: usize = 0,
    /// Tracks the function depth. 0 means global. Used to ban closures.
    fn_depth: usize = 0,

    pub fn init(arena: std.mem.Allocator, source: []const u8) !Analyzer {
        const root_scope = try arena.create(Scope);
        root_scope.* = .{
            .allocator = arena,
            .map = .init(arena),
            .parent = null,
        };

        return .{
            .arena = arena,
            .source = source,
            .root_scope = root_scope,
            .current_scope = root_scope,
            .type_map = .init(arena),
            .symbol_map = .init(arena),
            .type_interner = .init(arena),
        };
    }

    /// Implementation Details: This is the main dispatcher. It just reads `node.tag` and
    /// forwards the node to the correct `analyzeX` helper below.
    pub fn analyze(self: *Analyzer, node: *Node) !void {
        switch (node.tag) {
            .root => {
                for (node.data.root.decls) |decl| {
                    try self.analyze(decl);
                }
            },
            .declaration => try self.analyzeDeclaration(node),
            .block => try self.analyzeBlock(node),
            .assignment => {
                try self.analyze(node.data.assignment.identifier);
                try self.analyzer(node.data.assignment.assignment_expr);
            },
        }
    }

    /// Implementation Details: Out-of-order declarations are supported.
    /// We need TWO passes over the root decls:
    /// 1. Register all Names into the root scope.
    /// 2. Actually analyze the bodies/values.
    fn analyzeRoot(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: Remember to `pushScope()` when entering and `popScope()` when leaving!
    fn analyzeBlock(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: Type Inference happens here!
    /// If `type_expr` is missing, analyze the `initial_value` first to figure out the type.
    /// Register the Symbol in the scope *before* analyzing the initializer to support
    /// recursion, but careful of using the type before it's fully resolved!
    fn analyzeDeclaration(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: Two things to verify here:
    /// 1. Is the `identifier` actually an l-value (a memory location) and NOT marked `const`?
    /// 2. Does the type of `assignment_expr` exactly match (or safely coerce to) the LHS type?
    fn analyzeAssignment(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The condition MUST resolve to the boolean type.
    /// If `if` is used as an expression (`const x = if(c) a else b;`), enforce
    /// that the `then_branch` and `else_branch` evaluate to the exact same type.
    fn analyzeIf(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: Increment `self.loop_depth` before analyzing the body, and decrement after!
    /// The condition must resolve to a boolean.
    fn analyzeWhile(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The `value_capture` is actually an implicit variable declaration!
    /// Push a new scope, create a Symbol for the capture, and assign it the `child`
    /// type of the iterable (Array or Slice). Increment `loop_depth` for the body.
    fn analyzeFor(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The target dictates the type. Every `switch_item` pattern must match the target's type.
    /// Every `switch_prong` body must evaluate to the same type if used as an expression.
    /// (TODO: Check for exhaustiveness so the user doesn't miss an enum variant).
    fn analyzeSwitch(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: Compare the evaluated type of the expression against `self.expected_return_type`.
    /// If `self.expected_return_type` is null, the user must have typed `return` outside of a function!
    fn analyzeReturn(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: Verify that `self.loop_depth > 0`. If it's 0, throw a control flow error.
    fn analyzeLoopControl(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: `errdefer` is only valid if the current function's return type is an Error Union.
    /// Defer bodies execute at the end of the scope, but they shouldn't alter the type of the block.
    fn analyzeDefer(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: This creates a new Scope for parameters.
    /// CRITICAL: Save the current `self.expected_return_type`, overwrite it with this function's
    /// return type, analyze the body, and then RESTORE the old expected return type. (Functions can be nested!)
    fn analyzeFnLiteral(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: When mapping structs/unions to your `TypeInterner`, calculate the total
    /// `size` and `alignment` of the type based on its fields.
    /// Beware of infinite types: `struct A { b: A }` is illegal, it must be `b: *A`.
    fn analyzeAggregateLiteral(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: Look up the name string in `self.current_scope`.
    /// If found, map the AST Node to the Symbol in `self.symbol_map` so Codegen can find it.
    /// Assign the Symbol's type to this Node in `self.type_map`.
    fn analyzeIdentifier(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The operator dictates the rules.
    /// `+`, `-`, `*` require numbers on both sides and return that same number type.
    /// `==`, `<` require matching types on both sides, but RETURN a boolean type!
    fn analyzeBinaryExpr(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: `-` requires a signed int/float. `not` requires a boolean.
    fn analyzeUnaryExpr(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The LHS must be a function type. The number of arguments must perfectly
    /// match the number of parameters, and the types must align exactly.
    /// The resulting type of this node is the return type of the function.
    fn analyzeFnCall(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The LHS must be an Array or Slice type.
    /// The index expression MUST evaluate to an integer (like `u32` or `usize`).
    /// The resulting type is the `child` type of the array/slice.
    fn analyzeIndexAccess(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The LHS must be a struct/union. Look up the field name in that
    /// specific Type's definition to ensure it exists, and then return that field's type.
    fn analyzeFieldAccess(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The LHS must evaluate to a pointer type (`*T`).
    /// The resulting type of this node is `T`.
    fn analyzePtrDereference(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: This is where AST syntax transforms into `*Type` objects via `TypeInterner`.
    /// E.g., read `.pointer_type`, evaluate the child, and call `interner.getPointerType(...)`.
    fn analyzeTypeExpr(self: *Analyzer, node: *Node) !void {
        _ = self;
        _ = node;
        return error.TODO;
    }

    /// Implementation Details: The compiler doesn't care about the literal's value right now,
    /// just immediately push the corresponding primitive type into `self.type_map`.
    fn analyzeLiteral(self: *Analyzer, node: *Node) !void {
        _ = self;

        switch (node.tag) {
            .int_literal => {},
            .string_literal => {},
            .char_literal => {},
            .null_literal => {},
            .unreachable_literal => {},
            .error_literal => {},
        }
    }
};
