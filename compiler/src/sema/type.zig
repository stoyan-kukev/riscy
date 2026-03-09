const std = @import("std");

pub const Type = struct {
    tag: Tag,
    size: usize,
    alignment: usize,

    pub const Tag = union(enum) {
        u8: void,
        u16: void,
        u32: void,
        i8: void,
        i16: void,
        i32: void,
        boolean: void,
        type: void,
        void_type: void,

        slice: Slice,
        array: Array,
        pointer: Pointer,

        pub const Slice = struct {
            child: *Type,
            is_const: bool,
        };

        pub const Array = struct {
            length: usize,
            child: *Type,
        };

        pub const Pointer = struct {
            child: *Type,
            kind: Kind,
            is_const: bool,
            is_allowzero: bool,

            pub const Kind = enum { normal, @"volatile", many };
        };
    };
};

pub const TypeInterner = struct {
    arena: std.mem.Allocator,

    map: std.AutoHashMap(Type, *Type),

    type_u8: *Type,
    type_u32: *Type,
    type_bool: *Type,
    type_void: *Type,
    type_null: *Type,
    type_type: *Type,

    pub fn init(arena: std.mem.Allocator) !TypeInterner {
        var self = TypeInterner{
            .arena = arena,
            .map = std.AutoHashMap(Type, *Type).init(arena),
            .type_u8 = undefined,
            .type_u32 = undefined,
            .type_bool = undefined,
            .type_void = undefined,
            .type_type = undefined,
            .type_null = undefined,
        };

        self.type_u8 = try self.intern(.{ .size = 1, .alignment = 1, .tag = .u8 });
        self.type_u32 = try self.intern(.{ .size = 4, .alignment = 4, .tag = .u32 });
        self.type_bool = try self.intern(.{ .size = 1, .alignment = 1, .tag = .boolean });
        self.type_void = try self.intern(.{ .size = 0, .alignment = 0, .tag = .void_type });
        self.type_void = try self.intern(.{ .size = 0, .alignment = 0, .tag = .null_type });
        self.type_type = try self.intern(.{ .size = 0, .alignment = 0, .tag = .type });

        return self;
    }

    /// Get a type. Ensures only one instance of a type ever exists.
    pub fn intern(self: *TypeInterner, signature: Type) !*Type {
        if (self.map.get(signature)) |existing_ptr| {
            return existing_ptr;
        }

        const new_ptr = try self.arena.create(Type);
        new_ptr.* = signature;

        try self.map.put(signature, new_ptr);

        return new_ptr;
    }

    /// Wraps a child type in a pointer. E.g., turning `u32` into `*const u32`
    pub fn getPointerType(
        self: *TypeInterner,
        child: *Type,
        kind: Type.Tag.Pointer.Kind,
        is_const: bool,
        is_allowzero: bool,
    ) !*Type {
        return try self.intern(.{ .size = 4, .alignment = 4, .tag = .{ .pointer = .{
            .child = child,
            .kind = kind,
            .is_const = is_const,
            .is_allowzero = is_allowzero,
        } } });
    }

    /// Wraps a child type in a slice. E.g., turning `u8` into `[]const u8`
    pub fn getSliceType(self: *TypeInterner, child: *Type, is_const: bool) !*Type {
        return try self.intern(.{ .size = 8, .alignment = 4, .tag = .{ .slice = .{
            .child = child,
            .is_const = is_const,
        } } });
    }
};
