with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;
with Interfaces.C.Pointers;
with Ada.Unchecked_Conversion;

package GDNative.Thin is

   package IC renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package ICE renames Interfaces.C.Extensions;

   GODOT_API_VERSION                  : constant := 1;  --  ../gdnative/gdnative.h:65
   GODOT_TRUE                         : constant := 1;  --  ../gdnative/gdnative.h:125
   GODOT_FALSE                        : constant := 0;  --  ../gdnative/gdnative.h:126
   GODOT_AABB_SIZE                    : constant := 24;  --  ../gdnative/aabb.h:40
   GODOT_ARRAY_SIZE                   : constant := 8;
   GODOT_BASIS_SIZE                   : constant := 36;  --  ../gdnative/basis.h:40
   GODOT_COLOR_SIZE                   : constant := 16;  --  ./gdnative/color.h:40
   GODOT_DICTIONARY_SIZE              : constant := 8;
   GODOT_NODE_PATH_SIZE               : constant := 8;
   GODOT_PLANE_SIZE                   : constant := 16;  --  ../gdnative/plane.h:40
   GODOT_QUAT_SIZE                    : constant := 16;  --  ./gdnative/quat.h:40
   GODOT_RECT_SIZE                    : constant := 16;
   GODOT_RID_SIZE                     : constant := 8;
   GODOT_STRING_SIZE                  : constant := 8;
   GODOT_CHAR_STRING_SIZE             : constant := 8;
   GODOT_STRING_NAME_SIZE             : constant := 8;
   GODOT_TRANSFORM2D_SIZE             : constant := 24;
   GODOT_TRANSFORM_SIZE               : constant := 48;
   GODOT_VARIANT_SIZE                 : constant := 24;
   GODOT_VECTOR2_SIZE                 : constant := 8;
   GODOT_VECTOR3_SIZE                 : constant := 12;
   GODOT_POOL_ARRAY_READ_ACCESS_SIZE  : constant := 1;
   GODOT_POOL_ARRAY_WRITE_ACCESS_SIZE : constant := 1;

   type godot_method_bind is private;
   type godot_aabb is private;
   type godot_array is private;
   type godot_basis is private;
   type godot_color is private;
   type godot_dictionary is private;
   type godot_node_path is private;
   type godot_plane is private;
   type godot_pool_byte_array is private;
   type godot_pool_int_array is private;
   type godot_pool_real_array is private;
   type godot_pool_string_array is private;
   type godot_pool_vector2_array is private;
   type godot_pool_vector3_array is private;
   type godot_pool_color_array is private;
   type godot_quat is private;
   type godot_rect2 is private;
   type godot_rid is private;
   type godot_string is private;
   type godot_char_string is private;
   type godot_string_name is private;
   type godot_transform2d is private;
   type godot_transform is private;
   type godot_variant is private;
   type godot_variant_ptr is access all godot_variant;
   type godot_vector2 is private;
   type godot_vector3 is private;
   type godot_pool_array_write_access is private;
   type godot_pool_array_read_access is private;
   subtype godot_pool_byte_array_read_access is godot_pool_array_read_access;  -- ../gdnative/pool_arrays.h:48
   subtype godot_pool_int_array_read_access is godot_pool_array_read_access;  -- ../gdnative/pool_arrays.h:49
   subtype godot_pool_real_array_read_access is godot_pool_array_read_access;  -- ../gdnative/pool_arrays.h:50
   subtype godot_pool_string_array_read_access is godot_pool_array_read_access;  -- ../gdnative/pool_arrays.h:51
   subtype godot_pool_vector2_array_read_access is godot_pool_array_read_access;  -- ../gdnative/pool_arrays.h:52
   subtype godot_pool_vector3_array_read_access is godot_pool_array_read_access;  -- ../gdnative/pool_arrays.h:53
   subtype godot_pool_color_array_read_access is godot_pool_array_read_access;  -- ../gdnative/pool_arrays.h:54

   subtype godot_pool_byte_array_write_access is godot_pool_array_write_access;  -- ../gdnative/pool_arrays.h:64
   subtype godot_pool_int_array_write_access is godot_pool_array_write_access;  -- ../gdnative/pool_arrays.h:65
   subtype godot_pool_real_array_write_access is godot_pool_array_write_access;  -- ../gdnative/pool_arrays.h:66
   subtype godot_pool_string_array_write_access is godot_pool_array_write_access;  -- ../gdnative/pool_arrays.h:67
   subtype godot_pool_vector2_array_write_access is godot_pool_array_write_access;  -- ../gdnative/pool_arrays.h:68
   subtype godot_pool_vector3_array_write_access is godot_pool_array_write_access;  -- ../gdnative/pool_arrays.h:69
   subtype godot_pool_color_array_write_access is godot_pool_array_write_access;  -- ../gdnative/pool_arrays.h:70

   -- This is for libraries *using* the header, NOT GODOT EXPOSING STUFF!!
   type godot_error is 
     (GODOT_OK,                        -- (0)
      GODOT_FAILED,                    -- < Generic fail error
      GODOT_ERR_UNAVAILABLE,           -- < What is requested is unsupported/unavailable
      GODOT_ERR_UNCONFIGURED,          -- < The object being used hasn't been properly set up yet
      GODOT_ERR_UNAUTHORIZED,          -- < Missing credentials for requested resource
      GODOT_ERR_PARAMETER_RANGE_ERROR, -- < Parameter given out of range (5)
      GODOT_ERR_OUT_OF_MEMORY,         -- < Out of memory
      GODOT_ERR_FILE_NOT_FOUND,
      GODOT_ERR_FILE_BAD_DRIVE,
      GODOT_ERR_FILE_BAD_PATH,
      GODOT_ERR_FILE_NO_PERMISSION,    -- (10)
      GODOT_ERR_FILE_ALREADY_IN_USE,
      GODOT_ERR_FILE_CANT_OPEN,
      GODOT_ERR_FILE_CANT_WRITE,
      GODOT_ERR_FILE_CANT_READ,
      GODOT_ERR_FILE_UNRECOGNIZED,     -- (15)
      GODOT_ERR_FILE_CORRUPT,
      GODOT_ERR_FILE_MISSING_DEPENDENCIES,
      GODOT_ERR_FILE_EOF,
      GODOT_ERR_CANT_OPEN,             -- < Can't open a resource/socket/file
      GODOT_ERR_CANT_CREATE,           -- (20)
      GODOT_ERR_QUERY_FAILED,
      GODOT_ERR_ALREADY_IN_USE,
      GODOT_ERR_LOCKED,                -- < resource is locked
      GODOT_ERR_TIMEOUT,
      GODOT_ERR_CANT_CONNECT,          -- (25)
      GODOT_ERR_CANT_RESOLVE,
      GODOT_ERR_CONNECTION_ERROR,
      GODOT_ERR_CANT_ACQUIRE_RESOURCE,
      GODOT_ERR_CANT_FORK,
      GODOT_ERR_INVALID_DATA,          -- < Data passed is invalid (30)
      GODOT_ERR_INVALID_PARAMETER,     -- < Parameter passed is invalid
      GODOT_ERR_ALREADY_EXISTS,        -- < When adding, item already exists
      GODOT_ERR_DOES_NOT_EXIST,        -- < When retrieving/erasing, it item does not exist
      GODOT_ERR_DATABASE_CANT_READ,    -- < database is full
      GODOT_ERR_DATABASE_CANT_WRITE,   -- < database is full (35)
      GODOT_ERR_COMPILATION_FAILED,
      GODOT_ERR_METHOD_NOT_FOUND,
      GODOT_ERR_LINK_FAILED,
      GODOT_ERR_SCRIPT_FAILED,
      GODOT_ERR_CYCLIC_LINK,           -- (40)
      GODOT_ERR_INVALID_DECLARATION,
      GODOT_ERR_DUPLICATE_SYMBOL,
      GODOT_ERR_PARSE_ERROR,
      GODOT_ERR_BUSY,
      GODOT_ERR_SKIP,                  -- (45)
      GODOT_ERR_HELP,                  -- < user requested help!!
      GODOT_ERR_BUG,                   -- < a bug in the software certainly happened, due to a double check failing or unexpected behavior.
      GODOT_ERR_PRINTER_ON_FIRE);      -- the parallel port printer is engulfed in flames
   pragma Convention (C, godot_error); -- ../gdnative/gdnative.h:119

   type godot_variant_call_error is 
     (GODOT_CALL_ERROR_CALL_OK,
      GODOT_CALL_ERROR_CALL_ERROR_INVALID_METHOD,
      GODOT_CALL_ERROR_CALL_ERROR_INVALID_ARGUMENT,
      GODOT_CALL_ERROR_CALL_ERROR_TOO_MANY_ARGUMENTS,
      GODOT_CALL_ERROR_CALL_ERROR_TOO_FEW_ARGUMENTS,
      GODOT_CALL_ERROR_CALL_ERROR_INSTANCE_IS_NULL);
   pragma Convention (C, godot_variant_call_error);  -- ../gdnative/variant.h:88

   type godot_variant_operator is 
     (GODOT_VARIANT_OP_EQUAL,
      GODOT_VARIANT_OP_NOT_EQUAL,
      GODOT_VARIANT_OP_LESS,
      GODOT_VARIANT_OP_LESS_EQUAL,
      GODOT_VARIANT_OP_GREATER,
      GODOT_VARIANT_OP_GREATER_EQUAL,
      GODOT_VARIANT_OP_ADD,
      GODOT_VARIANT_OP_SUBTRACT,
      GODOT_VARIANT_OP_MULTIPLY,
      GODOT_VARIANT_OP_DIVIDE,
      GODOT_VARIANT_OP_NEGATE,
      GODOT_VARIANT_OP_POSITIVE,
      GODOT_VARIANT_OP_MODULE,
      GODOT_VARIANT_OP_STRING_CONCAT,
      GODOT_VARIANT_OP_SHIFT_LEFT,
      GODOT_VARIANT_OP_SHIFT_RIGHT,
      GODOT_VARIANT_OP_BIT_AND,
      GODOT_VARIANT_OP_BIT_OR,
      GODOT_VARIANT_OP_BIT_XOR,
      GODOT_VARIANT_OP_BIT_NEGATE,
      GODOT_VARIANT_OP_AND,
      GODOT_VARIANT_OP_OR,
      GODOT_VARIANT_OP_XOR,
      GODOT_VARIANT_OP_NOT,
      GODOT_VARIANT_OP_IN,
      GODOT_VARIANT_OP_MAX);
   pragma Convention (C, godot_variant_operator);  -- ./gdnative/variant.h:103

   type godot_vector3_axis is 
     (GODOT_VECTOR3_AXIS_X,
      GODOT_VECTOR3_AXIS_Y,
      GODOT_VECTOR3_AXIS_Z);
   pragma Convention (C, godot_vector3_axis);  -- ./gdnative/vector3.h:65

   type godot_variant_type is 
     (GODOT_VARIANT_TYPE_NIL,
      GODOT_VARIANT_TYPE_BOOL,
      GODOT_VARIANT_TYPE_INT,
      GODOT_VARIANT_TYPE_REAL,
      GODOT_VARIANT_TYPE_STRING,
      GODOT_VARIANT_TYPE_VECTOR2,
      GODOT_VARIANT_TYPE_RECT2,
      GODOT_VARIANT_TYPE_VECTOR3,
      GODOT_VARIANT_TYPE_TRANSFORM2D,
      GODOT_VARIANT_TYPE_PLANE,
      GODOT_VARIANT_TYPE_QUAT,
      GODOT_VARIANT_TYPE_AABB,
      GODOT_VARIANT_TYPE_BASIS,
      GODOT_VARIANT_TYPE_TRANSFORM,
      GODOT_VARIANT_TYPE_COLOR,
      GODOT_VARIANT_TYPE_NODE_PATH,
      GODOT_VARIANT_TYPE_RID,
      GODOT_VARIANT_TYPE_OBJECT,
      GODOT_VARIANT_TYPE_DICTIONARY,
      GODOT_VARIANT_TYPE_ARRAY,
      GODOT_VARIANT_TYPE_POOL_BYTE_ARRAY,
      GODOT_VARIANT_TYPE_POOL_INT_ARRAY,
      GODOT_VARIANT_TYPE_POOL_REAL_ARRAY,
      GODOT_VARIANT_TYPE_POOL_STRING_ARRAY,
      GODOT_VARIANT_TYPE_POOL_VECTOR2_ARRAY,
      GODOT_VARIANT_TYPE_POOL_VECTOR3_ARRAY,
      GODOT_VARIANT_TYPE_POOL_COLOR_ARRAY);
   pragma Convention (C, godot_variant_type);  -- ./gdnative/variant.h:49

   type GDNATIVE_API_TYPES is 
     (GDNATIVE_CORE,
      GDNATIVE_EXT_NATIVESCRIPT,
      GDNATIVE_EXT_PLUGINSCRIPT,
      GDNATIVE_EXT_ANDROID,
      GDNATIVE_EXT_ARVR,
      GDNATIVE_EXT_VIDEODECODER,
      GDNATIVE_EXT_NET);
   pragma Convention (C, GDNATIVE_API_TYPES);  -- gdnative_api_struct.gen.h:45

   -- bool
   subtype godot_bool is ICE.bool;  -- ../gdnative/gdnative.h:123

   -- int
   subtype godot_int is IC.int;  -- ../gdnative/gdnative.h:130

   -- real
   subtype godot_real is IC.C_float;  -- ../gdnative/gdnative.h:134

   -- char
   subtype godot_char_type is IC.wchar_t;  -- ../gdnative/string.h:41

   -- Object (forward declared)
   subtype godot_object is System.Address;  -- ../gdnative/gdnative.h:137
   Null_Godot_Object : constant godot_object := System.Null_Address;

  -- Script API
   type godot_gdnative_api_version is record
      major : aliased IC.unsigned;  -- ../gdnative/gdnative.h:228
      minor : aliased IC.unsigned;  -- ../gdnative/gdnative.h:229
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_api_version);  -- ../gdnative/gdnative.h:227

   type godot_gdnative_api_struct;
   type godot_gdnative_api_struct_ptr is access godot_gdnative_api_struct;
   type godot_gdnative_api_struct_ptr_array is array (IC.unsigned range <>) of aliased godot_gdnative_api_struct_ptr;
   type godot_gdnative_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- ../gdnative/gdnative.h:235
      version : aliased godot_gdnative_api_version;  -- ../gdnative/gdnative.h:236
      next : access constant godot_gdnative_api_struct;  -- ../gdnative/gdnative.h:237
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_api_struct);  -- ../gdnative/gdnative.h:234

   package GDnative_Api_Struct_Pointers is new Interfaces.C.Pointers (
      Index => IC.unsigned, 
      Element => godot_gdnative_api_struct_ptr,
      Element_Array => godot_gdnative_api_struct_ptr_array,
      Default_Terminator => null);

   type godot_gdnative_core_api_struct;
   type godot_gdnative_core_api_struct_ptr is access all godot_gdnative_core_api_struct;
   type godot_gdnative_init_options is record
      in_editor : aliased godot_bool;  -- ../gdnative/gdnative.h:243
      core_api_hash : aliased ICE.long_long;  -- ../gdnative/gdnative.h:244
      editor_api_hash : aliased ICE.long_long;  -- ../gdnative/gdnative.h:245
      no_api_hash : aliased ICE.long_long;  -- ../gdnative/gdnative.h:246
      report_version_mismatch : access procedure
           (arg1 : System.Address;
            arg2 : ICS.chars_ptr;
            arg3 : godot_gdnative_api_version;
            arg4 : godot_gdnative_api_version);  -- ../gdnative/gdnative.h:247
      report_loading_error : access procedure (arg1 : System.Address; arg2 : ICS.chars_ptr);  -- ../gdnative/gdnative.h:248
      gd_native_library : System.Address;  -- ../gdnative/gdnative.h:249
      api_struct : godot_gdnative_core_api_struct_ptr;  -- ../gdnative/gdnative.h:250
      active_library_path : godot_string;  -- ../gdnative/gdnative.h:251
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_init_options);  -- ../gdnative/gdnative.h:252


   type godot_gdnative_terminate_options is record
      in_editor : aliased godot_bool;  -- ../gdnative/gdnative.h:255
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_terminate_options);  -- ../gdnative/gdnative.h:256

   -- Calling convention?
   type godot_class_constructor is access function return System.Address;
   pragma Convention (C, godot_class_constructor);  -- ../gdnative/gdnative.h:259

   -- GDNative procedure types
   type godot_gdnative_init_fn is access procedure (arg1 : access godot_gdnative_init_options);
   pragma Convention (C, godot_gdnative_init_fn);  -- ../gdnative/gdnative.h:266

   type godot_gdnative_terminate_fn is access procedure (arg1 : access godot_gdnative_terminate_options);
   pragma Convention (C, godot_gdnative_terminate_fn);  -- ../gdnative/gdnative.h:267

   type godot_gdnative_procedure_fn is access function (arg1 : access godot_array) return godot_variant;
   pragma Convention (C, godot_gdnative_procedure_fn);  -- ../gdnative/gdnative.h:268

   -- System Functions
   type native_call_cb is access function (arg1 : System.Address; arg2 : access godot_array) return godot_variant;
   pragma Convention (C, native_call_cb);  -- ../gdnative/gdnative.h:272

   ------------------
   -- Nativescript --
   ------------------

   subtype godot_method_rpc_mode is IC.unsigned;
   GODOT_METHOD_RPC_MODE_DISABLED   : constant godot_method_rpc_mode := 0;
   GODOT_METHOD_RPC_MODE_REMOTE     : constant godot_method_rpc_mode := 1;
   GODOT_METHOD_RPC_MODE_MASTER     : constant godot_method_rpc_mode := 2;
   GODOT_METHOD_RPC_MODE_PUPPET     : constant godot_method_rpc_mode := 3;
   GODOT_METHOD_RPC_MODE_SLAVE      : constant godot_method_rpc_mode := 3;
   GODOT_METHOD_RPC_MODE_REMOTESYNC : constant godot_method_rpc_mode := 4;
   GODOT_METHOD_RPC_MODE_SYNC       : constant godot_method_rpc_mode := 4;
   GODOT_METHOD_RPC_MODE_MASTERSYNC : constant godot_method_rpc_mode := 5;
   GODOT_METHOD_RPC_MODE_PUPPETSYNC : constant godot_method_rpc_mode := 6;  -- ./nativescript/godot_nativescript.h:50

   type godot_property_hint is 
     (GODOT_PROPERTY_HINT_NONE,
      GODOT_PROPERTY_HINT_RANGE,
      GODOT_PROPERTY_HINT_EXP_RANGE,
      GODOT_PROPERTY_HINT_ENUM,
      GODOT_PROPERTY_HINT_EXP_EASING,
      GODOT_PROPERTY_HINT_LENGTH,
      GODOT_PROPERTY_HINT_SPRITE_FRAME,
      GODOT_PROPERTY_HINT_KEY_ACCEL,
      GODOT_PROPERTY_HINT_FLAGS,
      GODOT_PROPERTY_HINT_LAYERS_2D_RENDER,
      GODOT_PROPERTY_HINT_LAYERS_2D_PHYSICS,
      GODOT_PROPERTY_HINT_LAYERS_3D_RENDER,
      GODOT_PROPERTY_HINT_LAYERS_3D_PHYSICS,
      GODOT_PROPERTY_HINT_FILE,
      GODOT_PROPERTY_HINT_DIR,
      GODOT_PROPERTY_HINT_GLOBAL_FILE,
      GODOT_PROPERTY_HINT_GLOBAL_DIR,
      GODOT_PROPERTY_HINT_RESOURCE_TYPE,
      GODOT_PROPERTY_HINT_MULTILINE_TEXT,
      GODOT_PROPERTY_HINT_PLACEHOLDER_TEXT,
      GODOT_PROPERTY_HINT_COLOR_NO_ALPHA,
      GODOT_PROPERTY_HINT_IMAGE_COMPRESS_LOSSY,
      GODOT_PROPERTY_HINT_IMAGE_COMPRESS_LOSSLESS,
      GODOT_PROPERTY_HINT_OBJECT_ID,
      GODOT_PROPERTY_HINT_TYPE_STRING,
      GODOT_PROPERTY_HINT_NODE_PATH_TO_EDITED_NODE,
      GODOT_PROPERTY_HINT_METHOD_OF_VARIANT_TYPE,
      GODOT_PROPERTY_HINT_METHOD_OF_BASE_TYPE,
      GODOT_PROPERTY_HINT_METHOD_OF_INSTANCE,
      GODOT_PROPERTY_HINT_METHOD_OF_SCRIPT,
      GODOT_PROPERTY_HINT_PROPERTY_OF_VARIANT_TYPE,
      GODOT_PROPERTY_HINT_PROPERTY_OF_BASE_TYPE,
      GODOT_PROPERTY_HINT_PROPERTY_OF_INSTANCE,
      GODOT_PROPERTY_HINT_PROPERTY_OF_SCRIPT,
      GODOT_PROPERTY_HINT_MAX);
   pragma Convention (C, godot_property_hint);  -- ./nativescript/godot_nativescript.h:88

   subtype godot_property_usage_flags is IC.unsigned;
   GODOT_PROPERTY_USAGE_STORAGE                : constant godot_property_usage_flags := 1;
   GODOT_PROPERTY_USAGE_EDITOR                 : constant godot_property_usage_flags := 2;
   GODOT_PROPERTY_USAGE_NETWORK                : constant godot_property_usage_flags := 4;
   GODOT_PROPERTY_USAGE_EDITOR_HELPER          : constant godot_property_usage_flags := 8;
   GODOT_PROPERTY_USAGE_CHECKABLE              : constant godot_property_usage_flags := 16;
   GODOT_PROPERTY_USAGE_CHECKED                : constant godot_property_usage_flags := 32;
   GODOT_PROPERTY_USAGE_INTERNATIONALIZED      : constant godot_property_usage_flags := 64;
   GODOT_PROPERTY_USAGE_GROUP                  : constant godot_property_usage_flags := 128;
   GODOT_PROPERTY_USAGE_CATEGORY               : constant godot_property_usage_flags := 256;
   GODOT_PROPERTY_USAGE_STORE_IF_NONZERO       : constant godot_property_usage_flags := 512;
   GODOT_PROPERTY_USAGE_STORE_IF_NONONE        : constant godot_property_usage_flags := 1024;
   GODOT_PROPERTY_USAGE_NO_INSTANCE_STATE      : constant godot_property_usage_flags := 2048;
   GODOT_PROPERTY_USAGE_RESTART_IF_CHANGED     : constant godot_property_usage_flags := 4096;
   GODOT_PROPERTY_USAGE_SCRIPT_VARIABLE        : constant godot_property_usage_flags := 8192;
   GODOT_PROPERTY_USAGE_STORE_IF_NULL          : constant godot_property_usage_flags := 16384;
   GODOT_PROPERTY_USAGE_ANIMATE_AS_TRIGGER     : constant godot_property_usage_flags := 32768;
   GODOT_PROPERTY_USAGE_UPDATE_ALL_IF_MODIFIED : constant godot_property_usage_flags := 65536;
   GODOT_PROPERTY_USAGE_DEFAULT                : constant godot_property_usage_flags := 7;
   GODOT_PROPERTY_USAGE_DEFAULT_INTL           : constant godot_property_usage_flags := 71;
   GODOT_PROPERTY_USAGE_NOEDITOR               : constant godot_property_usage_flags := 5;  -- ./nativescript/godot_nativescript.h:113

   type godot_property_attributes is record
      rset_type : aliased godot_method_rpc_mode;  -- ./nativescript/godot_nativescript.h:116
      c_type : aliased godot_int;  -- ./nativescript/godot_nativescript.h:118
      hint : aliased godot_property_hint;  -- ./nativescript/godot_nativescript.h:119
      hint_string : aliased godot_string;  -- ./nativescript/godot_nativescript.h:120
      usage : aliased godot_property_usage_flags;  -- ./nativescript/godot_nativescript.h:121
      default_value : aliased godot_variant;  -- ./nativescript/godot_nativescript.h:122
   end record;
   pragma Convention (C_Pass_By_Copy, godot_property_attributes);  -- ./nativescript/godot_nativescript.h:123

   type godot_instance_create_func is record
      create_func : access function (arg1 : System.Address; arg2 : System.Address) return System.Address;  -- ./nativescript/godot_nativescript.h:127
      method_data : System.Address;  -- ./nativescript/godot_nativescript.h:128
      free_func : access procedure (arg1 : System.Address);  -- ./nativescript/godot_nativescript.h:129
   end record;
   pragma Convention (C_Pass_By_Copy, godot_instance_create_func);  -- ./nativescript/godot_nativescript.h:130

   type godot_instance_destroy_func is record
      destroy_func : access procedure
           (p_instance : System.Address;
            p_method_data : System.Address;
            p_user_data : System.Address);  -- ./nativescript/godot_nativescript.h:134
      method_data : System.Address;  -- ./nativescript/godot_nativescript.h:135
      free_func : access procedure (arg1 : System.Address);  -- ./nativescript/godot_nativescript.h:136
   end record;
   pragma Convention (C_Pass_By_Copy, godot_instance_destroy_func);  -- ./nativescript/godot_nativescript.h:137

   type godot_method_attributes is record
      rpc_type : aliased godot_method_rpc_mode;  -- ./nativescript/godot_nativescript.h:144
   end record;
   pragma Convention (C_Pass_By_Copy, godot_method_attributes);  -- ./nativescript/godot_nativescript.h:145

   type godot_variant_ptr_array is array (IC.unsigned range <>) of aliased godot_variant_ptr;

   package Godot_Instance_Method_Args_Ptrs is new Interfaces.C.Pointers (
      Index => IC.unsigned, 
      Element => godot_variant_ptr,
      Element_Array => godot_variant_ptr_array,
      Default_Terminator => null);

   package WChar_T_Ptrs is new Interfaces.C.Pointers (
      Index => IC.size_t, 
      Element => IC.wchar_t,
      Element_Array => IC.wchar_array,
      Default_Terminator => IC.wide_nul);

   type access_godot_instance_method is access function
           (arg1 : System.Address;
            arg2 : System.Address;
            arg3 : System.Address;
            arg4 : IC.int;
            arg5 : Godot_Instance_Method_Args_Ptrs.Pointer) return godot_variant;

   type godot_instance_method is record
      method : access_godot_instance_method;  -- ./nativescript/godot_nativescript.h:149
      method_data : System.Address;  -- ./nativescript/godot_nativescript.h:150
      free_func : access procedure (arg1 : System.Address);  -- ./nativescript/godot_nativescript.h:151
   end record;
   pragma Convention (C_Pass_By_Copy, godot_instance_method);  -- ./nativescript/godot_nativescript.h:152

   type godot_property_set_func is record
      set_func : access procedure
           (arg1 : System.Address;
            arg2 : System.Address;
            arg3 : System.Address;
            arg4 : access godot_variant);  -- ./nativescript/godot_nativescript.h:158
      method_data : System.Address;  -- ./nativescript/godot_nativescript.h:159
      free_func : access procedure (arg1 : System.Address);  -- ./nativescript/godot_nativescript.h:160
   end record;
   pragma Convention (C_Pass_By_Copy, godot_property_set_func);  -- ./nativescript/godot_nativescript.h:161

   type godot_property_get_func is record
      get_func : access function
           (arg1 : System.Address;
            arg2 : System.Address;
            arg3 : System.Address) return godot_variant;  -- ./nativescript/godot_nativescript.h:165
      method_data : System.Address;  -- ./nativescript/godot_nativescript.h:166
      free_func : access procedure (arg1 : System.Address);  -- ./nativescript/godot_nativescript.h:167
   end record;
   pragma Convention (C_Pass_By_Copy, godot_property_get_func);  -- ./nativescript/godot_nativescript.h:168

   type godot_signal_argument is record
      name : aliased godot_string;  -- ./nativescript/godot_nativescript.h:173
      c_type : aliased godot_int;  -- ./nativescript/godot_nativescript.h:174
      hint : aliased godot_property_hint;  -- ./nativescript/godot_nativescript.h:175
      hint_string : aliased godot_string;  -- ./nativescript/godot_nativescript.h:176
      usage : aliased godot_property_usage_flags;  -- ./nativescript/godot_nativescript.h:177
      default_value : aliased godot_variant;  -- ./nativescript/godot_nativescript.h:178
   end record;
   pragma Convention (C_Pass_By_Copy, godot_signal_argument);  -- ./nativescript/godot_nativescript.h:179

   type godot_signal is record
      name : aliased godot_string;  -- ./nativescript/godot_nativescript.h:182
      num_args : aliased IC.int;  -- ./nativescript/godot_nativescript.h:183
      args : access godot_signal_argument;  -- ./nativescript/godot_nativescript.h:184
      num_default_args : aliased IC.int;  -- ./nativescript/godot_nativescript.h:185
      default_args : access godot_variant;  -- ./nativescript/godot_nativescript.h:186
   end record;
   pragma Convention (C_Pass_By_Copy, godot_signal);  -- ./nativescript/godot_nativescript.h:187

   type godot_method_arg is record
      name : aliased godot_string;  -- ./nativescript/godot_nativescript.h:204
      c_type : aliased godot_variant_type;  -- ./nativescript/godot_nativescript.h:206
      hint : aliased godot_property_hint;  -- ./nativescript/godot_nativescript.h:207
      hint_string : aliased godot_string;  -- ./nativescript/godot_nativescript.h:208
   end record;
   pragma Convention (C_Pass_By_Copy, godot_method_arg);  -- ./nativescript/godot_nativescript.h:209

   type godot_instance_binding_functions is record
      alloc_instance_binding_data : access function
           (arg1 : System.Address;
            arg2 : System.Address;
            arg3 : System.Address) return System.Address;  -- ./nativescript/godot_nativescript.h:231
      free_instance_binding_data : access procedure (arg1 : System.Address; arg2 : System.Address);  -- ./nativescript/godot_nativescript.h:232
      refcount_incremented_instance_binding : access procedure (arg1 : System.Address; arg2 : System.Address);  -- ./nativescript/godot_nativescript.h:233
      refcount_decremented_instance_binding : access function (arg1 : System.Address; arg2 : System.Address) return ICE.bool;  -- ./nativescript/godot_nativescript.h:234
      data : System.Address;  -- ./nativescript/godot_nativescript.h:235
      free_func : access procedure (arg1 : System.Address);  -- ./nativescript/godot_nativescript.h:236
   end record;
   pragma Convention (C_Pass_By_Copy, godot_instance_binding_functions);  -- ./nativescript/godot_nativescript.h:237

   type godot_print_error_procedure is access procedure
     (p_description : ICS.chars_ptr;
      p_function : ICS.chars_ptr;
      p_file : ICS.chars_ptr;
      p_line : IC.int);

   ------------------
   -- GDNATIVE_API --
   ------------------
   type godot_gdnative_ext_nativescript_1_1_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:56
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:57
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:58
      godot_nativescript_set_method_argument_information : access procedure
           (arg1 : System.Address;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : IC.int;
            arg5 : access constant godot_method_arg);  -- gdnative_api_struct.gen.h:59
      godot_nativescript_set_class_documentation : access procedure
           (arg1 : System.Address;
            arg2 : ICS.chars_ptr;
            arg3 : godot_string);  -- gdnative_api_struct.gen.h:60
      godot_nativescript_set_method_documentation : access procedure
           (arg1 : System.Address;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : godot_string);  -- gdnative_api_struct.gen.h:61
      godot_nativescript_set_property_documentation : access procedure
           (arg1 : System.Address;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : godot_string);  -- gdnative_api_struct.gen.h:62
      godot_nativescript_set_signal_documentation : access procedure
           (arg1 : System.Address;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : godot_string);  -- gdnative_api_struct.gen.h:63
      godot_nativescript_set_global_type_tag : access procedure
           (arg1 : IC.int;
            arg2 : ICS.chars_ptr;
            arg3 : System.Address);  -- gdnative_api_struct.gen.h:64
      godot_nativescript_get_global_type_tag : access function (arg1 : IC.int; arg2 : ICS.chars_ptr) return System.Address;  -- gdnative_api_struct.gen.h:65
      godot_nativescript_set_type_tag : access procedure
           (arg1 : System.Address;
            arg2 : ICS.chars_ptr;
            arg3 : System.Address);  -- gdnative_api_struct.gen.h:66
      godot_nativescript_get_type_tag : access function (arg1 : System.Address) return System.Address;  -- gdnative_api_struct.gen.h:67
      godot_nativescript_register_instance_binding_data_functions : access function (arg1 : godot_instance_binding_functions) return IC.int;  -- gdnative_api_struct.gen.h:68
      godot_nativescript_unregister_instance_binding_data_functions : access procedure (arg1 : IC.int);  -- gdnative_api_struct.gen.h:69
      godot_nativescript_get_instance_binding_data : access function (arg1 : IC.int; arg2 : System.Address) return System.Address;  -- gdnative_api_struct.gen.h:70
      godot_nativescript_profiling_add_data : access procedure (arg1 : ICS.chars_ptr; arg2 : ICE.long_long);  -- gdnative_api_struct.gen.h:71
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_nativescript_1_1_api_struct);  -- gdnative_api_struct.gen.h:55

   ----------------------
   -- Nativescript API --
   ----------------------
   type Singleton_Handle is new System.Address;
   NULL_SINGLETON_HANDLE : constant Singleton_Handle := Singleton_Handle (System.Null_Address);
   
   type Nativescript_Handle is new System.Address;
   NULL_NATIVESCRIPT_HANDLE : constant Nativescript_Handle := Nativescript_Handle (System.Null_Address);

   type godot_gdnative_ext_nativescript_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:75
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:76
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:77
      godot_nativescript_register_class : access procedure
           (arg1 : Nativescript_Handle;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : godot_instance_create_func;
            arg5 : godot_instance_destroy_func);  -- gdnative_api_struct.gen.h:78
      godot_nativescript_register_tool_class : access procedure
           (arg1 : Nativescript_Handle;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : godot_instance_create_func;
            arg5 : godot_instance_destroy_func);  -- gdnative_api_struct.gen.h:79
      godot_nativescript_register_method : access procedure
           (arg1 : Nativescript_Handle;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : godot_method_attributes;
            arg5 : godot_instance_method);  -- gdnative_api_struct.gen.h:80
      godot_nativescript_register_property : access procedure
           (arg1 : Nativescript_Handle;
            arg2 : ICS.chars_ptr;
            arg3 : ICS.chars_ptr;
            arg4 : access godot_property_attributes;
            arg5 : godot_property_set_func;
            arg6 : godot_property_get_func);  -- gdnative_api_struct.gen.h:81
      godot_nativescript_register_signal : access procedure
           (arg1 : Nativescript_Handle;
            arg2 : ICS.chars_ptr;
            arg3 : access constant godot_signal);  -- gdnative_api_struct.gen.h:82
      godot_nativescript_get_userdata : access function (arg1 : System.Address) return System.Address;  -- gdnative_api_struct.gen.h:83
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_nativescript_api_struct);  -- gdnative_api_struct.gen.h:74
   
   type godot_gdnative_ext_nativescript_api_struct_ptr is access constant godot_gdnative_ext_nativescript_api_struct with Convention => C;
   
   function To_Api_Struct_Ptr is new Ada.Unchecked_Conversion (godot_gdnative_api_struct_ptr, godot_gdnative_ext_nativescript_api_struct_ptr);
   
   -----------------------
   -- PluginScript API  --
   -----------------------
   type godot_gdnative_ext_pluginscript_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:87
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:88
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:89
      godot_pluginscript_register_language : access procedure (arg1 : access IC.int);  -- gdnative_api_struct.gen.h:90
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_pluginscript_api_struct);  -- gdnative_api_struct.gen.h:86

   type godot_gdnative_ext_pluginscript_api_struct_ptr is access constant godot_gdnative_ext_pluginscript_api_struct with Convention => C;

   function To_Api_Struct_Ptr is new Ada.Unchecked_Conversion (godot_gdnative_api_struct_ptr, godot_gdnative_ext_pluginscript_api_struct_ptr);

   ------------------
   -- Android API  --
   ------------------
   type godot_gdnative_ext_android_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:94
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:95
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:96
      godot_android_is_activity_resumed : access function return ICE.bool;  -- gdnative_api_struct.gen.h:100
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_android_api_struct);  -- gdnative_api_struct.gen.h:93

   type godot_gdnative_ext_android_api_struct_ptr is access constant godot_gdnative_ext_android_api_struct with Convention => C;
   function To_Api_Struct_Ptr is new Ada.Unchecked_Conversion (godot_gdnative_api_struct_ptr, godot_gdnative_ext_android_api_struct_ptr);

   --------------
   -- ARVR API --
   --------------
   type godot_gdnative_ext_arvr_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:104
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:105
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:106
      godot_arvr_register_interface : access procedure (arg1 : access IC.int);  -- gdnative_api_struct.gen.h:107
      godot_arvr_get_worldscale : access function return godot_real;  -- gdnative_api_struct.gen.h:108
      godot_arvr_get_reference_frame : access function return godot_transform;  -- gdnative_api_struct.gen.h:109
      godot_arvr_blit : access procedure
           (arg1 : IC.int;
            arg2 : access godot_rid;
            arg3 : access godot_rect2);  -- gdnative_api_struct.gen.h:110
      godot_arvr_get_texid : access function (arg1 : access godot_rid) return godot_int;  -- gdnative_api_struct.gen.h:111
      godot_arvr_add_controller : access function
           (arg1 : ICS.chars_ptr;
            arg2 : godot_int;
            arg3 : godot_bool;
            arg4 : godot_bool) return godot_int;  -- gdnative_api_struct.gen.h:112
      godot_arvr_remove_controller : access procedure (arg1 : godot_int);  -- gdnative_api_struct.gen.h:113
      godot_arvr_set_controller_transform : access procedure
           (arg1 : godot_int;
            arg2 : access godot_transform;
            arg3 : godot_bool;
            arg4 : godot_bool);  -- gdnative_api_struct.gen.h:114
      godot_arvr_set_controller_button : access procedure
           (arg1 : godot_int;
            arg2 : godot_int;
            arg3 : godot_bool);  -- gdnative_api_struct.gen.h:115
      godot_arvr_set_controller_axis : access procedure
           (arg1 : godot_int;
            arg2 : godot_int;
            arg3 : godot_real;
            arg4 : godot_bool);  -- gdnative_api_struct.gen.h:116
      godot_arvr_get_controller_rumble : access function (arg1 : godot_int) return godot_real;  -- gdnative_api_struct.gen.h:117
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_arvr_api_struct);  -- gdnative_api_struct.gen.h:103

   type godot_gdnative_ext_arvr_api_struct_ptr is access constant godot_gdnative_ext_arvr_api_struct with Convention => C;
   function To_Api_Struct_Ptr is new Ada.Unchecked_Conversion (godot_gdnative_api_struct_ptr, godot_gdnative_ext_arvr_api_struct_ptr);

   ----------------------
   -- Videodecoder API --
   ----------------------
   type godot_gdnative_ext_videodecoder_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:121
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:122
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:123
      godot_videodecoder_file_read : access function
           (arg1 : System.Address;
            arg2 : access IC.unsigned_char;
            arg3 : IC.int) return godot_int;  -- gdnative_api_struct.gen.h:124
      godot_videodecoder_file_seek : access function
           (arg1 : System.Address;
            arg2 : IC.long;
            arg3 : IC.int) return IC.long; -- gdnative_api_struct.gen.h:125
      godot_videodecoder_register_decoder : access procedure (arg1 : access IC.int);  -- gdnative_api_struct.gen.h:126
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_videodecoder_api_struct);  -- gdnative_api_struct.gen.h:120

   type godot_gdnative_ext_videodecoder_api_struct_ptr is access constant godot_gdnative_ext_videodecoder_api_struct with Convention => C;
   function To_Api_Struct_Ptr is new Ada.Unchecked_Conversion (godot_gdnative_api_struct_ptr, godot_gdnative_ext_videodecoder_api_struct_ptr);


   type godot_gdnative_ext_net_3_2_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:130
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:131
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:132
      godot_net_set_webrtc_library : access function (arg1 : access IC.int) return godot_error;  -- gdnative_api_struct.gen.h:133
      godot_net_bind_webrtc_peer_connection : access procedure (arg1 : System.Address; arg2 : access IC.int);  -- gdnative_api_struct.gen.h:134
      godot_net_bind_webrtc_data_channel : access procedure (arg1 : System.Address; arg2 : access IC.int);  -- gdnative_api_struct.gen.h:135
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_net_3_2_api_struct);  -- gdnative_api_struct.gen.h:129

   -------------
   -- Net API --
   -------------
   type godot_gdnative_ext_net_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:139
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:140
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:141
      godot_net_bind_stream_peer : access procedure (arg1 : System.Address; arg2 : access IC.int);  -- gdnative_api_struct.gen.h:142
      godot_net_bind_packet_peer : access procedure (arg1 : System.Address; arg2 : access IC.int);  -- gdnative_api_struct.gen.h:143
      godot_net_bind_multiplayer_peer : access procedure (arg1 : System.Address; arg2 : access IC.int);  -- gdnative_api_struct.gen.h:144
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_ext_net_api_struct);  -- gdnative_api_struct.gen.h:138

   type godot_gdnative_ext_net_api_struct_ptr is access constant godot_gdnative_ext_net_api_struct with Convention => C;
   function To_Api_Struct_Ptr is new Ada.Unchecked_Conversion (godot_gdnative_api_struct_ptr, godot_gdnative_ext_net_api_struct_ptr);


   type godot_gdnative_core_1_2_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:148
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:149
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:150
      godot_dictionary_duplicate : access function (arg1 : access constant godot_dictionary; arg2 : godot_bool) return godot_dictionary;  -- gdnative_api_struct.gen.h:151
      godot_vector3_move_toward : access function
           (arg1 : access constant godot_vector3;
            arg2 : access constant godot_vector3;
            arg3 : godot_real) return godot_vector3;  -- gdnative_api_struct.gen.h:152
      godot_vector2_move_toward : access function
           (arg1 : access constant godot_vector2;
            arg2 : access constant godot_vector2;
            arg3 : godot_real) return godot_vector2;  -- gdnative_api_struct.gen.h:153
      godot_string_count : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_int;
            arg4 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:154
      godot_string_countn : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_int;
            arg4 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:155
      godot_vector3_direction_to : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:156
      godot_vector2_direction_to : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:157
      godot_array_slice : access function
           (arg1 : access constant godot_array;
            arg2 : godot_int;
            arg3 : godot_int;
            arg4 : godot_int;
            arg5 : godot_bool) return godot_array;  -- gdnative_api_struct.gen.h:158
      godot_pool_byte_array_empty : access function (arg1 : access constant godot_pool_byte_array) return godot_bool;  -- gdnative_api_struct.gen.h:159
      godot_pool_int_array_empty : access function (arg1 : access constant godot_pool_int_array) return godot_bool;  -- gdnative_api_struct.gen.h:160
      godot_pool_real_array_empty : access function (arg1 : access constant godot_pool_real_array) return godot_bool;  -- gdnative_api_struct.gen.h:161
      godot_pool_string_array_empty : access function (arg1 : access constant godot_pool_string_array) return godot_bool;  -- gdnative_api_struct.gen.h:162
      godot_pool_vector2_array_empty : access function (arg1 : access constant godot_pool_vector2_array) return godot_bool;  -- gdnative_api_struct.gen.h:163
      godot_pool_vector3_array_empty : access function (arg1 : access constant godot_pool_vector3_array) return godot_bool;  -- gdnative_api_struct.gen.h:164
      godot_pool_color_array_empty : access function (arg1 : access constant godot_pool_color_array) return godot_bool;  -- gdnative_api_struct.gen.h:165
      godot_get_class_tag : access function (arg1 : access constant godot_string_name) return System.Address;  -- gdnative_api_struct.gen.h:166
      godot_object_cast_to : access function (arg1 : System.Address; arg2 : System.Address) return System.Address;  -- gdnative_api_struct.gen.h:167
      godot_instance_from_id : access function (arg1 : godot_int) return System.Address;  -- gdnative_api_struct.gen.h:168
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_core_1_2_api_struct);  -- gdnative_api_struct.gen.h:147

   type godot_gdnative_core_1_1_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:172
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:173
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:174
      godot_color_to_abgr32 : access function (arg1 : access constant godot_color) return godot_int;  -- gdnative_api_struct.gen.h:175
      godot_color_to_abgr64 : access function (arg1 : access constant godot_color) return godot_int;  -- gdnative_api_struct.gen.h:176
      godot_color_to_argb64 : access function (arg1 : access constant godot_color) return godot_int;  -- gdnative_api_struct.gen.h:177
      godot_color_to_rgba64 : access function (arg1 : access constant godot_color) return godot_int;  -- gdnative_api_struct.gen.h:178
      godot_color_darkened : access function (arg1 : access constant godot_color; arg2 : godot_real) return godot_color;  -- gdnative_api_struct.gen.h:179
      godot_color_from_hsv : access function
           (arg1 : access constant godot_color;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real;
            arg5 : godot_real) return godot_color;  -- gdnative_api_struct.gen.h:180
      godot_color_lightened : access function (arg1 : access constant godot_color; arg2 : godot_real) return godot_color;  -- gdnative_api_struct.gen.h:181
      godot_array_duplicate : access function (arg1 : access constant godot_array; arg2 : godot_bool) return godot_array;  -- gdnative_api_struct.gen.h:182
      godot_array_max : access function (arg1 : access constant godot_array) return godot_variant;  -- gdnative_api_struct.gen.h:183
      godot_array_min : access function (arg1 : access constant godot_array) return godot_variant;  -- gdnative_api_struct.gen.h:184
      godot_array_shuffle : access procedure (arg1 : access godot_array);  -- gdnative_api_struct.gen.h:185
      godot_basis_slerp : access function
           (arg1 : access constant godot_basis;
            arg2 : access constant godot_basis;
            arg3 : godot_real) return godot_basis;  -- gdnative_api_struct.gen.h:186
      godot_dictionary_get_with_default : access function
           (arg1 : access constant godot_dictionary;
            arg2 : access constant godot_variant;
            arg3 : access constant godot_variant) return godot_variant;  -- gdnative_api_struct.gen.h:187
      godot_dictionary_erase_with_return : access function (arg1 : access godot_dictionary; arg2 : access constant godot_variant) return ICE.bool;  -- gdnative_api_struct.gen.h:188
      godot_node_path_get_as_property_path : access function (arg1 : access constant godot_node_path) return godot_node_path;  -- gdnative_api_struct.gen.h:189
      godot_quat_set_axis_angle : access procedure
           (arg1 : access godot_quat;
            arg2 : access constant godot_vector3;
            arg3 : godot_real);  -- gdnative_api_struct.gen.h:190
      godot_rect2_grow_individual : access function
           (arg1 : access constant godot_rect2;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real;
            arg5 : godot_real) return godot_rect2;  -- gdnative_api_struct.gen.h:191
      godot_rect2_grow_margin : access function
           (arg1 : access constant godot_rect2;
            arg2 : godot_int;
            arg3 : godot_real) return godot_rect2;  -- gdnative_api_struct.gen.h:192
      godot_rect2_abs : access function (arg1 : access constant godot_rect2) return godot_rect2;  -- gdnative_api_struct.gen.h:193
      godot_string_dedent : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:194
      godot_string_trim_prefix : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:195
      godot_string_trim_suffix : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:196
      godot_string_rstrip : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:197
      godot_string_rsplit : access function
           (arg1 : access constant godot_string;
            arg2 : access constant godot_string;
            arg3 : godot_bool;
            arg4 : godot_int) return godot_pool_string_array;  -- gdnative_api_struct.gen.h:198
      godot_basis_get_quat : access function (arg1 : access constant godot_basis) return godot_quat;  -- gdnative_api_struct.gen.h:199
      godot_basis_set_quat : access procedure (arg1 : access godot_basis; arg2 : access constant godot_quat);  -- gdnative_api_struct.gen.h:200
      godot_basis_set_axis_angle_scale : access procedure
           (arg1 : access godot_basis;
            arg2 : access constant godot_vector3;
            arg3 : godot_real;
            arg4 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:201
      godot_basis_set_euler_scale : access procedure
           (arg1 : access godot_basis;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:202
      godot_basis_set_quat_scale : access procedure
           (arg1 : access godot_basis;
            arg2 : access constant godot_quat;
            arg3 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:203
      godot_is_instance_valid : access function (arg1 : System.Address) return ICE.bool;  -- gdnative_api_struct.gen.h:204
      godot_quat_new_with_basis : access procedure (arg1 : access godot_quat; arg2 : access constant godot_basis);  -- gdnative_api_struct.gen.h:205
      godot_quat_new_with_euler : access procedure (arg1 : access godot_quat; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:206
      godot_transform_new_with_quat : access procedure (arg1 : access godot_transform; arg2 : access constant godot_quat);  -- gdnative_api_struct.gen.h:207
      godot_variant_get_operator_name : access function (arg1 : godot_variant_operator) return godot_string;  -- gdnative_api_struct.gen.h:208
      godot_variant_evaluate : access procedure
           (arg1 : godot_variant_operator;
            arg2 : access constant godot_variant;
            arg3 : access constant godot_variant;
            arg4 : access godot_variant;
            arg5 : access godot_bool);  -- gdnative_api_struct.gen.h:209
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_core_1_1_api_struct);  -- gdnative_api_struct.gen.h:171

   type godot_gdnative_core_api_struct is record
      c_type : aliased GDNATIVE_API_TYPES;  -- gdnative_api_struct.gen.h:213
      version : aliased godot_gdnative_api_version;  -- gdnative_api_struct.gen.h:214
      next : access constant godot_gdnative_api_struct;  -- gdnative_api_struct.gen.h:215
      num_extensions : aliased IC.unsigned;  -- gdnative_api_struct.gen.h:216
      extensions : GDnative_Api_Struct_Pointers.Pointer;  -- gdnative_api_struct.gen.h:217
      godot_color_new_rgba : access procedure
           (arg1 : access godot_color;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real;
            arg5 : godot_real);  -- gdnative_api_struct.gen.h:218
      godot_color_new_rgb : access procedure
           (arg1 : access godot_color;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real);  -- gdnative_api_struct.gen.h:219
      godot_color_get_r : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:220
      godot_color_set_r : access procedure (arg1 : access godot_color; arg2 : godot_real);  -- gdnative_api_struct.gen.h:221
      godot_color_get_g : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:222
      godot_color_set_g : access procedure (arg1 : access godot_color; arg2 : godot_real);  -- gdnative_api_struct.gen.h:223
      godot_color_get_b : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:224
      godot_color_set_b : access procedure (arg1 : access godot_color; arg2 : godot_real);  -- gdnative_api_struct.gen.h:225
      godot_color_get_a : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:226
      godot_color_set_a : access procedure (arg1 : access godot_color; arg2 : godot_real);  -- gdnative_api_struct.gen.h:227
      godot_color_get_h : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:228
      godot_color_get_s : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:229
      godot_color_get_v : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:230
      godot_color_as_string : access function (arg1 : access constant godot_color) return godot_string;  -- gdnative_api_struct.gen.h:231
      godot_color_to_rgba32 : access function (arg1 : access constant godot_color) return godot_int;  -- gdnative_api_struct.gen.h:232
      godot_color_to_argb32 : access function (arg1 : access constant godot_color) return godot_int;  -- gdnative_api_struct.gen.h:233
      godot_color_gray : access function (arg1 : access constant godot_color) return godot_real;  -- gdnative_api_struct.gen.h:234
      godot_color_inverted : access function (arg1 : access constant godot_color) return godot_color;  -- gdnative_api_struct.gen.h:235
      godot_color_contrasted : access function (arg1 : access constant godot_color) return godot_color;  -- gdnative_api_struct.gen.h:236
      godot_color_linear_interpolate : access function
           (arg1 : access constant godot_color;
            arg2 : access constant godot_color;
            arg3 : godot_real) return godot_color;  -- gdnative_api_struct.gen.h:237
      godot_color_blend : access function (arg1 : access constant godot_color; arg2 : access constant godot_color) return godot_color;  -- gdnative_api_struct.gen.h:238
      godot_color_to_html : access function (arg1 : access constant godot_color; arg2 : godot_bool) return godot_string;  -- gdnative_api_struct.gen.h:239
      godot_color_operator_equal : access function (arg1 : access constant godot_color; arg2 : access constant godot_color) return godot_bool;  -- gdnative_api_struct.gen.h:240
      godot_color_operator_less : access function (arg1 : access constant godot_color; arg2 : access constant godot_color) return godot_bool;  -- gdnative_api_struct.gen.h:241
      godot_vector2_new : access procedure
           (arg1 : access godot_vector2;
            arg2 : godot_real;
            arg3 : godot_real);  -- gdnative_api_struct.gen.h:242
      godot_vector2_as_string : access function (arg1 : access constant godot_vector2) return godot_string;  -- gdnative_api_struct.gen.h:243
      godot_vector2_normalized : access function (arg1 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:244
      godot_vector2_length : access function (arg1 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:245
      godot_vector2_angle : access function (arg1 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:246
      godot_vector2_length_squared : access function (arg1 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:247
      godot_vector2_is_normalized : access function (arg1 : access constant godot_vector2) return godot_bool;  -- gdnative_api_struct.gen.h:248
      godot_vector2_distance_to : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:249
      godot_vector2_distance_squared_to : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:250
      godot_vector2_angle_to : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:251
      godot_vector2_angle_to_point : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:252
      godot_vector2_linear_interpolate : access function
           (arg1 : access constant godot_vector2;
            arg2 : access constant godot_vector2;
            arg3 : godot_real) return godot_vector2;  -- gdnative_api_struct.gen.h:253
      godot_vector2_cubic_interpolate : access function
           (arg1 : access constant godot_vector2;
            arg2 : access constant godot_vector2;
            arg3 : access constant godot_vector2;
            arg4 : access constant godot_vector2;
            arg5 : godot_real) return godot_vector2;  -- gdnative_api_struct.gen.h:254
      godot_vector2_rotated : access function (arg1 : access constant godot_vector2; arg2 : godot_real) return godot_vector2;  -- gdnative_api_struct.gen.h:255
      godot_vector2_tangent : access function (arg1 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:256
      godot_vector2_floor : access function (arg1 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:257
      godot_vector2_snapped : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:258
      godot_vector2_aspect : access function (arg1 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:259
      godot_vector2_dot : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:260
      godot_vector2_slide : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:261
      godot_vector2_bounce : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:262
      godot_vector2_reflect : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:263
      godot_vector2_abs : access function (arg1 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:264
      godot_vector2_clamped : access function (arg1 : access constant godot_vector2; arg2 : godot_real) return godot_vector2;  -- gdnative_api_struct.gen.h:265
      godot_vector2_operator_add : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:266
      godot_vector2_operator_subtract : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:267
      godot_vector2_operator_multiply_vector : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:268
      godot_vector2_operator_multiply_scalar : access function (arg1 : access constant godot_vector2; arg2 : godot_real) return godot_vector2;  -- gdnative_api_struct.gen.h:269
      godot_vector2_operator_divide_vector : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:270
      godot_vector2_operator_divide_scalar : access function (arg1 : access constant godot_vector2; arg2 : godot_real) return godot_vector2;  -- gdnative_api_struct.gen.h:271
      godot_vector2_operator_equal : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_bool;  -- gdnative_api_struct.gen.h:272
      godot_vector2_operator_less : access function (arg1 : access constant godot_vector2; arg2 : access constant godot_vector2) return godot_bool;  -- gdnative_api_struct.gen.h:273
      godot_vector2_operator_neg : access function (arg1 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:274
      godot_vector2_set_x : access procedure (arg1 : access godot_vector2; arg2 : godot_real);  -- gdnative_api_struct.gen.h:275
      godot_vector2_set_y : access procedure (arg1 : access godot_vector2; arg2 : godot_real);  -- gdnative_api_struct.gen.h:276
      godot_vector2_get_x : access function (arg1 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:277
      godot_vector2_get_y : access function (arg1 : access constant godot_vector2) return godot_real;  -- gdnative_api_struct.gen.h:278
      godot_quat_new : access procedure
           (arg1 : access godot_quat;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real;
            arg5 : godot_real);  -- gdnative_api_struct.gen.h:279
      godot_quat_new_with_axis_angle : access procedure
           (arg1 : access godot_quat;
            arg2 : access constant godot_vector3;
            arg3 : godot_real);  -- gdnative_api_struct.gen.h:280
      godot_quat_get_x : access function (arg1 : access constant godot_quat) return godot_real;  -- gdnative_api_struct.gen.h:281
      godot_quat_set_x : access procedure (arg1 : access godot_quat; arg2 : godot_real);  -- gdnative_api_struct.gen.h:282
      godot_quat_get_y : access function (arg1 : access constant godot_quat) return godot_real;  -- gdnative_api_struct.gen.h:283
      godot_quat_set_y : access procedure (arg1 : access godot_quat; arg2 : godot_real);  -- gdnative_api_struct.gen.h:284
      godot_quat_get_z : access function (arg1 : access constant godot_quat) return godot_real;  -- gdnative_api_struct.gen.h:285
      godot_quat_set_z : access procedure (arg1 : access godot_quat; arg2 : godot_real);  -- gdnative_api_struct.gen.h:286
      godot_quat_get_w : access function (arg1 : access constant godot_quat) return godot_real;  -- gdnative_api_struct.gen.h:287
      godot_quat_set_w : access procedure (arg1 : access godot_quat; arg2 : godot_real);  -- gdnative_api_struct.gen.h:288
      godot_quat_as_string : access function (arg1 : access constant godot_quat) return godot_string;  -- gdnative_api_struct.gen.h:289
      godot_quat_length : access function (arg1 : access constant godot_quat) return godot_real;  -- gdnative_api_struct.gen.h:290
      godot_quat_length_squared : access function (arg1 : access constant godot_quat) return godot_real;  -- gdnative_api_struct.gen.h:291
      godot_quat_normalized : access function (arg1 : access constant godot_quat) return godot_quat;  -- gdnative_api_struct.gen.h:292
      godot_quat_is_normalized : access function (arg1 : access constant godot_quat) return godot_bool;  -- gdnative_api_struct.gen.h:293
      godot_quat_inverse : access function (arg1 : access constant godot_quat) return godot_quat;  -- gdnative_api_struct.gen.h:294
      godot_quat_dot : access function (arg1 : access constant godot_quat; arg2 : access constant godot_quat) return godot_real;  -- gdnative_api_struct.gen.h:295
      godot_quat_xform : access function (arg1 : access constant godot_quat; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:296
      godot_quat_slerp : access function
           (arg1 : access constant godot_quat;
            arg2 : access constant godot_quat;
            arg3 : godot_real) return godot_quat;  -- gdnative_api_struct.gen.h:297
      godot_quat_slerpni : access function
           (arg1 : access constant godot_quat;
            arg2 : access constant godot_quat;
            arg3 : godot_real) return godot_quat;  -- gdnative_api_struct.gen.h:298
      godot_quat_cubic_slerp : access function
           (arg1 : access constant godot_quat;
            arg2 : access constant godot_quat;
            arg3 : access constant godot_quat;
            arg4 : access constant godot_quat;
            arg5 : godot_real) return godot_quat;  -- gdnative_api_struct.gen.h:299
      godot_quat_operator_multiply : access function (arg1 : access constant godot_quat; arg2 : godot_real) return godot_quat;  -- gdnative_api_struct.gen.h:300
      godot_quat_operator_add : access function (arg1 : access constant godot_quat; arg2 : access constant godot_quat) return godot_quat;  -- gdnative_api_struct.gen.h:301
      godot_quat_operator_subtract : access function (arg1 : access constant godot_quat; arg2 : access constant godot_quat) return godot_quat;  -- gdnative_api_struct.gen.h:302
      godot_quat_operator_divide : access function (arg1 : access constant godot_quat; arg2 : godot_real) return godot_quat;  -- gdnative_api_struct.gen.h:303
      godot_quat_operator_equal : access function (arg1 : access constant godot_quat; arg2 : access constant godot_quat) return godot_bool;  -- gdnative_api_struct.gen.h:304
      godot_quat_operator_neg : access function (arg1 : access constant godot_quat) return godot_quat;  -- gdnative_api_struct.gen.h:305
      godot_basis_new_with_rows : access procedure
           (arg1 : access godot_basis;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3;
            arg4 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:306
      godot_basis_new_with_axis_and_angle : access procedure
           (arg1 : access godot_basis;
            arg2 : access constant godot_vector3;
            arg3 : godot_real);  -- gdnative_api_struct.gen.h:307
      godot_basis_new_with_euler : access procedure (arg1 : access godot_basis; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:308
      godot_basis_as_string : access function (arg1 : access constant godot_basis) return godot_string;  -- gdnative_api_struct.gen.h:309
      godot_basis_inverse : access function (arg1 : access constant godot_basis) return godot_basis;  -- gdnative_api_struct.gen.h:310
      godot_basis_transposed : access function (arg1 : access constant godot_basis) return godot_basis;  -- gdnative_api_struct.gen.h:311
      godot_basis_orthonormalized : access function (arg1 : access constant godot_basis) return godot_basis;  -- gdnative_api_struct.gen.h:312
      godot_basis_determinant : access function (arg1 : access constant godot_basis) return godot_real;  -- gdnative_api_struct.gen.h:313
      godot_basis_rotated : access function
           (arg1 : access constant godot_basis;
            arg2 : access constant godot_vector3;
            arg3 : godot_real) return godot_basis;  -- gdnative_api_struct.gen.h:314
      godot_basis_scaled : access function (arg1 : access constant godot_basis; arg2 : access constant godot_vector3) return godot_basis;  -- gdnative_api_struct.gen.h:315
      godot_basis_get_scale : access function (arg1 : access constant godot_basis) return godot_vector3;  -- gdnative_api_struct.gen.h:316
      godot_basis_get_euler : access function (arg1 : access constant godot_basis) return godot_vector3;  -- gdnative_api_struct.gen.h:317
      godot_basis_tdotx : access function (arg1 : access constant godot_basis; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:318
      godot_basis_tdoty : access function (arg1 : access constant godot_basis; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:319
      godot_basis_tdotz : access function (arg1 : access constant godot_basis; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:320
      godot_basis_xform : access function (arg1 : access constant godot_basis; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:321
      godot_basis_xform_inv : access function (arg1 : access constant godot_basis; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:322
      godot_basis_get_orthogonal_index : access function (arg1 : access constant godot_basis) return godot_int;  -- gdnative_api_struct.gen.h:323
      godot_basis_new : access procedure (arg1 : access godot_basis);  -- gdnative_api_struct.gen.h:324
      godot_basis_new_with_euler_quat : access procedure (arg1 : access godot_basis; arg2 : access constant godot_quat);  -- gdnative_api_struct.gen.h:325
      godot_basis_get_elements : access procedure (arg1 : access constant godot_basis; arg2 : access godot_vector3);  -- gdnative_api_struct.gen.h:326
      godot_basis_get_axis : access function (arg1 : access constant godot_basis; arg2 : godot_int) return godot_vector3;  -- gdnative_api_struct.gen.h:327
      godot_basis_set_axis : access procedure
           (arg1 : access godot_basis;
            arg2 : godot_int;
            arg3 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:328
      godot_basis_get_row : access function (arg1 : access constant godot_basis; arg2 : godot_int) return godot_vector3;  -- gdnative_api_struct.gen.h:329
      godot_basis_set_row : access procedure
           (arg1 : access godot_basis;
            arg2 : godot_int;
            arg3 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:330
      godot_basis_operator_equal : access function (arg1 : access constant godot_basis; arg2 : access constant godot_basis) return godot_bool;  -- gdnative_api_struct.gen.h:331
      godot_basis_operator_add : access function (arg1 : access constant godot_basis; arg2 : access constant godot_basis) return godot_basis;  -- gdnative_api_struct.gen.h:332
      godot_basis_operator_subtract : access function (arg1 : access constant godot_basis; arg2 : access constant godot_basis) return godot_basis;  -- gdnative_api_struct.gen.h:333
      godot_basis_operator_multiply_vector : access function (arg1 : access constant godot_basis; arg2 : access constant godot_basis) return godot_basis;  -- gdnative_api_struct.gen.h:334
      godot_basis_operator_multiply_scalar : access function (arg1 : access constant godot_basis; arg2 : godot_real) return godot_basis;  -- gdnative_api_struct.gen.h:335
      godot_vector3_new : access procedure
           (arg1 : access godot_vector3;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real);  -- gdnative_api_struct.gen.h:336
      godot_vector3_as_string : access function (arg1 : access constant godot_vector3) return godot_string;  -- gdnative_api_struct.gen.h:337
      godot_vector3_min_axis : access function (arg1 : access constant godot_vector3) return godot_int;  -- gdnative_api_struct.gen.h:338
      godot_vector3_max_axis : access function (arg1 : access constant godot_vector3) return godot_int;  -- gdnative_api_struct.gen.h:339
      godot_vector3_length : access function (arg1 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:340
      godot_vector3_length_squared : access function (arg1 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:341
      godot_vector3_is_normalized : access function (arg1 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:342
      godot_vector3_normalized : access function (arg1 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:343
      godot_vector3_inverse : access function (arg1 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:344
      godot_vector3_snapped : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:345
      godot_vector3_rotated : access function
           (arg1 : access constant godot_vector3;
            arg2 : access constant godot_vector3;
            arg3 : godot_real) return godot_vector3;  -- gdnative_api_struct.gen.h:346
      godot_vector3_linear_interpolate : access function
           (arg1 : access constant godot_vector3;
            arg2 : access constant godot_vector3;
            arg3 : godot_real) return godot_vector3;  -- gdnative_api_struct.gen.h:347
      godot_vector3_cubic_interpolate : access function
           (arg1 : access constant godot_vector3;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3;
            arg4 : access constant godot_vector3;
            arg5 : godot_real) return godot_vector3;  -- gdnative_api_struct.gen.h:348
      godot_vector3_dot : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:349
      godot_vector3_cross : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:350
      godot_vector3_outer : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_basis;  -- gdnative_api_struct.gen.h:351
      godot_vector3_to_diagonal_matrix : access function (arg1 : access constant godot_vector3) return godot_basis;  -- gdnative_api_struct.gen.h:352
      godot_vector3_abs : access function (arg1 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:353
      godot_vector3_floor : access function (arg1 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:354
      godot_vector3_ceil : access function (arg1 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:355
      godot_vector3_distance_to : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:356
      godot_vector3_distance_squared_to : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:357
      godot_vector3_angle_to : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:358
      godot_vector3_slide : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:359
      godot_vector3_bounce : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:360
      godot_vector3_reflect : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:361
      godot_vector3_operator_add : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:362
      godot_vector3_operator_subtract : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:363
      godot_vector3_operator_multiply_vector : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:364
      godot_vector3_operator_multiply_scalar : access function (arg1 : access constant godot_vector3; arg2 : godot_real) return godot_vector3;  -- gdnative_api_struct.gen.h:365
      godot_vector3_operator_divide_vector : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:366
      godot_vector3_operator_divide_scalar : access function (arg1 : access constant godot_vector3; arg2 : godot_real) return godot_vector3;  -- gdnative_api_struct.gen.h:367
      godot_vector3_operator_equal : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:368
      godot_vector3_operator_less : access function (arg1 : access constant godot_vector3; arg2 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:369
      godot_vector3_operator_neg : access function (arg1 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:370
      godot_vector3_set_axis : access procedure
           (arg1 : access godot_vector3;
            arg2 : godot_vector3_axis;
            arg3 : godot_real);  -- gdnative_api_struct.gen.h:371
      godot_vector3_get_axis : access function (arg1 : access constant godot_vector3; arg2 : godot_vector3_axis) return godot_real;  -- gdnative_api_struct.gen.h:372
      godot_pool_byte_array_new : access procedure (arg1 : access godot_pool_byte_array);  -- gdnative_api_struct.gen.h:373
      godot_pool_byte_array_new_copy : access procedure (arg1 : access godot_pool_byte_array; arg2 : access constant godot_pool_byte_array);  -- gdnative_api_struct.gen.h:374
      godot_pool_byte_array_new_with_array : access procedure (arg1 : access godot_pool_byte_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:375
      godot_pool_byte_array_append : access procedure (arg1 : access godot_pool_byte_array; arg2 : IC.unsigned_char);  -- gdnative_api_struct.gen.h:376
      godot_pool_byte_array_append_array : access procedure (arg1 : access godot_pool_byte_array; arg2 : access constant godot_pool_byte_array);  -- gdnative_api_struct.gen.h:377
      godot_pool_byte_array_insert : access function
           (arg1 : access godot_pool_byte_array;
            arg2 : godot_int;
            arg3 : IC.unsigned_char) return godot_error;  -- gdnative_api_struct.gen.h:378
      godot_pool_byte_array_invert : access procedure (arg1 : access godot_pool_byte_array);  -- gdnative_api_struct.gen.h:379
      godot_pool_byte_array_push_back : access procedure (arg1 : access godot_pool_byte_array; arg2 : IC.unsigned_char);  -- gdnative_api_struct.gen.h:380
      godot_pool_byte_array_remove : access procedure (arg1 : access godot_pool_byte_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:381
      godot_pool_byte_array_resize : access procedure (arg1 : access godot_pool_byte_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:382
      godot_pool_byte_array_read : access function (arg1 : access constant godot_pool_byte_array) return access godot_pool_byte_array_read_access;  -- gdnative_api_struct.gen.h:383
      godot_pool_byte_array_write : access function (arg1 : access godot_pool_byte_array) return access godot_pool_byte_array_write_access;  -- gdnative_api_struct.gen.h:384
      godot_pool_byte_array_set : access procedure
           (arg1 : access godot_pool_byte_array;
            arg2 : godot_int;
            arg3 : IC.unsigned_char);  -- gdnative_api_struct.gen.h:385
      godot_pool_byte_array_get : access function (arg1 : access constant godot_pool_byte_array; arg2 : godot_int) return IC.unsigned_char;  -- gdnative_api_struct.gen.h:386
      godot_pool_byte_array_size : access function (arg1 : access constant godot_pool_byte_array) return godot_int;  -- gdnative_api_struct.gen.h:387
      godot_pool_byte_array_destroy : access procedure (arg1 : access godot_pool_byte_array);  -- gdnative_api_struct.gen.h:388
      godot_pool_int_array_new : access procedure (arg1 : access godot_pool_int_array);  -- gdnative_api_struct.gen.h:389
      godot_pool_int_array_new_copy : access procedure (arg1 : access godot_pool_int_array; arg2 : access constant godot_pool_int_array);  -- gdnative_api_struct.gen.h:390
      godot_pool_int_array_new_with_array : access procedure (arg1 : access godot_pool_int_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:391
      godot_pool_int_array_append : access procedure (arg1 : access godot_pool_int_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:392
      godot_pool_int_array_append_array : access procedure (arg1 : access godot_pool_int_array; arg2 : access constant godot_pool_int_array);  -- gdnative_api_struct.gen.h:393
      godot_pool_int_array_insert : access function
           (arg1 : access godot_pool_int_array;
            arg2 : godot_int;
            arg3 : godot_int) return godot_error;  -- gdnative_api_struct.gen.h:394
      godot_pool_int_array_invert : access procedure (arg1 : access godot_pool_int_array);  -- gdnative_api_struct.gen.h:395
      godot_pool_int_array_push_back : access procedure (arg1 : access godot_pool_int_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:396
      godot_pool_int_array_remove : access procedure (arg1 : access godot_pool_int_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:397
      godot_pool_int_array_resize : access procedure (arg1 : access godot_pool_int_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:398
      godot_pool_int_array_read : access function (arg1 : access constant godot_pool_int_array) return access godot_pool_int_array_read_access;  -- gdnative_api_struct.gen.h:399
      godot_pool_int_array_write : access function (arg1 : access godot_pool_int_array) return access godot_pool_int_array_write_access;  -- gdnative_api_struct.gen.h:400
      godot_pool_int_array_set : access procedure
           (arg1 : access godot_pool_int_array;
            arg2 : godot_int;
            arg3 : godot_int);  -- gdnative_api_struct.gen.h:401
      godot_pool_int_array_get : access function (arg1 : access constant godot_pool_int_array; arg2 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:402
      godot_pool_int_array_size : access function (arg1 : access constant godot_pool_int_array) return godot_int;  -- gdnative_api_struct.gen.h:403
      godot_pool_int_array_destroy : access procedure (arg1 : access godot_pool_int_array);  -- gdnative_api_struct.gen.h:404
      godot_pool_real_array_new : access procedure (arg1 : access godot_pool_real_array);  -- gdnative_api_struct.gen.h:405
      godot_pool_real_array_new_copy : access procedure (arg1 : access godot_pool_real_array; arg2 : access constant godot_pool_real_array);  -- gdnative_api_struct.gen.h:406
      godot_pool_real_array_new_with_array : access procedure (arg1 : access godot_pool_real_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:407
      godot_pool_real_array_append : access procedure (arg1 : access godot_pool_real_array; arg2 : godot_real);  -- gdnative_api_struct.gen.h:408
      godot_pool_real_array_append_array : access procedure (arg1 : access godot_pool_real_array; arg2 : access constant godot_pool_real_array);  -- gdnative_api_struct.gen.h:409
      godot_pool_real_array_insert : access function
           (arg1 : access godot_pool_real_array;
            arg2 : godot_int;
            arg3 : godot_real) return godot_error;  -- gdnative_api_struct.gen.h:410
      godot_pool_real_array_invert : access procedure (arg1 : access godot_pool_real_array);  -- gdnative_api_struct.gen.h:411
      godot_pool_real_array_push_back : access procedure (arg1 : access godot_pool_real_array; arg2 : godot_real);  -- gdnative_api_struct.gen.h:412
      godot_pool_real_array_remove : access procedure (arg1 : access godot_pool_real_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:413
      godot_pool_real_array_resize : access procedure (arg1 : access godot_pool_real_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:414
      godot_pool_real_array_read : access function (arg1 : access constant godot_pool_real_array) return access godot_pool_real_array_read_access;  -- gdnative_api_struct.gen.h:415
      godot_pool_real_array_write : access function (arg1 : access godot_pool_real_array) return access godot_pool_real_array_write_access;  -- gdnative_api_struct.gen.h:416
      godot_pool_real_array_set : access procedure
           (arg1 : access godot_pool_real_array;
            arg2 : godot_int;
            arg3 : godot_real);  -- gdnative_api_struct.gen.h:417
      godot_pool_real_array_get : access function (arg1 : access constant godot_pool_real_array; arg2 : godot_int) return godot_real;  -- gdnative_api_struct.gen.h:418
      godot_pool_real_array_size : access function (arg1 : access constant godot_pool_real_array) return godot_int;  -- gdnative_api_struct.gen.h:419
      godot_pool_real_array_destroy : access procedure (arg1 : access godot_pool_real_array);  -- gdnative_api_struct.gen.h:420
      godot_pool_string_array_new : access procedure (arg1 : access godot_pool_string_array);  -- gdnative_api_struct.gen.h:421
      godot_pool_string_array_new_copy : access procedure (arg1 : access godot_pool_string_array; arg2 : access constant godot_pool_string_array);  -- gdnative_api_struct.gen.h:422
      godot_pool_string_array_new_with_array : access procedure (arg1 : access godot_pool_string_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:423
      godot_pool_string_array_append : access procedure (arg1 : access godot_pool_string_array; arg2 : access constant godot_string);  -- gdnative_api_struct.gen.h:424
      godot_pool_string_array_append_array : access procedure (arg1 : access godot_pool_string_array; arg2 : access constant godot_pool_string_array);  -- gdnative_api_struct.gen.h:425
      godot_pool_string_array_insert : access function
           (arg1 : access godot_pool_string_array;
            arg2 : godot_int;
            arg3 : access constant godot_string) return godot_error;  -- gdnative_api_struct.gen.h:426
      godot_pool_string_array_invert : access procedure (arg1 : access godot_pool_string_array);  -- gdnative_api_struct.gen.h:427
      godot_pool_string_array_push_back : access procedure (arg1 : access godot_pool_string_array; arg2 : access constant godot_string);  -- gdnative_api_struct.gen.h:428
      godot_pool_string_array_remove : access procedure (arg1 : access godot_pool_string_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:429
      godot_pool_string_array_resize : access procedure (arg1 : access godot_pool_string_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:430
      godot_pool_string_array_read : access function (arg1 : access constant godot_pool_string_array) return access godot_pool_string_array_read_access;  -- gdnative_api_struct.gen.h:431
      godot_pool_string_array_write : access function (arg1 : access godot_pool_string_array) return access godot_pool_string_array_write_access;  -- gdnative_api_struct.gen.h:432
      godot_pool_string_array_set : access procedure
           (arg1 : access godot_pool_string_array;
            arg2 : godot_int;
            arg3 : access constant godot_string);  -- gdnative_api_struct.gen.h:433
      godot_pool_string_array_get : access function (arg1 : access constant godot_pool_string_array; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:434
      godot_pool_string_array_size : access function (arg1 : access constant godot_pool_string_array) return godot_int;  -- gdnative_api_struct.gen.h:435
      godot_pool_string_array_destroy : access procedure (arg1 : access godot_pool_string_array);  -- gdnative_api_struct.gen.h:436
      godot_pool_vector2_array_new : access procedure (arg1 : access godot_pool_vector2_array);  -- gdnative_api_struct.gen.h:437
      godot_pool_vector2_array_new_copy : access procedure (arg1 : access godot_pool_vector2_array; arg2 : access constant godot_pool_vector2_array);  -- gdnative_api_struct.gen.h:438
      godot_pool_vector2_array_new_with_array : access procedure (arg1 : access godot_pool_vector2_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:439
      godot_pool_vector2_array_append : access procedure (arg1 : access godot_pool_vector2_array; arg2 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:440
      godot_pool_vector2_array_append_array : access procedure (arg1 : access godot_pool_vector2_array; arg2 : access constant godot_pool_vector2_array);  -- gdnative_api_struct.gen.h:441
      godot_pool_vector2_array_insert : access function
           (arg1 : access godot_pool_vector2_array;
            arg2 : godot_int;
            arg3 : access constant godot_vector2) return godot_error;  -- gdnative_api_struct.gen.h:442
      godot_pool_vector2_array_invert : access procedure (arg1 : access godot_pool_vector2_array);  -- gdnative_api_struct.gen.h:443
      godot_pool_vector2_array_push_back : access procedure (arg1 : access godot_pool_vector2_array; arg2 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:444
      godot_pool_vector2_array_remove : access procedure (arg1 : access godot_pool_vector2_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:445
      godot_pool_vector2_array_resize : access procedure (arg1 : access godot_pool_vector2_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:446
      godot_pool_vector2_array_read : access function (arg1 : access constant godot_pool_vector2_array) return access godot_pool_vector2_array_read_access;  -- gdnative_api_struct.gen.h:447
      godot_pool_vector2_array_write : access function (arg1 : access godot_pool_vector2_array) return access godot_pool_vector2_array_write_access;  -- gdnative_api_struct.gen.h:448
      godot_pool_vector2_array_set : access procedure
           (arg1 : access godot_pool_vector2_array;
            arg2 : godot_int;
            arg3 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:449
      godot_pool_vector2_array_get : access function (arg1 : access constant godot_pool_vector2_array; arg2 : godot_int) return godot_vector2;  -- gdnative_api_struct.gen.h:450
      godot_pool_vector2_array_size : access function (arg1 : access constant godot_pool_vector2_array) return godot_int;  -- gdnative_api_struct.gen.h:451
      godot_pool_vector2_array_destroy : access procedure (arg1 : access godot_pool_vector2_array);  -- gdnative_api_struct.gen.h:452
      godot_pool_vector3_array_new : access procedure (arg1 : access godot_pool_vector3_array);  -- gdnative_api_struct.gen.h:453
      godot_pool_vector3_array_new_copy : access procedure (arg1 : access godot_pool_vector3_array; arg2 : access constant godot_pool_vector3_array);  -- gdnative_api_struct.gen.h:454
      godot_pool_vector3_array_new_with_array : access procedure (arg1 : access godot_pool_vector3_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:455
      godot_pool_vector3_array_append : access procedure (arg1 : access godot_pool_vector3_array; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:456
      godot_pool_vector3_array_append_array : access procedure (arg1 : access godot_pool_vector3_array; arg2 : access constant godot_pool_vector3_array);  -- gdnative_api_struct.gen.h:457
      godot_pool_vector3_array_insert : access function
           (arg1 : access godot_pool_vector3_array;
            arg2 : godot_int;
            arg3 : access constant godot_vector3) return godot_error;  -- gdnative_api_struct.gen.h:458
      godot_pool_vector3_array_invert : access procedure (arg1 : access godot_pool_vector3_array);  -- gdnative_api_struct.gen.h:459
      godot_pool_vector3_array_push_back : access procedure (arg1 : access godot_pool_vector3_array; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:460
      godot_pool_vector3_array_remove : access procedure (arg1 : access godot_pool_vector3_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:461
      godot_pool_vector3_array_resize : access procedure (arg1 : access godot_pool_vector3_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:462
      godot_pool_vector3_array_read : access function (arg1 : access constant godot_pool_vector3_array) return access godot_pool_vector3_array_read_access;  -- gdnative_api_struct.gen.h:463
      godot_pool_vector3_array_write : access function (arg1 : access godot_pool_vector3_array) return access godot_pool_vector3_array_write_access;  -- gdnative_api_struct.gen.h:464
      godot_pool_vector3_array_set : access procedure
           (arg1 : access godot_pool_vector3_array;
            arg2 : godot_int;
            arg3 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:465
      godot_pool_vector3_array_get : access function (arg1 : access constant godot_pool_vector3_array; arg2 : godot_int) return godot_vector3;  -- gdnative_api_struct.gen.h:466
      godot_pool_vector3_array_size : access function (arg1 : access constant godot_pool_vector3_array) return godot_int;  -- gdnative_api_struct.gen.h:467
      godot_pool_vector3_array_destroy : access procedure (arg1 : access godot_pool_vector3_array);  -- gdnative_api_struct.gen.h:468
      godot_pool_color_array_new : access procedure (arg1 : access godot_pool_color_array);  -- gdnative_api_struct.gen.h:469
      godot_pool_color_array_new_copy : access procedure (arg1 : access godot_pool_color_array; arg2 : access constant godot_pool_color_array);  -- gdnative_api_struct.gen.h:470
      godot_pool_color_array_new_with_array : access procedure (arg1 : access godot_pool_color_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:471
      godot_pool_color_array_append : access procedure (arg1 : access godot_pool_color_array; arg2 : access constant godot_color);  -- gdnative_api_struct.gen.h:472
      godot_pool_color_array_append_array : access procedure (arg1 : access godot_pool_color_array; arg2 : access constant godot_pool_color_array);  -- gdnative_api_struct.gen.h:473
      godot_pool_color_array_insert : access function
           (arg1 : access godot_pool_color_array;
            arg2 : godot_int;
            arg3 : access constant godot_color) return godot_error;  -- gdnative_api_struct.gen.h:474
      godot_pool_color_array_invert : access procedure (arg1 : access godot_pool_color_array);  -- gdnative_api_struct.gen.h:475
      godot_pool_color_array_push_back : access procedure (arg1 : access godot_pool_color_array; arg2 : access constant godot_color);  -- gdnative_api_struct.gen.h:476
      godot_pool_color_array_remove : access procedure (arg1 : access godot_pool_color_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:477
      godot_pool_color_array_resize : access procedure (arg1 : access godot_pool_color_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:478
      godot_pool_color_array_read : access function (arg1 : access constant godot_pool_color_array) return access godot_pool_color_array_read_access;  -- gdnative_api_struct.gen.h:479
      godot_pool_color_array_write : access function (arg1 : access godot_pool_color_array) return access godot_pool_color_array_write_access;  -- gdnative_api_struct.gen.h:480
      godot_pool_color_array_set : access procedure
           (arg1 : access godot_pool_color_array;
            arg2 : godot_int;
            arg3 : access constant godot_color);  -- gdnative_api_struct.gen.h:481
      godot_pool_color_array_get : access function (arg1 : access constant godot_pool_color_array; arg2 : godot_int) return godot_color;  -- gdnative_api_struct.gen.h:482
      godot_pool_color_array_size : access function (arg1 : access constant godot_pool_color_array) return godot_int;  -- gdnative_api_struct.gen.h:483
      godot_pool_color_array_destroy : access procedure (arg1 : access godot_pool_color_array);  -- gdnative_api_struct.gen.h:484
      godot_pool_byte_array_read_access_copy : access function (arg1 : access constant godot_pool_byte_array_read_access) return access godot_pool_byte_array_read_access;  -- gdnative_api_struct.gen.h:485
      godot_pool_byte_array_read_access_ptr : access function (arg1 : access constant godot_pool_byte_array_read_access) return access IC.unsigned_char;  -- gdnative_api_struct.gen.h:486
      godot_pool_byte_array_read_access_operator_assign : access procedure (arg1 : access godot_pool_byte_array_read_access; arg2 : access godot_pool_byte_array_read_access);  -- gdnative_api_struct.gen.h:487
      godot_pool_byte_array_read_access_destroy : access procedure (arg1 : access godot_pool_byte_array_read_access);  -- gdnative_api_struct.gen.h:488
      godot_pool_int_array_read_access_copy : access function (arg1 : access constant godot_pool_int_array_read_access) return access godot_pool_int_array_read_access;  -- gdnative_api_struct.gen.h:489
      godot_pool_int_array_read_access_ptr : access function (arg1 : access constant godot_pool_int_array_read_access) return access godot_int;  -- gdnative_api_struct.gen.h:490
      godot_pool_int_array_read_access_operator_assign : access procedure (arg1 : access godot_pool_int_array_read_access; arg2 : access godot_pool_int_array_read_access);  -- gdnative_api_struct.gen.h:491
      godot_pool_int_array_read_access_destroy : access procedure (arg1 : access godot_pool_int_array_read_access);  -- gdnative_api_struct.gen.h:492
      godot_pool_real_array_read_access_copy : access function (arg1 : access constant godot_pool_real_array_read_access) return access godot_pool_real_array_read_access;  -- gdnative_api_struct.gen.h:493
      godot_pool_real_array_read_access_ptr : access function (arg1 : access constant godot_pool_real_array_read_access) return access godot_real;  -- gdnative_api_struct.gen.h:494
      godot_pool_real_array_read_access_operator_assign : access procedure (arg1 : access godot_pool_real_array_read_access; arg2 : access godot_pool_real_array_read_access);  -- gdnative_api_struct.gen.h:495
      godot_pool_real_array_read_access_destroy : access procedure (arg1 : access godot_pool_real_array_read_access);  -- gdnative_api_struct.gen.h:496
      godot_pool_string_array_read_access_copy : access function (arg1 : access constant godot_pool_string_array_read_access) return access godot_pool_string_array_read_access;  -- gdnative_api_struct.gen.h:497
      godot_pool_string_array_read_access_ptr : access function (arg1 : access constant godot_pool_string_array_read_access) return access constant godot_string;  -- gdnative_api_struct.gen.h:498
      godot_pool_string_array_read_access_operator_assign : access procedure (arg1 : access godot_pool_string_array_read_access; arg2 : access godot_pool_string_array_read_access);  -- gdnative_api_struct.gen.h:499
      godot_pool_string_array_read_access_destroy : access procedure (arg1 : access godot_pool_string_array_read_access);  -- gdnative_api_struct.gen.h:500
      godot_pool_vector2_array_read_access_copy : access function (arg1 : access constant godot_pool_vector2_array_read_access) return access godot_pool_vector2_array_read_access;  -- gdnative_api_struct.gen.h:501
      godot_pool_vector2_array_read_access_ptr : access function (arg1 : access constant godot_pool_vector2_array_read_access) return access constant godot_vector2;  -- gdnative_api_struct.gen.h:502
      godot_pool_vector2_array_read_access_operator_assign : access procedure (arg1 : access godot_pool_vector2_array_read_access; arg2 : access godot_pool_vector2_array_read_access);  -- gdnative_api_struct.gen.h:503
      godot_pool_vector2_array_read_access_destroy : access procedure (arg1 : access godot_pool_vector2_array_read_access);  -- gdnative_api_struct.gen.h:504
      godot_pool_vector3_array_read_access_copy : access function (arg1 : access constant godot_pool_vector3_array_read_access) return access godot_pool_vector3_array_read_access;  -- gdnative_api_struct.gen.h:505
      godot_pool_vector3_array_read_access_ptr : access function (arg1 : access constant godot_pool_vector3_array_read_access) return access constant godot_vector3;  -- gdnative_api_struct.gen.h:506
      godot_pool_vector3_array_read_access_operator_assign : access procedure (arg1 : access godot_pool_vector3_array_read_access; arg2 : access godot_pool_vector3_array_read_access);  -- gdnative_api_struct.gen.h:507
      godot_pool_vector3_array_read_access_destroy : access procedure (arg1 : access godot_pool_vector3_array_read_access);  -- gdnative_api_struct.gen.h:508
      godot_pool_color_array_read_access_copy : access function (arg1 : access constant godot_pool_color_array_read_access) return access godot_pool_color_array_read_access;  -- gdnative_api_struct.gen.h:509
      godot_pool_color_array_read_access_ptr : access function (arg1 : access constant godot_pool_color_array_read_access) return access constant godot_color;  -- gdnative_api_struct.gen.h:510
      godot_pool_color_array_read_access_operator_assign : access procedure (arg1 : access godot_pool_color_array_read_access; arg2 : access godot_pool_color_array_read_access);  -- gdnative_api_struct.gen.h:511
      godot_pool_color_array_read_access_destroy : access procedure (arg1 : access godot_pool_color_array_read_access);  -- gdnative_api_struct.gen.h:512
      godot_pool_byte_array_write_access_copy : access function (arg1 : access constant godot_pool_byte_array_write_access) return access godot_pool_byte_array_write_access;  -- gdnative_api_struct.gen.h:513
      godot_pool_byte_array_write_access_ptr : access function (arg1 : access constant godot_pool_byte_array_write_access) return access IC.unsigned_char;  -- gdnative_api_struct.gen.h:514
      godot_pool_byte_array_write_access_operator_assign : access procedure (arg1 : access godot_pool_byte_array_write_access; arg2 : access godot_pool_byte_array_write_access);  -- gdnative_api_struct.gen.h:515
      godot_pool_byte_array_write_access_destroy : access procedure (arg1 : access godot_pool_byte_array_write_access);  -- gdnative_api_struct.gen.h:516
      godot_pool_int_array_write_access_copy : access function (arg1 : access constant godot_pool_int_array_write_access) return access godot_pool_int_array_write_access;  -- gdnative_api_struct.gen.h:517
      godot_pool_int_array_write_access_ptr : access function (arg1 : access constant godot_pool_int_array_write_access) return access godot_int;  -- gdnative_api_struct.gen.h:518
      godot_pool_int_array_write_access_operator_assign : access procedure (arg1 : access godot_pool_int_array_write_access; arg2 : access godot_pool_int_array_write_access);  -- gdnative_api_struct.gen.h:519
      godot_pool_int_array_write_access_destroy : access procedure (arg1 : access godot_pool_int_array_write_access);  -- gdnative_api_struct.gen.h:520
      godot_pool_real_array_write_access_copy : access function (arg1 : access constant godot_pool_real_array_write_access) return access godot_pool_real_array_write_access;  -- gdnative_api_struct.gen.h:521
      godot_pool_real_array_write_access_ptr : access function (arg1 : access constant godot_pool_real_array_write_access) return access godot_real;  -- gdnative_api_struct.gen.h:522
      godot_pool_real_array_write_access_operator_assign : access procedure (arg1 : access godot_pool_real_array_write_access; arg2 : access godot_pool_real_array_write_access);  -- gdnative_api_struct.gen.h:523
      godot_pool_real_array_write_access_destroy : access procedure (arg1 : access godot_pool_real_array_write_access);  -- gdnative_api_struct.gen.h:524
      godot_pool_string_array_write_access_copy : access function (arg1 : access constant godot_pool_string_array_write_access) return access godot_pool_string_array_write_access;  -- gdnative_api_struct.gen.h:525
      godot_pool_string_array_write_access_ptr : access function (arg1 : access constant godot_pool_string_array_write_access) return access godot_string;  -- gdnative_api_struct.gen.h:526
      godot_pool_string_array_write_access_operator_assign : access procedure (arg1 : access godot_pool_string_array_write_access; arg2 : access godot_pool_string_array_write_access);  -- gdnative_api_struct.gen.h:527
      godot_pool_string_array_write_access_destroy : access procedure (arg1 : access godot_pool_string_array_write_access);  -- gdnative_api_struct.gen.h:528
      godot_pool_vector2_array_write_access_copy : access function (arg1 : access constant godot_pool_vector2_array_write_access) return access godot_pool_vector2_array_write_access;  -- gdnative_api_struct.gen.h:529
      godot_pool_vector2_array_write_access_ptr : access function (arg1 : access constant godot_pool_vector2_array_write_access) return access godot_vector2;  -- gdnative_api_struct.gen.h:530
      godot_pool_vector2_array_write_access_operator_assign : access procedure (arg1 : access godot_pool_vector2_array_write_access; arg2 : access godot_pool_vector2_array_write_access);  -- gdnative_api_struct.gen.h:531
      godot_pool_vector2_array_write_access_destroy : access procedure (arg1 : access godot_pool_vector2_array_write_access);  -- gdnative_api_struct.gen.h:532
      godot_pool_vector3_array_write_access_copy : access function (arg1 : access constant godot_pool_vector3_array_write_access) return access godot_pool_vector3_array_write_access;  -- gdnative_api_struct.gen.h:533
      godot_pool_vector3_array_write_access_ptr : access function (arg1 : access constant godot_pool_vector3_array_write_access) return access godot_vector3;  -- gdnative_api_struct.gen.h:534
      godot_pool_vector3_array_write_access_operator_assign : access procedure (arg1 : access godot_pool_vector3_array_write_access; arg2 : access godot_pool_vector3_array_write_access);  -- gdnative_api_struct.gen.h:535
      godot_pool_vector3_array_write_access_destroy : access procedure (arg1 : access godot_pool_vector3_array_write_access);  -- gdnative_api_struct.gen.h:536
      godot_pool_color_array_write_access_copy : access function (arg1 : access constant godot_pool_color_array_write_access) return access godot_pool_color_array_write_access;  -- gdnative_api_struct.gen.h:537
      godot_pool_color_array_write_access_ptr : access function (arg1 : access constant godot_pool_color_array_write_access) return access godot_color;  -- gdnative_api_struct.gen.h:538
      godot_pool_color_array_write_access_operator_assign : access procedure (arg1 : access godot_pool_color_array_write_access; arg2 : access godot_pool_color_array_write_access);  -- gdnative_api_struct.gen.h:539
      godot_pool_color_array_write_access_destroy : access procedure (arg1 : access godot_pool_color_array_write_access);  -- gdnative_api_struct.gen.h:540
      godot_array_new : access procedure (arg1 : access godot_array);  -- gdnative_api_struct.gen.h:541
      godot_array_new_copy : access procedure (arg1 : access godot_array; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:542
      godot_array_new_pool_color_array : access procedure (arg1 : access godot_array; arg2 : access constant godot_pool_color_array);  -- gdnative_api_struct.gen.h:543
      godot_array_new_pool_vector3_array : access procedure (arg1 : access godot_array; arg2 : access constant godot_pool_vector3_array);  -- gdnative_api_struct.gen.h:544
      godot_array_new_pool_vector2_array : access procedure (arg1 : access godot_array; arg2 : access constant godot_pool_vector2_array);  -- gdnative_api_struct.gen.h:545
      godot_array_new_pool_string_array : access procedure (arg1 : access godot_array; arg2 : access constant godot_pool_string_array);  -- gdnative_api_struct.gen.h:546
      godot_array_new_pool_real_array : access procedure (arg1 : access godot_array; arg2 : access constant godot_pool_real_array);  -- gdnative_api_struct.gen.h:547
      godot_array_new_pool_int_array : access procedure (arg1 : access godot_array; arg2 : access constant godot_pool_int_array);  -- gdnative_api_struct.gen.h:548
      godot_array_new_pool_byte_array : access procedure (arg1 : access godot_array; arg2 : access constant godot_pool_byte_array);  -- gdnative_api_struct.gen.h:549
      godot_array_set : access procedure
           (arg1 : access godot_array;
            arg2 : godot_int;
            arg3 : access constant godot_variant);  -- gdnative_api_struct.gen.h:550
      godot_array_get : access function (arg1 : access constant godot_array; arg2 : godot_int) return godot_variant;  -- gdnative_api_struct.gen.h:551
      godot_array_operator_index : access function (arg1 : access godot_array; arg2 : godot_int) return access godot_variant;  -- gdnative_api_struct.gen.h:552
      godot_array_operator_index_const : access function (arg1 : access constant godot_array; arg2 : godot_int) return access constant godot_variant;  -- gdnative_api_struct.gen.h:553
      godot_array_append : access procedure (arg1 : access godot_array; arg2 : access constant godot_variant);  -- gdnative_api_struct.gen.h:554
      godot_array_clear : access procedure (arg1 : access godot_array);  -- gdnative_api_struct.gen.h:555
      godot_array_count : access function (arg1 : access constant godot_array; arg2 : access constant godot_variant) return godot_int;  -- gdnative_api_struct.gen.h:556
      godot_array_empty : access function (arg1 : access constant godot_array) return godot_bool;  -- gdnative_api_struct.gen.h:557
      godot_array_erase : access procedure (arg1 : access godot_array; arg2 : access constant godot_variant);  -- gdnative_api_struct.gen.h:558
      godot_array_front : access function (arg1 : access constant godot_array) return godot_variant;  -- gdnative_api_struct.gen.h:559
      godot_array_back : access function (arg1 : access constant godot_array) return godot_variant;  -- gdnative_api_struct.gen.h:560
      godot_array_find : access function
           (arg1 : access constant godot_array;
            arg2 : access constant godot_variant;
            arg3 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:561
      godot_array_find_last : access function (arg1 : access constant godot_array; arg2 : access constant godot_variant) return godot_int;  -- gdnative_api_struct.gen.h:562
      godot_array_has : access function (arg1 : access constant godot_array; arg2 : access constant godot_variant) return godot_bool;  -- gdnative_api_struct.gen.h:563
      godot_array_hash : access function (arg1 : access constant godot_array) return godot_int;  -- gdnative_api_struct.gen.h:564
      godot_array_insert : access procedure
           (arg1 : access godot_array;
            arg2 : godot_int;
            arg3 : access constant godot_variant);  -- gdnative_api_struct.gen.h:565
      godot_array_invert : access procedure (arg1 : access godot_array);  -- gdnative_api_struct.gen.h:566
      godot_array_pop_back : access function (arg1 : access godot_array) return godot_variant;  -- gdnative_api_struct.gen.h:567
      godot_array_pop_front : access function (arg1 : access godot_array) return godot_variant;  -- gdnative_api_struct.gen.h:568
      godot_array_push_back : access procedure (arg1 : access godot_array; arg2 : access constant godot_variant);  -- gdnative_api_struct.gen.h:569
      godot_array_push_front : access procedure (arg1 : access godot_array; arg2 : access constant godot_variant);  -- gdnative_api_struct.gen.h:570
      godot_array_remove : access procedure (arg1 : access godot_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:571
      godot_array_resize : access procedure (arg1 : access godot_array; arg2 : godot_int);  -- gdnative_api_struct.gen.h:572
      godot_array_rfind : access function
           (arg1 : access constant godot_array;
            arg2 : access constant godot_variant;
            arg3 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:573
      godot_array_size : access function (arg1 : access constant godot_array) return godot_int;  -- gdnative_api_struct.gen.h:574
      godot_array_sort : access procedure (arg1 : access godot_array);  -- gdnative_api_struct.gen.h:575
      godot_array_sort_custom : access procedure
           (arg1 : access godot_array;
            arg2 : System.Address;
            arg3 : access constant godot_string);  -- gdnative_api_struct.gen.h:576
      godot_array_bsearch : access function
           (arg1 : access godot_array;
            arg2 : access constant godot_variant;
            arg3 : godot_bool) return godot_int;  -- gdnative_api_struct.gen.h:577
      godot_array_bsearch_custom : access function
           (arg1 : access godot_array;
            arg2 : access constant godot_variant;
            arg3 : System.Address;
            arg4 : access constant godot_string;
            arg5 : godot_bool) return godot_int;  -- gdnative_api_struct.gen.h:578
      godot_array_destroy : access procedure (arg1 : access godot_array);  -- gdnative_api_struct.gen.h:579
      godot_dictionary_new : access procedure (arg1 : access godot_dictionary);  -- gdnative_api_struct.gen.h:580
      godot_dictionary_new_copy : access procedure (arg1 : access godot_dictionary; arg2 : access constant godot_dictionary);  -- gdnative_api_struct.gen.h:581
      godot_dictionary_destroy : access procedure (arg1 : access godot_dictionary);  -- gdnative_api_struct.gen.h:582
      godot_dictionary_size : access function (arg1 : access constant godot_dictionary) return godot_int;  -- gdnative_api_struct.gen.h:583
      godot_dictionary_empty : access function (arg1 : access constant godot_dictionary) return godot_bool;  -- gdnative_api_struct.gen.h:584
      godot_dictionary_clear : access procedure (arg1 : access godot_dictionary);  -- gdnative_api_struct.gen.h:585
      godot_dictionary_has : access function (arg1 : access constant godot_dictionary; arg2 : access constant godot_variant) return godot_bool;  -- gdnative_api_struct.gen.h:586
      godot_dictionary_has_all : access function (arg1 : access constant godot_dictionary; arg2 : access constant godot_array) return godot_bool;  -- gdnative_api_struct.gen.h:587
      godot_dictionary_erase : access procedure (arg1 : access godot_dictionary; arg2 : access constant godot_variant);  -- gdnative_api_struct.gen.h:588
      godot_dictionary_hash : access function (arg1 : access constant godot_dictionary) return godot_int;  -- gdnative_api_struct.gen.h:589
      godot_dictionary_keys : access function (arg1 : access constant godot_dictionary) return godot_array;  -- gdnative_api_struct.gen.h:590
      godot_dictionary_values : access function (arg1 : access constant godot_dictionary) return godot_array;  -- gdnative_api_struct.gen.h:591
      godot_dictionary_get : access function (arg1 : access constant godot_dictionary; arg2 : access constant godot_variant) return godot_variant;  -- gdnative_api_struct.gen.h:592
      godot_dictionary_set : access procedure
           (arg1 : access godot_dictionary;
            arg2 : access constant godot_variant;
            arg3 : access constant godot_variant);  -- gdnative_api_struct.gen.h:593
      godot_dictionary_operator_index : access function (arg1 : access godot_dictionary; arg2 : access constant godot_variant) return access godot_variant;  -- gdnative_api_struct.gen.h:594
      godot_dictionary_operator_index_const : access function (arg1 : access constant godot_dictionary; arg2 : access constant godot_variant) return access constant godot_variant;  -- gdnative_api_struct.gen.h:595
      godot_dictionary_next : access function (arg1 : access constant godot_dictionary; arg2 : access constant godot_variant) return access godot_variant;  -- gdnative_api_struct.gen.h:596
      godot_dictionary_operator_equal : access function (arg1 : access constant godot_dictionary; arg2 : access constant godot_dictionary) return godot_bool;  -- gdnative_api_struct.gen.h:597
      godot_dictionary_to_json : access function (arg1 : access constant godot_dictionary) return godot_string;  -- gdnative_api_struct.gen.h:598
      godot_node_path_new : access procedure (arg1 : access godot_node_path; arg2 : access constant godot_string);  -- gdnative_api_struct.gen.h:599
      godot_node_path_new_copy : access procedure (arg1 : access godot_node_path; arg2 : access constant godot_node_path);  -- gdnative_api_struct.gen.h:600
      godot_node_path_destroy : access procedure (arg1 : access godot_node_path);  -- gdnative_api_struct.gen.h:601
      godot_node_path_as_string : access function (arg1 : access constant godot_node_path) return godot_string;  -- gdnative_api_struct.gen.h:602
      godot_node_path_is_absolute : access function (arg1 : access constant godot_node_path) return godot_bool;  -- gdnative_api_struct.gen.h:603
      godot_node_path_get_name_count : access function (arg1 : access constant godot_node_path) return godot_int;  -- gdnative_api_struct.gen.h:604
      godot_node_path_get_name : access function (arg1 : access constant godot_node_path; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:605
      godot_node_path_get_subname_count : access function (arg1 : access constant godot_node_path) return godot_int;  -- gdnative_api_struct.gen.h:606
      godot_node_path_get_subname : access function (arg1 : access constant godot_node_path; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:607
      godot_node_path_get_concatenated_subnames : access function (arg1 : access constant godot_node_path) return godot_string;  -- gdnative_api_struct.gen.h:608
      godot_node_path_is_empty : access function (arg1 : access constant godot_node_path) return godot_bool;  -- gdnative_api_struct.gen.h:609
      godot_node_path_operator_equal : access function (arg1 : access constant godot_node_path; arg2 : access constant godot_node_path) return godot_bool;  -- gdnative_api_struct.gen.h:610
      godot_plane_new_with_reals : access procedure
           (arg1 : access godot_plane;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real;
            arg5 : godot_real);  -- gdnative_api_struct.gen.h:611
      godot_plane_new_with_vectors : access procedure
           (arg1 : access godot_plane;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3;
            arg4 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:612
      godot_plane_new_with_normal : access procedure
           (arg1 : access godot_plane;
            arg2 : access constant godot_vector3;
            arg3 : godot_real);  -- gdnative_api_struct.gen.h:613
      godot_plane_as_string : access function (arg1 : access constant godot_plane) return godot_string;  -- gdnative_api_struct.gen.h:614
      godot_plane_normalized : access function (arg1 : access constant godot_plane) return godot_plane;  -- gdnative_api_struct.gen.h:615
      godot_plane_center : access function (arg1 : access constant godot_plane) return godot_vector3;  -- gdnative_api_struct.gen.h:616
      godot_plane_get_any_point : access function (arg1 : access constant godot_plane) return godot_vector3;  -- gdnative_api_struct.gen.h:617
      godot_plane_is_point_over : access function (arg1 : access constant godot_plane; arg2 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:618
      godot_plane_distance_to : access function (arg1 : access constant godot_plane; arg2 : access constant godot_vector3) return godot_real;  -- gdnative_api_struct.gen.h:619
      godot_plane_has_point : access function
           (arg1 : access constant godot_plane;
            arg2 : access constant godot_vector3;
            arg3 : godot_real) return godot_bool;  -- gdnative_api_struct.gen.h:620
      godot_plane_project : access function (arg1 : access constant godot_plane; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:621
      godot_plane_intersect_3 : access function
           (arg1 : access constant godot_plane;
            arg2 : access godot_vector3;
            arg3 : access constant godot_plane;
            arg4 : access constant godot_plane) return godot_bool;  -- gdnative_api_struct.gen.h:622
      godot_plane_intersects_ray : access function
           (arg1 : access constant godot_plane;
            arg2 : access godot_vector3;
            arg3 : access constant godot_vector3;
            arg4 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:623
      godot_plane_intersects_segment : access function
           (arg1 : access constant godot_plane;
            arg2 : access godot_vector3;
            arg3 : access constant godot_vector3;
            arg4 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:624
      godot_plane_operator_neg : access function (arg1 : access constant godot_plane) return godot_plane;  -- gdnative_api_struct.gen.h:625
      godot_plane_operator_equal : access function (arg1 : access constant godot_plane; arg2 : access constant godot_plane) return godot_bool;  -- gdnative_api_struct.gen.h:626
      godot_plane_set_normal : access procedure (arg1 : access godot_plane; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:627
      godot_plane_get_normal : access function (arg1 : access constant godot_plane) return godot_vector3;  -- gdnative_api_struct.gen.h:628
      godot_plane_get_d : access function (arg1 : access constant godot_plane) return godot_real;  -- gdnative_api_struct.gen.h:629
      godot_plane_set_d : access procedure (arg1 : access godot_plane; arg2 : godot_real);  -- gdnative_api_struct.gen.h:630
      godot_rect2_new_with_position_and_size : access procedure
           (arg1 : access godot_rect2;
            arg2 : access constant godot_vector2;
            arg3 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:631
      godot_rect2_new : access procedure
           (arg1 : access godot_rect2;
            arg2 : godot_real;
            arg3 : godot_real;
            arg4 : godot_real;
            arg5 : godot_real);  -- gdnative_api_struct.gen.h:632
      godot_rect2_as_string : access function (arg1 : access constant godot_rect2) return godot_string;  -- gdnative_api_struct.gen.h:633
      godot_rect2_get_area : access function (arg1 : access constant godot_rect2) return godot_real;  -- gdnative_api_struct.gen.h:634
      godot_rect2_intersects : access function (arg1 : access constant godot_rect2; arg2 : access constant godot_rect2) return godot_bool;  -- gdnative_api_struct.gen.h:635
      godot_rect2_encloses : access function (arg1 : access constant godot_rect2; arg2 : access constant godot_rect2) return godot_bool;  -- gdnative_api_struct.gen.h:636
      godot_rect2_has_no_area : access function (arg1 : access constant godot_rect2) return godot_bool;  -- gdnative_api_struct.gen.h:637
      godot_rect2_clip : access function (arg1 : access constant godot_rect2; arg2 : access constant godot_rect2) return godot_rect2;  -- gdnative_api_struct.gen.h:638
      godot_rect2_merge : access function (arg1 : access constant godot_rect2; arg2 : access constant godot_rect2) return godot_rect2;  -- gdnative_api_struct.gen.h:639
      godot_rect2_has_point : access function (arg1 : access constant godot_rect2; arg2 : access constant godot_vector2) return godot_bool;  -- gdnative_api_struct.gen.h:640
      godot_rect2_grow : access function (arg1 : access constant godot_rect2; arg2 : godot_real) return godot_rect2;  -- gdnative_api_struct.gen.h:641
      godot_rect2_expand : access function (arg1 : access constant godot_rect2; arg2 : access constant godot_vector2) return godot_rect2;  -- gdnative_api_struct.gen.h:642
      godot_rect2_operator_equal : access function (arg1 : access constant godot_rect2; arg2 : access constant godot_rect2) return godot_bool;  -- gdnative_api_struct.gen.h:643
      godot_rect2_get_position : access function (arg1 : access constant godot_rect2) return godot_vector2;  -- gdnative_api_struct.gen.h:644
      godot_rect2_get_size : access function (arg1 : access constant godot_rect2) return godot_vector2;  -- gdnative_api_struct.gen.h:645
      godot_rect2_set_position : access procedure (arg1 : access godot_rect2; arg2 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:646
      godot_rect2_set_size : access procedure (arg1 : access godot_rect2; arg2 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:647
      godot_aabb_new : access procedure
           (arg1 : access godot_aabb;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:648
      godot_aabb_get_position : access function (arg1 : access constant godot_aabb) return godot_vector3;  -- gdnative_api_struct.gen.h:649
      godot_aabb_set_position : access procedure (arg1 : access constant godot_aabb; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:650
      godot_aabb_get_size : access function (arg1 : access constant godot_aabb) return godot_vector3;  -- gdnative_api_struct.gen.h:651
      godot_aabb_set_size : access procedure (arg1 : access constant godot_aabb; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:652
      godot_aabb_as_string : access function (arg1 : access constant godot_aabb) return godot_string;  -- gdnative_api_struct.gen.h:653
      godot_aabb_get_area : access function (arg1 : access constant godot_aabb) return godot_real;  -- gdnative_api_struct.gen.h:654
      godot_aabb_has_no_area : access function (arg1 : access constant godot_aabb) return godot_bool;  -- gdnative_api_struct.gen.h:655
      godot_aabb_has_no_surface : access function (arg1 : access constant godot_aabb) return godot_bool;  -- gdnative_api_struct.gen.h:656
      godot_aabb_intersects : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_aabb) return godot_bool;  -- gdnative_api_struct.gen.h:657
      godot_aabb_encloses : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_aabb) return godot_bool;  -- gdnative_api_struct.gen.h:658
      godot_aabb_merge : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_aabb) return godot_aabb;  -- gdnative_api_struct.gen.h:659
      godot_aabb_intersection : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_aabb) return godot_aabb;  -- gdnative_api_struct.gen.h:660
      godot_aabb_intersects_plane : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_plane) return godot_bool;  -- gdnative_api_struct.gen.h:661
      godot_aabb_intersects_segment : access function
           (arg1 : access constant godot_aabb;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:662
      godot_aabb_has_point : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_vector3) return godot_bool;  -- gdnative_api_struct.gen.h:663
      godot_aabb_get_support : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:664
      godot_aabb_get_longest_axis : access function (arg1 : access constant godot_aabb) return godot_vector3;  -- gdnative_api_struct.gen.h:665
      godot_aabb_get_longest_axis_index : access function (arg1 : access constant godot_aabb) return godot_int;  -- gdnative_api_struct.gen.h:666
      godot_aabb_get_longest_axis_size : access function (arg1 : access constant godot_aabb) return godot_real;  -- gdnative_api_struct.gen.h:667
      godot_aabb_get_shortest_axis : access function (arg1 : access constant godot_aabb) return godot_vector3;  -- gdnative_api_struct.gen.h:668
      godot_aabb_get_shortest_axis_index : access function (arg1 : access constant godot_aabb) return godot_int;  -- gdnative_api_struct.gen.h:669
      godot_aabb_get_shortest_axis_size : access function (arg1 : access constant godot_aabb) return godot_real;  -- gdnative_api_struct.gen.h:670
      godot_aabb_expand : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_vector3) return godot_aabb;  -- gdnative_api_struct.gen.h:671
      godot_aabb_grow : access function (arg1 : access constant godot_aabb; arg2 : godot_real) return godot_aabb;  -- gdnative_api_struct.gen.h:672
      godot_aabb_get_endpoint : access function (arg1 : access constant godot_aabb; arg2 : godot_int) return godot_vector3;  -- gdnative_api_struct.gen.h:673
      godot_aabb_operator_equal : access function (arg1 : access constant godot_aabb; arg2 : access constant godot_aabb) return godot_bool;  -- gdnative_api_struct.gen.h:674
      godot_rid_new : access procedure (arg1 : access godot_rid);  -- gdnative_api_struct.gen.h:675
      godot_rid_get_id : access function (arg1 : access constant godot_rid) return godot_int;  -- gdnative_api_struct.gen.h:676
      godot_rid_new_with_resource : access procedure (arg1 : access godot_rid; arg2 : System.Address);  -- gdnative_api_struct.gen.h:677
      godot_rid_operator_equal : access function (arg1 : access constant godot_rid; arg2 : access constant godot_rid) return godot_bool;  -- gdnative_api_struct.gen.h:678
      godot_rid_operator_less : access function (arg1 : access constant godot_rid; arg2 : access constant godot_rid) return godot_bool;  -- gdnative_api_struct.gen.h:679
      godot_transform_new_with_axis_origin : access procedure
           (arg1 : access godot_transform;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3;
            arg4 : access constant godot_vector3;
            arg5 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:680
      godot_transform_new : access procedure
           (arg1 : access godot_transform;
            arg2 : access constant godot_basis;
            arg3 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:681
      godot_transform_get_basis : access function (arg1 : access constant godot_transform) return godot_basis;  -- gdnative_api_struct.gen.h:682
      godot_transform_set_basis : access procedure (arg1 : access godot_transform; arg2 : access constant godot_basis);  -- gdnative_api_struct.gen.h:683
      godot_transform_get_origin : access function (arg1 : access constant godot_transform) return godot_vector3;  -- gdnative_api_struct.gen.h:684
      godot_transform_set_origin : access procedure (arg1 : access godot_transform; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:685
      godot_transform_as_string : access function (arg1 : access constant godot_transform) return godot_string;  -- gdnative_api_struct.gen.h:686
      godot_transform_inverse : access function (arg1 : access constant godot_transform) return godot_transform;  -- gdnative_api_struct.gen.h:687
      godot_transform_affine_inverse : access function (arg1 : access constant godot_transform) return godot_transform;  -- gdnative_api_struct.gen.h:688
      godot_transform_orthonormalized : access function (arg1 : access constant godot_transform) return godot_transform;  -- gdnative_api_struct.gen.h:689
      godot_transform_rotated : access function
           (arg1 : access constant godot_transform;
            arg2 : access constant godot_vector3;
            arg3 : godot_real) return godot_transform;  -- gdnative_api_struct.gen.h:690
      godot_transform_scaled : access function (arg1 : access constant godot_transform; arg2 : access constant godot_vector3) return godot_transform;  -- gdnative_api_struct.gen.h:691
      godot_transform_translated : access function (arg1 : access constant godot_transform; arg2 : access constant godot_vector3) return godot_transform;  -- gdnative_api_struct.gen.h:692
      godot_transform_looking_at : access function
           (arg1 : access constant godot_transform;
            arg2 : access constant godot_vector3;
            arg3 : access constant godot_vector3) return godot_transform;  -- gdnative_api_struct.gen.h:693
      godot_transform_xform_plane : access function (arg1 : access constant godot_transform; arg2 : access constant godot_plane) return godot_plane;  -- gdnative_api_struct.gen.h:694
      godot_transform_xform_inv_plane : access function (arg1 : access constant godot_transform; arg2 : access constant godot_plane) return godot_plane;  -- gdnative_api_struct.gen.h:695
      godot_transform_new_identity : access procedure (arg1 : access godot_transform);  -- gdnative_api_struct.gen.h:696
      godot_transform_operator_equal : access function (arg1 : access constant godot_transform; arg2 : access constant godot_transform) return godot_bool;  -- gdnative_api_struct.gen.h:697
      godot_transform_operator_multiply : access function (arg1 : access constant godot_transform; arg2 : access constant godot_transform) return godot_transform;  -- gdnative_api_struct.gen.h:698
      godot_transform_xform_vector3 : access function (arg1 : access constant godot_transform; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:699
      godot_transform_xform_inv_vector3 : access function (arg1 : access constant godot_transform; arg2 : access constant godot_vector3) return godot_vector3;  -- gdnative_api_struct.gen.h:700
      godot_transform_xform_aabb : access function (arg1 : access constant godot_transform; arg2 : access constant godot_aabb) return godot_aabb;  -- gdnative_api_struct.gen.h:701
      godot_transform_xform_inv_aabb : access function (arg1 : access constant godot_transform; arg2 : access constant godot_aabb) return godot_aabb;  -- gdnative_api_struct.gen.h:702
      godot_transform2d_new : access procedure
           (arg1 : access godot_transform2d;
            arg2 : godot_real;
            arg3 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:703
      godot_transform2d_new_axis_origin : access procedure
           (arg1 : access godot_transform2d;
            arg2 : access constant godot_vector2;
            arg3 : access constant godot_vector2;
            arg4 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:704
      godot_transform2d_as_string : access function (arg1 : access constant godot_transform2d) return godot_string;  -- gdnative_api_struct.gen.h:705
      godot_transform2d_inverse : access function (arg1 : access constant godot_transform2d) return godot_transform2d;  -- gdnative_api_struct.gen.h:706
      godot_transform2d_affine_inverse : access function (arg1 : access constant godot_transform2d) return godot_transform2d;  -- gdnative_api_struct.gen.h:707
      godot_transform2d_get_rotation : access function (arg1 : access constant godot_transform2d) return godot_real;  -- gdnative_api_struct.gen.h:708
      godot_transform2d_get_origin : access function (arg1 : access constant godot_transform2d) return godot_vector2;  -- gdnative_api_struct.gen.h:709
      godot_transform2d_get_scale : access function (arg1 : access constant godot_transform2d) return godot_vector2;  -- gdnative_api_struct.gen.h:710
      godot_transform2d_orthonormalized : access function (arg1 : access constant godot_transform2d) return godot_transform2d;  -- gdnative_api_struct.gen.h:711
      godot_transform2d_rotated : access function (arg1 : access constant godot_transform2d; arg2 : godot_real) return godot_transform2d;  -- gdnative_api_struct.gen.h:712
      godot_transform2d_scaled : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_vector2) return godot_transform2d;  -- gdnative_api_struct.gen.h:713
      godot_transform2d_translated : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_vector2) return godot_transform2d;  -- gdnative_api_struct.gen.h:714
      godot_transform2d_xform_vector2 : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:715
      godot_transform2d_xform_inv_vector2 : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:716
      godot_transform2d_basis_xform_vector2 : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:717
      godot_transform2d_basis_xform_inv_vector2 : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_vector2) return godot_vector2;  -- gdnative_api_struct.gen.h:718
      godot_transform2d_interpolate_with : access function
           (arg1 : access constant godot_transform2d;
            arg2 : access constant godot_transform2d;
            arg3 : godot_real) return godot_transform2d;  -- gdnative_api_struct.gen.h:719
      godot_transform2d_operator_equal : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_transform2d) return godot_bool;  -- gdnative_api_struct.gen.h:720
      godot_transform2d_operator_multiply : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_transform2d) return godot_transform2d;  -- gdnative_api_struct.gen.h:721
      godot_transform2d_new_identity : access procedure (arg1 : access godot_transform2d);  -- gdnative_api_struct.gen.h:722
      godot_transform2d_xform_rect2 : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_rect2) return godot_rect2;  -- gdnative_api_struct.gen.h:723
      godot_transform2d_xform_inv_rect2 : access function (arg1 : access constant godot_transform2d; arg2 : access constant godot_rect2) return godot_rect2;  -- gdnative_api_struct.gen.h:724
      godot_variant_get_type : access function (arg1 : access constant godot_variant) return godot_variant_type;  -- gdnative_api_struct.gen.h:725
      godot_variant_new_copy : access procedure (arg1 : access godot_variant; arg2 : access constant godot_variant);  -- gdnative_api_struct.gen.h:726
      godot_variant_new_nil : access procedure (arg1 : access godot_variant);  -- gdnative_api_struct.gen.h:727
      godot_variant_new_bool : access procedure (arg1 : access godot_variant; arg2 : godot_bool);  -- gdnative_api_struct.gen.h:728
      godot_variant_new_uint : access procedure (arg1 : access godot_variant; arg2 : ICE.long_long);  -- gdnative_api_struct.gen.h:729
      godot_variant_new_int : access procedure (arg1 : access godot_variant; arg2 : IC.long);  -- gdnative_api_struct.gen.h:730
      godot_variant_new_real : access procedure (arg1 : access godot_variant; arg2 : IC.double);  -- gdnative_api_struct.gen.h:731
      godot_variant_new_string : access procedure (arg1 : access godot_variant; arg2 : access constant godot_string);  -- gdnative_api_struct.gen.h:732
      godot_variant_new_vector2 : access procedure (arg1 : access godot_variant; arg2 : access constant godot_vector2);  -- gdnative_api_struct.gen.h:733
      godot_variant_new_rect2 : access procedure (arg1 : access godot_variant; arg2 : access constant godot_rect2);  -- gdnative_api_struct.gen.h:734
      godot_variant_new_vector3 : access procedure (arg1 : access godot_variant; arg2 : access constant godot_vector3);  -- gdnative_api_struct.gen.h:735
      godot_variant_new_transform2d : access procedure (arg1 : access godot_variant; arg2 : access constant godot_transform2d);  -- gdnative_api_struct.gen.h:736
      godot_variant_new_plane : access procedure (arg1 : access godot_variant; arg2 : access constant godot_plane);  -- gdnative_api_struct.gen.h:737
      godot_variant_new_quat : access procedure (arg1 : access godot_variant; arg2 : access constant godot_quat);  -- gdnative_api_struct.gen.h:738
      godot_variant_new_aabb : access procedure (arg1 : access godot_variant; arg2 : access constant godot_aabb);  -- gdnative_api_struct.gen.h:739
      godot_variant_new_basis : access procedure (arg1 : access godot_variant; arg2 : access constant godot_basis);  -- gdnative_api_struct.gen.h:740
      godot_variant_new_transform : access procedure (arg1 : access godot_variant; arg2 : access constant godot_transform);  -- gdnative_api_struct.gen.h:741
      godot_variant_new_color : access procedure (arg1 : access godot_variant; arg2 : access constant godot_color);  -- gdnative_api_struct.gen.h:742
      godot_variant_new_node_path : access procedure (arg1 : access godot_variant; arg2 : access constant godot_node_path);  -- gdnative_api_struct.gen.h:743
      godot_variant_new_rid : access procedure (arg1 : access godot_variant; arg2 : access constant godot_rid);  -- gdnative_api_struct.gen.h:744
      godot_variant_new_object : access procedure (arg1 : access godot_variant; arg2 : System.Address);  -- gdnative_api_struct.gen.h:745
      godot_variant_new_dictionary : access procedure (arg1 : access godot_variant; arg2 : access constant godot_dictionary);  -- gdnative_api_struct.gen.h:746
      godot_variant_new_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_array);  -- gdnative_api_struct.gen.h:747
      godot_variant_new_pool_byte_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_pool_byte_array);  -- gdnative_api_struct.gen.h:748
      godot_variant_new_pool_int_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_pool_int_array);  -- gdnative_api_struct.gen.h:749
      godot_variant_new_pool_real_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_pool_real_array);  -- gdnative_api_struct.gen.h:750
      godot_variant_new_pool_string_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_pool_string_array);  -- gdnative_api_struct.gen.h:751
      godot_variant_new_pool_vector2_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_pool_vector2_array);  -- gdnative_api_struct.gen.h:752
      godot_variant_new_pool_vector3_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_pool_vector3_array);  -- gdnative_api_struct.gen.h:753
      godot_variant_new_pool_color_array : access procedure (arg1 : access godot_variant; arg2 : access constant godot_pool_color_array);  -- gdnative_api_struct.gen.h:754
      godot_variant_as_bool : access function (arg1 : access constant godot_variant) return godot_bool;  -- gdnative_api_struct.gen.h:755
      godot_variant_as_uint : access function (arg1 : access constant godot_variant) return ICE.long_long;  -- gdnative_api_struct.gen.h:756
      godot_variant_as_int : access function (arg1 : access constant godot_variant) return IC.long;  -- gdnative_api_struct.gen.h:757
      godot_variant_as_real : access function (arg1 : access constant godot_variant) return IC.double;  -- gdnative_api_struct.gen.h:758
      godot_variant_as_string : access function (arg1 : access constant godot_variant) return godot_string;  -- gdnative_api_struct.gen.h:759
      godot_variant_as_vector2 : access function (arg1 : access constant godot_variant) return godot_vector2;  -- gdnative_api_struct.gen.h:760
      godot_variant_as_rect2 : access function (arg1 : access constant godot_variant) return godot_rect2;  -- gdnative_api_struct.gen.h:761
      godot_variant_as_vector3 : access function (arg1 : access constant godot_variant) return godot_vector3;  -- gdnative_api_struct.gen.h:762
      godot_variant_as_transform2d : access function (arg1 : access constant godot_variant) return godot_transform2d;  -- gdnative_api_struct.gen.h:763
      godot_variant_as_plane : access function (arg1 : access constant godot_variant) return godot_plane;  -- gdnative_api_struct.gen.h:764
      godot_variant_as_quat : access function (arg1 : access constant godot_variant) return godot_quat;  -- gdnative_api_struct.gen.h:765
      godot_variant_as_aabb : access function (arg1 : access constant godot_variant) return godot_aabb;  -- gdnative_api_struct.gen.h:766
      godot_variant_as_basis : access function (arg1 : access constant godot_variant) return godot_basis;  -- gdnative_api_struct.gen.h:767
      godot_variant_as_transform : access function (arg1 : access constant godot_variant) return godot_transform;  -- gdnative_api_struct.gen.h:768
      godot_variant_as_color : access function (arg1 : access constant godot_variant) return godot_color;  -- gdnative_api_struct.gen.h:769
      godot_variant_as_node_path : access function (arg1 : access constant godot_variant) return godot_node_path;  -- gdnative_api_struct.gen.h:770
      godot_variant_as_rid : access function (arg1 : access constant godot_variant) return godot_rid;  -- gdnative_api_struct.gen.h:771
      godot_variant_as_object : access function (arg1 : access constant godot_variant) return System.Address;  -- gdnative_api_struct.gen.h:772
      godot_variant_as_dictionary : access function (arg1 : access constant godot_variant) return godot_dictionary;  -- gdnative_api_struct.gen.h:773
      godot_variant_as_array : access function (arg1 : access constant godot_variant) return godot_array;  -- gdnative_api_struct.gen.h:774
      godot_variant_as_pool_byte_array : access function (arg1 : access constant godot_variant) return godot_pool_byte_array;  -- gdnative_api_struct.gen.h:775
      godot_variant_as_pool_int_array : access function (arg1 : access constant godot_variant) return godot_pool_int_array;  -- gdnative_api_struct.gen.h:776
      godot_variant_as_pool_real_array : access function (arg1 : access constant godot_variant) return godot_pool_real_array;  -- gdnative_api_struct.gen.h:777
      godot_variant_as_pool_string_array : access function (arg1 : access constant godot_variant) return godot_pool_string_array;  -- gdnative_api_struct.gen.h:778
      godot_variant_as_pool_vector2_array : access function (arg1 : access constant godot_variant) return godot_pool_vector2_array;  -- gdnative_api_struct.gen.h:779
      godot_variant_as_pool_vector3_array : access function (arg1 : access constant godot_variant) return godot_pool_vector3_array;  -- gdnative_api_struct.gen.h:780
      godot_variant_as_pool_color_array : access function (arg1 : access constant godot_variant) return godot_pool_color_array;  -- gdnative_api_struct.gen.h:781
      godot_variant_call : access function
           (arg1 : access godot_variant;
            arg2 : access constant godot_string;
            arg3 : System.Address;
            arg4 : godot_int;
            arg5 : access godot_variant_call_error) return godot_variant;  -- gdnative_api_struct.gen.h:782
      godot_variant_has_method : access function (arg1 : access constant godot_variant; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:783
      godot_variant_operator_equal : access function (arg1 : access constant godot_variant; arg2 : access constant godot_variant) return godot_bool;  -- gdnative_api_struct.gen.h:784
      godot_variant_operator_less : access function (arg1 : access constant godot_variant; arg2 : access constant godot_variant) return godot_bool;  -- gdnative_api_struct.gen.h:785
      godot_variant_hash_compare : access function (arg1 : access constant godot_variant; arg2 : access constant godot_variant) return godot_bool;  -- gdnative_api_struct.gen.h:786
      godot_variant_booleanize : access function (arg1 : access constant godot_variant) return godot_bool;  -- gdnative_api_struct.gen.h:787
      godot_variant_destroy : access procedure (arg1 : access godot_variant);  -- gdnative_api_struct.gen.h:788
      godot_char_string_length : access function (arg1 : access constant godot_char_string) return godot_int;  -- gdnative_api_struct.gen.h:789
      godot_char_string_get_data : access function (arg1 : access constant godot_char_string) return ICS.chars_ptr;  -- gdnative_api_struct.gen.h:790
      godot_char_string_destroy : access procedure (arg1 : access godot_char_string);  -- gdnative_api_struct.gen.h:791
      godot_string_new : access procedure (arg1 : access godot_string);  -- gdnative_api_struct.gen.h:792
      godot_string_new_copy : access procedure (arg1 : access godot_string; arg2 : access constant godot_string);  -- gdnative_api_struct.gen.h:793
      godot_string_new_with_wide_string : access procedure
           (arg1 : access godot_string;
            arg2 : access IC.wchar_t;
            arg3 : IC.int);  -- gdnative_api_struct.gen.h:794
      godot_string_operator_index : access function (arg1 : access godot_string; arg2 : godot_int) return access IC.wchar_t;  -- gdnative_api_struct.gen.h:795
      godot_string_operator_index_const : access function (arg1 : access constant godot_string; arg2 : godot_int) return IC.wchar_t;  -- gdnative_api_struct.gen.h:796
      godot_string_wide_str : access function (arg1 : access constant godot_string) return WChar_T_Ptrs.Pointer;  -- gdnative_api_struct.gen.h:797
      godot_string_operator_equal : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:798
      godot_string_operator_less : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:799
      godot_string_operator_plus : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:800
      godot_string_length : access function (arg1 : access constant godot_string) return godot_int;  -- gdnative_api_struct.gen.h:801
      godot_string_casecmp_to : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return IC.signed_char;  -- gdnative_api_struct.gen.h:802
      godot_string_nocasecmp_to : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return IC.signed_char;  -- gdnative_api_struct.gen.h:803
      godot_string_naturalnocasecmp_to : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return IC.signed_char;  -- gdnative_api_struct.gen.h:804
      godot_string_begins_with : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:805
      godot_string_begins_with_char_array : access function (arg1 : access constant godot_string; arg2 : ICS.chars_ptr) return godot_bool;  -- gdnative_api_struct.gen.h:806
      godot_string_bigrams : access function (arg1 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:807
      godot_string_chr : access function (arg1 : IC.wchar_t) return godot_string;  -- gdnative_api_struct.gen.h:808
      godot_string_ends_with : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:809
      godot_string_find : access function (arg1 : access constant godot_string; arg2 : godot_string) return godot_int;  -- gdnative_api_struct.gen.h:810
      godot_string_find_from : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:811
      godot_string_findmk : access function (arg1 : access constant godot_string; arg2 : access constant godot_array) return godot_int;  -- gdnative_api_struct.gen.h:812
      godot_string_findmk_from : access function
           (arg1 : access constant godot_string;
            arg2 : access constant godot_array;
            arg3 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:813
      godot_string_findmk_from_in_place : access function
           (arg1 : access constant godot_string;
            arg2 : access constant godot_array;
            arg3 : godot_int;
            arg4 : access godot_int) return godot_int;  -- gdnative_api_struct.gen.h:814
      godot_string_findn : access function (arg1 : access constant godot_string; arg2 : godot_string) return godot_int;  -- gdnative_api_struct.gen.h:815
      godot_string_findn_from : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:816
      godot_string_find_last : access function (arg1 : access constant godot_string; arg2 : godot_string) return godot_int;  -- gdnative_api_struct.gen.h:817
      godot_string_format : access function (arg1 : access constant godot_string; arg2 : access constant godot_variant) return godot_string;  -- gdnative_api_struct.gen.h:818
      godot_string_format_with_custom_placeholder : access function
           (arg1 : access constant godot_string;
            arg2 : access constant godot_variant;
            arg3 : ICS.chars_ptr) return godot_string;  -- gdnative_api_struct.gen.h:819
      godot_string_hex_encode_buffer : access function (arg1 : access IC.unsigned_char; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:820
      godot_string_hex_to_int : access function (arg1 : access constant godot_string) return godot_int;  -- gdnative_api_struct.gen.h:821
      godot_string_hex_to_int_without_prefix : access function (arg1 : access constant godot_string) return godot_int;  -- gdnative_api_struct.gen.h:822
      godot_string_insert : access function
           (arg1 : access constant godot_string;
            arg2 : godot_int;
            arg3 : godot_string) return godot_string;  -- gdnative_api_struct.gen.h:823
      godot_string_is_numeric : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:824
      godot_string_is_subsequence_of : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:825
      godot_string_is_subsequence_ofi : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:826
      godot_string_lpad : access function (arg1 : access constant godot_string; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:827
      godot_string_lpad_with_custom_character : access function
           (arg1 : access constant godot_string;
            arg2 : godot_int;
            arg3 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:828
      godot_string_match : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:829
      godot_string_matchn : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:830
      godot_string_md5 : access function (arg1 : access IC.unsigned_char) return godot_string;  -- gdnative_api_struct.gen.h:831
      godot_string_num : access function (arg1 : IC.double) return godot_string;  -- gdnative_api_struct.gen.h:832
      godot_string_num_int64 : access function (arg1 : IC.long; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:833
      godot_string_num_int64_capitalized : access function
           (arg1 : IC.long;
            arg2 : godot_int;
            arg3 : godot_bool) return godot_string;  -- gdnative_api_struct.gen.h:834
      godot_string_num_real : access function (arg1 : IC.double) return godot_string;  -- gdnative_api_struct.gen.h:835
      godot_string_num_scientific : access function (arg1 : IC.double) return godot_string;  -- gdnative_api_struct.gen.h:836
      godot_string_num_with_decimals : access function (arg1 : IC.double; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:837
      godot_string_pad_decimals : access function (arg1 : access constant godot_string; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:838
      godot_string_pad_zeros : access function (arg1 : access constant godot_string; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:839
      godot_string_replace_first : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_string) return godot_string;  -- gdnative_api_struct.gen.h:840
      godot_string_replace : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_string) return godot_string;  -- gdnative_api_struct.gen.h:841
      godot_string_replacen : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_string) return godot_string;  -- gdnative_api_struct.gen.h:842
      godot_string_rfind : access function (arg1 : access constant godot_string; arg2 : godot_string) return godot_int;  -- gdnative_api_struct.gen.h:843
      godot_string_rfindn : access function (arg1 : access constant godot_string; arg2 : godot_string) return godot_int;  -- gdnative_api_struct.gen.h:844
      godot_string_rfind_from : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:845
      godot_string_rfindn_from : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:846
      godot_string_rpad : access function (arg1 : access constant godot_string; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:847
      godot_string_rpad_with_custom_character : access function
           (arg1 : access constant godot_string;
            arg2 : godot_int;
            arg3 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:848
      godot_string_similarity : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_real;  -- gdnative_api_struct.gen.h:849
      godot_string_sprintf : access function
           (arg1 : access constant godot_string;
            arg2 : access constant godot_array;
            arg3 : access godot_bool) return godot_string;  -- gdnative_api_struct.gen.h:850
      godot_string_substr : access function
           (arg1 : access constant godot_string;
            arg2 : godot_int;
            arg3 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:851
      godot_string_to_double : access function (arg1 : access constant godot_string) return IC.double;  -- gdnative_api_struct.gen.h:852
      godot_string_to_float : access function (arg1 : access constant godot_string) return godot_real;  -- gdnative_api_struct.gen.h:853
      godot_string_to_int : access function (arg1 : access constant godot_string) return godot_int;  -- gdnative_api_struct.gen.h:854
      godot_string_camelcase_to_underscore : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:855
      godot_string_camelcase_to_underscore_lowercased : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:856
      godot_string_capitalize : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:857
      godot_string_char_to_double : access function (arg1 : ICS.chars_ptr) return IC.double;  -- gdnative_api_struct.gen.h:858
      godot_string_char_to_int : access function (arg1 : ICS.chars_ptr) return godot_int;  -- gdnative_api_struct.gen.h:859
      godot_string_wchar_to_int : access function (arg1 : access IC.wchar_t) return IC.long;  -- gdnative_api_struct.gen.h:860
      godot_string_char_to_int_with_len : access function (arg1 : ICS.chars_ptr; arg2 : godot_int) return godot_int;  -- gdnative_api_struct.gen.h:861
      godot_string_char_to_int64_with_len : access function (arg1 : access IC.wchar_t; arg2 : IC.int) return IC.long;  -- gdnative_api_struct.gen.h:862
      godot_string_hex_to_int64 : access function (arg1 : access constant godot_string) return IC.long;  -- gdnative_api_struct.gen.h:863
      godot_string_hex_to_int64_with_prefix : access function (arg1 : access constant godot_string) return IC.long;  -- gdnative_api_struct.gen.h:864
      godot_string_to_int64 : access function (arg1 : access constant godot_string) return IC.long;  -- gdnative_api_struct.gen.h:865
      godot_string_unicode_char_to_double : access function (arg1 : access IC.wchar_t; arg2 : System.Address) return IC.double;  -- gdnative_api_struct.gen.h:866
      godot_string_get_slice_count : access function (arg1 : access constant godot_string; arg2 : godot_string) return godot_int;  -- gdnative_api_struct.gen.h:867
      godot_string_get_slice : access function
           (arg1 : access constant godot_string;
            arg2 : godot_string;
            arg3 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:868
      godot_string_get_slicec : access function
           (arg1 : access constant godot_string;
            arg2 : IC.wchar_t;
            arg3 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:869
      godot_string_split : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:870
      godot_string_split_allow_empty : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:871
      godot_string_split_floats : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:872
      godot_string_split_floats_allows_empty : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:873
      godot_string_split_floats_mk : access function (arg1 : access constant godot_string; arg2 : access constant godot_array) return godot_array;  -- gdnative_api_struct.gen.h:874
      godot_string_split_floats_mk_allows_empty : access function (arg1 : access constant godot_string; arg2 : access constant godot_array) return godot_array;  -- gdnative_api_struct.gen.h:875
      godot_string_split_ints : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:876
      godot_string_split_ints_allows_empty : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:877
      godot_string_split_ints_mk : access function (arg1 : access constant godot_string; arg2 : access constant godot_array) return godot_array;  -- gdnative_api_struct.gen.h:878
      godot_string_split_ints_mk_allows_empty : access function (arg1 : access constant godot_string; arg2 : access constant godot_array) return godot_array;  -- gdnative_api_struct.gen.h:879
      godot_string_split_spaces : access function (arg1 : access constant godot_string) return godot_array;  -- gdnative_api_struct.gen.h:880
      godot_string_char_lowercase : access function (arg1 : IC.wchar_t) return IC.wchar_t;  -- gdnative_api_struct.gen.h:881
      godot_string_char_uppercase : access function (arg1 : IC.wchar_t) return IC.wchar_t;  -- gdnative_api_struct.gen.h:882
      godot_string_to_lower : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:883
      godot_string_to_upper : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:884
      godot_string_get_basename : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:885
      godot_string_get_extension : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:886
      godot_string_left : access function (arg1 : access constant godot_string; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:887
      godot_string_ord_at : access function (arg1 : access constant godot_string; arg2 : godot_int) return IC.wchar_t;  -- gdnative_api_struct.gen.h:888
      godot_string_plus_file : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:889
      godot_string_right : access function (arg1 : access constant godot_string; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:890
      godot_string_strip_edges : access function
           (arg1 : access constant godot_string;
            arg2 : godot_bool;
            arg3 : godot_bool) return godot_string;  -- gdnative_api_struct.gen.h:891
      godot_string_strip_escapes : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:892
      godot_string_erase : access procedure
           (arg1 : access godot_string;
            arg2 : godot_int;
            arg3 : godot_int);  -- gdnative_api_struct.gen.h:893
      godot_string_ascii : access function (arg1 : access constant godot_string) return godot_char_string;  -- gdnative_api_struct.gen.h:894
      godot_string_ascii_extended : access function (arg1 : access constant godot_string) return godot_char_string;  -- gdnative_api_struct.gen.h:895
      godot_string_utf8 : access function (arg1 : access constant godot_string) return godot_char_string;  -- gdnative_api_struct.gen.h:896
      godot_string_parse_utf8 : access function (arg1 : access godot_string; arg2 : ICS.chars_ptr) return godot_bool;  -- gdnative_api_struct.gen.h:897
      godot_string_parse_utf8_with_len : access function
           (arg1 : access godot_string;
            arg2 : ICS.chars_ptr;
            arg3 : godot_int) return godot_bool;  -- gdnative_api_struct.gen.h:898
      godot_string_chars_to_utf8 : access function (arg1 : ICS.chars_ptr) return godot_string;  -- gdnative_api_struct.gen.h:899
      godot_string_chars_to_utf8_with_len : access function (arg1 : ICS.chars_ptr; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:900
      godot_string_hash : access function (arg1 : access constant godot_string) return IC.unsigned;  -- gdnative_api_struct.gen.h:901
      godot_string_hash64 : access function (arg1 : access constant godot_string) return ICE.long_long;  -- gdnative_api_struct.gen.h:902
      godot_string_hash_chars : access function (arg1 : ICS.chars_ptr) return IC.unsigned;  -- gdnative_api_struct.gen.h:903
      godot_string_hash_chars_with_len : access function (arg1 : ICS.chars_ptr; arg2 : godot_int) return IC.unsigned;  -- gdnative_api_struct.gen.h:904
      godot_string_hash_utf8_chars : access function (arg1 : access IC.wchar_t) return IC.unsigned;  -- gdnative_api_struct.gen.h:905
      godot_string_hash_utf8_chars_with_len : access function (arg1 : access IC.wchar_t; arg2 : godot_int) return IC.unsigned;  -- gdnative_api_struct.gen.h:906
      godot_string_md5_buffer : access function (arg1 : access constant godot_string) return godot_pool_byte_array;  -- gdnative_api_struct.gen.h:907
      godot_string_md5_text : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:908
      godot_string_sha256_buffer : access function (arg1 : access constant godot_string) return godot_pool_byte_array;  -- gdnative_api_struct.gen.h:909
      godot_string_sha256_text : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:910
      godot_string_empty : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:911
      godot_string_get_base_dir : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:912
      godot_string_get_file : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:913
      godot_string_humanize_size : access function (arg1 :  IC.size_t) return godot_string;  -- gdnative_api_struct.gen.h:914
      godot_string_is_abs_path : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:915
      godot_string_is_rel_path : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:916
      godot_string_is_resource_file : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:917
      godot_string_path_to : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:918
      godot_string_path_to_file : access function (arg1 : access constant godot_string; arg2 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:919
      godot_string_simplify_path : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:920
      godot_string_c_escape : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:921
      godot_string_c_escape_multiline : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:922
      godot_string_c_unescape : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:923
      godot_string_http_escape : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:924
      godot_string_http_unescape : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:925
      godot_string_json_escape : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:926
      godot_string_word_wrap : access function (arg1 : access constant godot_string; arg2 : godot_int) return godot_string;  -- gdnative_api_struct.gen.h:927
      godot_string_xml_escape : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:928
      godot_string_xml_escape_with_quotes : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:929
      godot_string_xml_unescape : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:930
      godot_string_percent_decode : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:931
      godot_string_percent_encode : access function (arg1 : access constant godot_string) return godot_string;  -- gdnative_api_struct.gen.h:932
      godot_string_is_valid_float : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:933
      godot_string_is_valid_hex_number : access function (arg1 : access constant godot_string; arg2 : godot_bool) return godot_bool;  -- gdnative_api_struct.gen.h:934
      godot_string_is_valid_html_color : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:935
      godot_string_is_valid_identifier : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:936
      godot_string_is_valid_integer : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:937
      godot_string_is_valid_ip_address : access function (arg1 : access constant godot_string) return godot_bool;  -- gdnative_api_struct.gen.h:938
      godot_string_destroy : access procedure (arg1 : access godot_string);  -- gdnative_api_struct.gen.h:939
      godot_string_name_new : access procedure (arg1 : access godot_string_name; arg2 : access constant godot_string);  -- gdnative_api_struct.gen.h:940
      godot_string_name_new_data : access procedure (arg1 : access godot_string_name; arg2 : ICS.chars_ptr);  -- gdnative_api_struct.gen.h:941
      godot_string_name_get_name : access function (arg1 : access constant godot_string_name) return godot_string;  -- gdnative_api_struct.gen.h:942
      godot_string_name_get_hash : access function (arg1 : access constant godot_string_name) return IC.unsigned;  -- gdnative_api_struct.gen.h:943
      godot_string_name_get_data_unique_pointer : access function (arg1 : access constant godot_string_name) return System.Address;  -- gdnative_api_struct.gen.h:944
      godot_string_name_operator_equal : access function (arg1 : access constant godot_string_name; arg2 : access constant godot_string_name) return godot_bool;  -- gdnative_api_struct.gen.h:945
      godot_string_name_operator_less : access function (arg1 : access constant godot_string_name; arg2 : access constant godot_string_name) return godot_bool;  -- gdnative_api_struct.gen.h:946
      godot_string_name_destroy : access procedure (arg1 : access godot_string_name);  -- gdnative_api_struct.gen.h:947
      godot_object_destroy : access procedure (arg1 : godot_object);  -- gdnative_api_struct.gen.h:948
      godot_global_get_singleton : access function (arg1 : ICS.chars_ptr) return godot_object;  -- gdnative_api_struct.gen.h:949
      godot_method_bind_get_method : access function (p_classname : ICS.chars_ptr; p_methodname : ICS.chars_ptr) return access godot_method_bind;  -- gdnative_api_struct.gen.h:950
      godot_method_bind_ptrcall : access procedure
           (p_method_bind : access godot_method_bind;
            p_instance : System.Address; -- godot_object * 
            p_args : System.Address; --const void **
            p_ret : System.Address); -- void * -- gdnative_api_struct.gen.h:951
      godot_method_bind_call : access function
           (p_method_bind : access godot_method_bind;
            p_instance : System.Address; -- godot_object *
            p_args : Godot_Instance_Method_Args_Ptrs.Pointer; -- godot_variant **
            p_arg_count : IC.int; -- p_arg_count
            p_call_error : access godot_variant_call_error) 
            return godot_variant;  -- gdnative_api_struct.gen.h:952
      godot_get_class_constructor : access function (arg1 : ICS.chars_ptr) return godot_class_constructor;  -- gdnative_api_struct.gen.h:953
      godot_get_global_constants : access function return godot_dictionary;  -- gdnative_api_struct.gen.h:954
      godot_register_native_call_type : access procedure (arg1 : ICS.chars_ptr; arg2 : native_call_cb);  -- gdnative_api_struct.gen.h:955
      godot_alloc : access function (arg1 : IC.int) return System.Address;  -- gdnative_api_struct.gen.h:956
      godot_realloc : access function (arg1 : System.Address; arg2 : IC.int) return System.Address;  -- gdnative_api_struct.gen.h:957
      godot_free : access procedure (arg1 : System.Address);  -- gdnative_api_struct.gen.h:958
      godot_print_error : godot_print_error_procedure;  -- gdnative_api_struct.gen.h:959
      godot_print_warning : godot_print_error_procedure;  -- gdnative_api_struct.gen.h:960
      godot_print : access procedure (p_message : access constant godot_string);  -- gdnative_api_struct.gen.h:961
   end record;
   pragma Convention (C_Pass_By_Copy, godot_gdnative_core_api_struct);  -- gdnative_api_struct.gen.h:212

-------
private
-------

   type byte_array is array (IC.unsigned range <>) of aliased IC.unsigned_char;

   -----------------
   -- method_bind --
   -----------------
   type godot_method_bind is record
      priv : aliased byte_array (1 .. 1);  -- ./gdnative/gdnative.h:219
   end record;
   pragma Convention (C_Pass_By_Copy, godot_method_bind);  -- ./gdnative/gdnative.h:220

   ----------
   -- aabb --
   ----------
   type godot_aabb is record
      priv : aliased byte_array (1 .. GODOT_AABB_SIZE);  -- ./gdnative/aabb.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_aabb);

   -----------
   -- array --
   -----------
   type godot_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/array.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_array);  -- ./gdnative/array.h:46

   -----------
   -- basis --
   -----------
   type godot_basis is record
      priv : aliased byte_array (1 .. GODOT_BASIS_SIZE);  -- ./gdnative/basis.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_basis);  -- ./gdnative/basis.h:46
      
   -----------
   -- color --
   -----------
   type godot_color is record
      priv : aliased byte_array (1 .. GODOT_COLOR_SIZE);  -- ./gdnative/color.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_color);  -- ./gdnative/color.h:46

   ----------------
   -- dictionary --
   ----------------
   type godot_dictionary is record
      priv : aliased byte_array (1 .. GODOT_DICTIONARY_SIZE);  -- ./gdnative/dictionary.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_dictionary);  -- ./gdnative/dictionary.h:46

   ---------------
   -- node_path --
   ---------------
   type godot_node_path is record
      priv : aliased byte_array (1 .. GODOT_NODE_PATH_SIZE);  -- ./gdnative/node_path.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_node_path);  -- ./gdnative/node_path.h:46

   -----------
   -- plane --
   -----------
   type godot_plane is record
      priv : aliased byte_array (1 .. GODOT_PLANE_SIZE);  -- ./gdnative/plane.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_plane);  -- ./gdnative/plane.h:46

   ---------------------
   -- pool_byte_array --
   ---------------------
   type godot_pool_byte_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/pool_arrays.h:79
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_byte_array);  -- ./gdnative/pool_arrays.h:80
   
   --------------------
   -- pool_int_array --
   --------------------
   type godot_pool_int_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/pool_arrays.h:90
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_int_array);  -- ./gdnative/pool_arrays.h:91
   
   ---------------------
   -- pool_real_array --
   ---------------------
   type godot_pool_real_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/pool_arrays.h:101
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_real_array);  -- ./gdnative/pool_arrays.h:102
   
   -----------------------
   -- pool_string_array --
   -----------------------
   type godot_pool_string_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/pool_arrays.h:112
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_string_array);  -- ./gdnative/pool_arrays.h:113
   
   ------------------------
   -- pool_vector2_array --
   ------------------------
   type godot_pool_vector2_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/pool_arrays.h:123
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_vector2_array);  -- ./gdnative/pool_arrays.h:124
   
   ------------------------
   -- pool_vector3_array --
   ------------------------
   type godot_pool_vector3_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/pool_arrays.h:134
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_vector3_array);  -- ./gdnative/pool_arrays.h:135
  
   ----------------------
   -- pool_color_array --
   ----------------------
   type godot_pool_color_array is record
      priv : aliased byte_array (1 .. GODOT_ARRAY_SIZE);  -- ./gdnative/pool_arrays.h:145
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_color_array);  -- ./gdnative/pool_arrays.h:146

   ----------
   -- quat --
   ----------
   type godot_quat is record
      priv : aliased byte_array (1 .. GODOT_QUAT_SIZE);  -- ./gdnative/quat.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_quat);  -- ./gdnative/quat.h:46

   -----------
   -- rect2 --
   -----------
   type godot_rect2 is record
      priv : aliased byte_array (1 .. GODOT_RECT_SIZE);  -- ./gdnative/rect2.h:43
   end record;
   pragma Convention (C_Pass_By_Copy, godot_rect2);  -- ./gdnative/rect2.h:42

   ---------
   -- rid --
   ---------
   type godot_rid is record
      priv : aliased byte_array (1 .. GODOT_RID_SIZE);  -- ./gdnative/rid.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_rid);  -- ./gdnative/rid.h:46

   ------------
   -- string --
   ------------
   type godot_string is record
      priv : aliased byte_array (1 .. GODOT_STRING_SIZE);  -- ./gdnative/string.h:49
   end record;
   pragma Convention (C_Pass_By_Copy, godot_string);  -- ./gdnative/string.h:50

   -----------------
   -- char_string --
   -----------------
   type godot_char_string is record
      priv : aliased byte_array (1 .. GODOT_CHAR_STRING_SIZE);  -- ./gdnative/string.h:57
   end record;
   pragma Convention (C_Pass_By_Copy, godot_char_string);  -- ./gdnative/string.h:58

   -----------------
   -- string_name --
   -----------------
   type godot_string_name is record
      priv : aliased byte_array (1 .. GODOT_STRING_NAME_SIZE);  -- ./gdnative/string_name.h:46
   end record;
   pragma Convention (C_Pass_By_Copy, godot_string_name);  -- ./gdnative/string_name.h:47

   -----------------
   -- transform2d --
   -----------------
   type godot_transform2d is record
      priv : aliased byte_array (1 .. GODOT_TRANSFORM2D_SIZE);  -- ./gdnative/transform2d.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_transform2d);  -- ./gdnative/transform2d.h:46

   ---------------
   -- transform --
   ---------------
   type godot_transform is record
      priv : aliased byte_array (1 .. GODOT_TRANSFORM_SIZE);  -- ./gdnative/transform.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_transform);  -- ./gdnative/transform.h:46

   -------------
   -- variant --
   -------------
   type godot_variant is record
      priv : aliased byte_array (1 .. GODOT_VARIANT_SIZE);  -- ./gdnative/variant.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_variant);  -- ./gdnative/variant.h:46

   -------------
   -- vector2 --
   -------------
   type godot_vector2 is record
      priv : aliased byte_array (1 .. GODOT_VECTOR2_SIZE);  -- ./gdnative/vector2.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_vector2);  -- ./gdnative/vector2.h:46

   -------------
   -- vector3 --
   -------------
   type godot_vector3 is record
      priv : aliased byte_array (1 .. GODOT_VECTOR3_SIZE);  -- ./gdnative/vector2.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_vector3);  -- ./gdnative/vector2.h:46

   ----------------------------
   -- pool_array_read_access --
   ----------------------------
   type godot_pool_array_read_access is record
      priv : aliased byte_array (1 .. GODOT_POOL_ARRAY_READ_ACCESS_SIZE);  -- ./gdnative/pool_arrays.h:45
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_array_read_access);  -- ./gdnative/pool_arrays.h:46

   -----------------------------
   -- pool_array_write_access --
   -----------------------------
   type godot_pool_array_write_access is record
      priv : aliased byte_array (1 .. GODOT_POOL_ARRAY_WRITE_ACCESS_SIZE);  -- ./gdnative/pool_arrays.h:61
   end record;
   pragma Convention (C_Pass_By_Copy, godot_pool_array_write_access);  -- ./gdnative/pool_arrays.h:62

end;
