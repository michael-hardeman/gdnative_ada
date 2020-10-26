with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Exceptions;
with Ada.Exceptions.Traceback;
with Ada.Finalization;

with GNAT.Traceback;
with GNAT.Traceback.Symbolic;

with GDNative.Thick.Tokenizer;

package body GDNative.Thick.Exceptions is

  package IC  renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;
  package AE  renames Ada.Exceptions;
  package AF  renames Ada.Finalization;
  package GT  renames GNAT.Traceback;
  package GTS renames GNAT.Traceback.Symbolic;

  type Error_Report is new AF.Controlled with record
    Subprogram  : ICS.chars_ptr;
    File        : ICS.chars_ptr;
    Line        : IC.int;
  end record;

  ---------------------------
  -- Error_Report Finalize --
  ---------------------------
  overriding procedure Finalize (Object : in out Error_Report) is begin
    ICS.Free (Object.Subprogram);
    ICS.Free (Object.File);
  end;

  -----------
  -- Parse --
  -----------
  -- Will extract stack trace info from a GNAT symbolic stack trace output.
  --
  -- Example Input:
  --
  -- [C:\project\libproject.dll]
  -- test_stack_trace.call_stack at test_stack_trace.adb:10
  -- test_stack_trace.inner at test_stack_trace.adb:16
  -- test_stack_trace.middle at test_stack_trace.adb:21
  -- test_stack_trace.outer at test_stack_trace.adb:26
  -- _ada_test_stack_trace at test_stack_trace.adb:30
  procedure Parse (Report : in out Error_Report) is
    use Tokenizer;
    Traces       : GT.Tracebacks_Array := GT.Call_Chain (Max_Len => 8, Skip_Frames => 3);
    Input        : String              := GTS.Symbolic_Traceback (Traces);
    Seps         : Character_Array     := (' ', ':', ASCII.LF, ASCII.CR);
    State        : Tokenizer_State     := Initialize (Input, Seps);
  begin
    Skip_Line (State); -- [path/lib<project.library_name>.dll]
    Report.Subprogram  := ICS.New_String (Read_String (State));
    Skip (State);      -- at
    Report.File        := ICS.New_String (Read_String (State));
    Report.Line        := IC.int         (Read_Integer (State));
  end;

  -----------------
  -- Put Warning --
  -----------------
  procedure Put_Warning (Message : in Wide_String) is
    Description : ICS.chars_ptr := ICS.New_String (To_Str (Message));
    Report      : Error_Report;
  begin
    Parse (Report);
    Core_Api.godot_print_warning (Description, Report.Subprogram, Report.File, Report.Line);
    ICS.Free (Description);
  end;

  ---------------
  -- Put Error --
  ---------------
  procedure Put_Error (Occurrence : in Ada.Exceptions.Exception_Occurrence) is
    Description : ICS.chars_ptr := ICS.New_String (AE.Exception_Information (Occurrence));
    Report      : Error_Report;
  begin
    Parse (Report);
    Core_Api.godot_print_error (Description, Report.Subprogram, Report.File, Report.Line);
    ICS.Free (Description);
  end;

end;