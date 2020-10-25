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
  -- 00417D7F in ?? at cygming-crtend.c:0
  -- 00401A61 in test_stack_trace.call_stack at test_stack_trace.adb:10
  -- 00401A25 in test_stack_trace.inner at test_stack_trace.adb:16
  -- 00401A0C in test_stack_trace.middle at test_stack_trace.adb:21
  -- 0040193E in test_stack_trace.outer at test_stack_trace.adb:26
  -- 004018A2 in _ada_test_stack_trace at test_stack_trace.adb:30
  -- 004016BE in main at b~test_stack_trace.adb:183
  -- 00401235 in ?? at cygming-crtend.c:0
  -- 00401286 in ?? at cygming-crtend.c:0
  -- 7C817075 in ?? at ??:0
  procedure Parse (Report : in out Error_Report) is
    use Tokenizer;
    Traces       : GT.Tracebacks_Array := GT.Call_Chain (Max_Len => 10, Skip_Frames => 2);
    Input        : String              := GTS.Symbolic_Traceback (Traces);
    Seps         : Character_Array     := (' ', ':'); 
    State        : Tokenizer_State     := Initialize (Input, Seps);
  begin
    Skip (State);     -- Hex
    Skip (State);     -- in
    Report.Subprogram := ICS.New_String (Read_String (State));
    Skip (State);     -- at
    Report.File       := ICS.New_String (Read_String (State));
    Report.Line       := IC.int         (Read_Integer (State));
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

  -------------------------
  -- Last_Chance_Handler --
  -------------------------
  procedure Last_Chance_Handler (Occurrence : Ada.Exceptions.Exception_Occurrence) is
    procedure Unhandled_Terminate 
      with No_Return, Import, Convention => C, External_Name => "__gnat_unhandled_terminate";
  begin
    begin
      Put_Error (Occurrence); -- Process the exception here.
    exception
      when others => null;
    end;
    Unhandled_Terminate;
  end;

end;