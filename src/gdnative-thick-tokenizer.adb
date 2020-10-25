with Ada.Integer_Text_IO;  use Ada.Integer_Text_IO;

package body GDNative.Thick.Tokenizer is 
  
  ----------------
  -- Initialize --
  ----------------
  function Initialize (Input : String; Separators : Character_Array) return Tokenizer_State is begin
    return Tokenizer_State'(
      Input_Length      => Input'Length,
      Separators_Length => Separators'Length,
      Input             => Input,
      Separators        => Separators,
      Current           => Input (Input'First),
      Current_Offset    => 0,
      Token_Start       => 0,
      Token_End         => 0);
  end;

  ----------
  -- Next --
  ----------
  procedure Next (State : in out Tokenizer_State) is begin
    State.Current_Offset := State.Current_Offset + 1;
    State.Current := State.Input (State.Input'First + State.Current_Offset);
  end;

  -----------
  -- Start --
  -----------
  procedure Start (State : in out Tokenizer_State) is begin
    State.Token_Start := State.Input'First + State.Current_Offset;
  end;

  ----------
  -- Stop --
  ----------
  procedure Stop (State : in out Tokenizer_State) is begin
    State.Token_End := State.Input'First + State.Current_Offset;
  end;

  ----------
  -- Read --
  ----------
  function Read (State : in out Tokenizer_State) return String is begin
    return State.Input (State.Token_Start .. State.Token_End);
  end;

  -------------------------
  -- At Separator Or End --
  -------------------------
  function At_Separator_Or_End (State : Tokenizer_State) return Boolean is begin 
    return State.Current_Offset >= State.Input'length or
      (for some Separator of State.Separators => Separator = State.Current);
  end;

  ---------------------
  -- At Token Or End --
  ---------------------
  function At_Token_Or_End (State : Tokenizer_State) return Boolean is begin 
    return State.Current_Offset >= State.Input'length or
      (for all Separator of State.Separators => Separator /= State.Current);
  end;

  ---------------------
  -- Skip Until Next --
  ---------------------
  procedure Skip_Until_Next (State : in out Tokenizer_State) is begin
    loop
      exit when At_Token_Or_End (State);
      Next (State);
    end loop;
  end;

  ----------
  -- Skip --
  ----------
  procedure Skip (State : in out Tokenizer_State) is begin
    loop
      exit when At_Separator_Or_End (State);
      Next (State);
    end loop;
    Skip_Until_Next (State);
  end;

  -----------------
  -- Read String --
  -----------------
  function Read_String (State : in out Tokenizer_State) return String is begin
    Start (State);
    loop
      exit when At_Separator_Or_End (State);
      Next (State);
    end loop;
    Stop (State);
    Skip_Until_Next (State);
    return Read (State);
  end;

  ------------------
  -- Read Integer --
  ------------------
  function Read_Integer (State : in out Tokenizer_State) return Integer is 
    Token  : String := Read_String (State);
    Output : Integer;
    Last   : Integer;
  begin
    Get (Token, Output, Last);
    return Output;
  end;

end;