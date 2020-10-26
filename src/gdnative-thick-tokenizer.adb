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
  -- At End Or Indicator --
  -------------------------
  function At_End_Or_Indicator (State : Tokenizer_State; Indicators : Character_Array) return Boolean is begin
    return State.Current_Offset >= State.Input'length or
      (for some Indicator of Indicators => Indicator = State.Current);
  end;

  -----------------------------
  -- At End Or Not Indicator --
  -----------------------------
  function At_End_Or_Not_Indicator (State : Tokenizer_State; Indicators : Character_Array) return Boolean is begin
    return State.Current_Offset >= State.Input'length or
      (for all Indicator of Indicators => Indicator /= State.Current);
  end;

  ----------------
  -- Skip Until --
  ----------------
  procedure Skip_Until (State : in out Tokenizer_State; Indicators : Character_Array) is begin
    loop
      exit when At_End_Or_Indicator (State, Indicators);
      Next (State);
    end loop;
  end;

  --------------------
  -- Skip Until Not --
  --------------------
  procedure Skip_Until_Not (State : in out Tokenizer_State; Indicators : Character_Array) is begin
    loop
      exit when At_End_Or_Not_Indicator (State, Indicators);
      Next (State);
    end loop;
  end;

  ---------------
  -- Skip Line --
  ---------------
  procedure Skip_Line (State : in out Tokenizer_State) is begin
    Skip_Until     (State, (ASCII.CR, ASCII.LF)); 
    Skip_Until_Not (State, (ASCII.CR, ASCII.LF));
  end;

  ----------
  -- Skip --
  ----------
  procedure Skip (State : in out Tokenizer_State) is begin
    Skip_Until     (State, State.Separators);
    Skip_Until_Not (State, State.Separators);
  end;

  -----------------
  -- Read String --
  -----------------
  function Read_String (State : in out Tokenizer_State) return String is begin
    Start (State);
    loop
      exit when At_End_Or_Indicator (State, State.Separators);
      Next (State);
    end loop;
    Stop (State);
    Skip_Until_Not (State, State.Separators);
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