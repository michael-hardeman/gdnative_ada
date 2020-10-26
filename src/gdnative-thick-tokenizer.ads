package GDNative.Thick.Tokenizer is

  type Character_Array is array (Positive range <>) of Character;

  type Tokenizer_State (<>) is private;

  function Initialize (Input : String; Separators : Character_Array) return Tokenizer_State;

  procedure Skip           (State : in out Tokenizer_State);
  procedure Skip_Until     (State : in out Tokenizer_State; Indicators : Character_Array);
  procedure Skip_Until_Not (State : in out Tokenizer_State; Indicators : Character_Array);
  procedure Skip_Line      (State : in out Tokenizer_State);
  function  Read_String    (State : in out Tokenizer_State) return String;
  function  Read_Integer   (State : in out Tokenizer_State) return Integer;

-------
private
-------

  type Tokenizer_State (
    Input_Length      : Positive;
    Separators_Length : Positive) 
  is record
    Input          : String          (1 .. Input_Length);
    Separators     : Character_Array (1 .. Separators_Length);
    Current        : Character;
    Current_Offset : Natural;
    Token_Start    : Natural;
    Token_End      : Natural;
  end record;

  procedure Next  (State : in out Tokenizer_State);
  procedure Start (State : in out Tokenizer_State);
  procedure Stop  (State : in out Tokenizer_State);
  function  Read  (State : in out Tokenizer_State) return String;

end;