package Concorde.Abilities is

   type Check_Result is
      record
         Roll       : Positive;
         Score      : Natural;
         Modifier   : Integer;
         Difficulty : Positive;
         Success    : Boolean;
      end record;

   function Show
     (Result : Check_Result)
     return String;

   function Check_Modifier (Score : Natural) return Integer;

   function Check_Ability
     (Score      : Natural;
      Difficulty : Positive)
      return Check_Result;

   function Check_Skill
     (Level      : Natural;
      Difficulty : Positive)
      return Check_Result;

end Concorde.Abilities;
