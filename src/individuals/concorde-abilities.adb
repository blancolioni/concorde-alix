with WL.Random;

package body Concorde.Abilities is

   Score_Modifiers : constant array (0 .. 15) of Integer :=
                       (0        => -3,
                        1 .. 2   => -2,
                        3 .. 5   => -1,
                        6 .. 8   => 0,
                        9 .. 11  => 1,
                        12 .. 14 => 2,
                        15       => 3);

   function Check_Ability
     (Score      : Natural;
      Difficulty : Positive)
      return Check_Result
   is
      Roll : constant Positive := WL.Random.Random_Number (1, 6)
               + WL.Random.Random_Number (1, 6);
      Total : constant Integer := Roll + Check_Modifier (Score);
   begin
      return Check_Result'
        (Roll       => Roll,
         Score      => Score,
         Modifier   => Check_Modifier (Score),
         Difficulty => Difficulty,
         Success    => Roll > 2 and then Total >= Difficulty);
   end Check_Ability;

   --------------------
   -- Check_Modifier --
   --------------------

   function Check_Modifier (Score : Natural) return Integer is
   begin
      return Score_Modifiers (Natural'Min (Score, Score_Modifiers'Last));
   end Check_Modifier;

   -----------------
   -- Check_Skill --
   -----------------

   function Check_Skill
     (Level      : Natural;
      Difficulty : Positive)
      return Check_Result
   is
      Roll : constant Positive := WL.Random.Random_Number (1, 6)
               + WL.Random.Random_Number (1, 6);
      Total : constant Integer := Roll + Level;
   begin
      return Check_Result'
        (Roll       => Roll,
         Score      => Level,
         Modifier   => Level,
         Difficulty => Difficulty,
         Success    => Roll > 2 and then Total >= Difficulty);
   end Check_Skill;

   ----------
   -- Show --
   ----------

   function Show
     (Result : Check_Result)
      return String
   is
   begin
      return "check: score" & Result.Score'Image
        & "; modifier" & Result.Modifier'Image
        & "; difficulty" & Result.Difficulty'Image
        & "; roll" & Result.Roll'Image
        & "; result "
        & (if Result.Success then "success" else "failure");
   end Show;

end Concorde.Abilities;
