with Ada.Characters.Latin_1;

with Concorde.Calendar;
with Concorde.Identifiers;
with Concorde.Logging;

with Accord.Historical_Account;

package body Concorde.Agents is

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Agent : Accord.Agent.Agent_Class;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String)
   is
   begin
      Add_Cash (Agent.Account, Cash, Tag);
   end Add_Cash;

   --------------
   -- Add_Cash --
   --------------

   procedure Add_Cash
     (Account : Accord.Account.Account_Class;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String)
   is
      use type Concorde.Money.Money_Type;
      Old_Cash : constant Concorde.Money.Money_Type := Account.Cash;
      New_Cash : constant Concorde.Money.Money_Type := Old_Cash + Cash;
   begin
      Account.Update_Account
        .Set_Cash (New_Cash)
        .Set_Earn (Account.Earn + Cash)
        .Done;
      Accord.Historical_Account.Create
        (Account    => Account,
         Time_Stamp => Concorde.Calendar.Clock,
         Tag        => Tag,
         Change     => Cash,
         Cash       => New_Cash);
   end Add_Cash;

   ----------
   -- Cash --
   ----------

   function Cash
     (Agent : Accord.Agent.Agent_Class)
      return Concorde.Money.Money_Type
   is
   begin
      return Agent.Account.Cash;
   end Cash;

   --------------
   -- Describe --
   --------------

   function Describe
     (Agent : Accord.Agent.Agent_Class)
      return String
   is
   begin
      return "Agent " & Agent.Identifier;
   end Describe;

   ----------------
   -- Limit_Cash --
   ----------------

   function Limit_Cash
     (Account : Accord.Account.Account_Class)
      return Concorde.Money.Money_Type
   is
      use Concorde.Money;
   begin
      if Account.Guarantor.Has_Element then
         return Account.Cash + Limit_Cash (Account.Guarantor);
      else
         return Account.Cash;
      end if;
   end Limit_Cash;

   ---------------
   -- Log_Agent --
   ---------------

   procedure Log_Agent
     (Agent   : Accord.Agent.Agent_Class;
      Context : String;
      Message : String)
   is
   begin
      Concorde.Logging.Log
        (Describe (Agent),
         Context & Ada.Characters.Latin_1.HT & Message);
   end Log_Agent;

   -----------------
   -- New_Account --
   -----------------

   function New_Account
     (Starting_Balance : Concorde.Money.Money_Type;
      Guarantor        : Accord.Account.Account_Class :=
        Accord.Account.Empty_Handle)
      return Accord.Account.Account_Handle
   is
   begin
      return Accord.Account.Create
        (Identifier => Concorde.Identifiers.Next_Identifier,
         Guarantor  => Guarantor,
         Start_Cash => Starting_Balance,
         Cash       => Starting_Balance,
         Earn       => Concorde.Money.Zero,
         Spend      => Concorde.Money.Zero);
   end New_Account;

   -----------------
   -- Remove_Cash --
   -----------------

   procedure Remove_Cash
     (Agent : Accord.Agent.Agent_Class;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String)
   is
   begin
      Remove_Cash (Agent.Account, Cash, Tag);
   end Remove_Cash;

   -----------------
   -- Remove_Cash --
   -----------------

   procedure Remove_Cash
     (Account : Accord.Account.Account_Class;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String)
   is
      use type Concorde.Money.Money_Type;
      Guarantor : constant Accord.Account.Account_Class :=
                    Account.Guarantor;
      Old_Cash : constant Concorde.Money.Money_Type := Account.Cash;
      New_Cash : constant Concorde.Money.Money_Type := Old_Cash - Cash;
   begin
      if New_Cash < Concorde.Money.Zero
        and then Guarantor.Has_Element
      then
         Accord.Account.Update_Account (Account)
           .Set_Cash (Concorde.Money.Zero)
           .Set_Spend (Account.Spend + Cash)
           .Done;
         Accord.Historical_Account.Create
           (Account    => Account,
            Time_Stamp => Concorde.Calendar.Clock,
            Tag        => Tag,
            Change     => Concorde.Money.Zero - Cash,
            Cash       => Concorde.Money.Zero);
         Remove_Cash (Guarantor, Cash,
                      "xfer acct " & Account.Identifier);
      else
         Account.Update_Account
           .Set_Cash (New_Cash)
           .Set_Spend (Account.Spend + Cash)
           .Done;
         Accord.Historical_Account.Create
           (Account    => Account,
            Time_Stamp => Concorde.Calendar.Clock,
            Tag        => Tag,
            Change     => Concorde.Money.Zero - Cash,
            Cash       => New_Cash);
      end if;
   end Remove_Cash;

end Concorde.Agents;
