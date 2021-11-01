with Concorde.Money;

with Accord.Account;
with Accord.Agent;

package Concorde.Agents is

   function Cash
     (Agent : Accord.Agent.Agent_Class)
      return Concorde.Money.Money_Type;

   procedure Add_Cash
     (Agent : Accord.Agent.Agent_Class;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String);

   procedure Add_Cash
     (Account : Accord.Account.Account_Class;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String);

   procedure Remove_Cash
     (Agent : Accord.Agent.Agent_Class;
      Cash  : Concorde.Money.Money_Type;
      Tag   : String);

   procedure Remove_Cash
     (Account : Accord.Account.Account_Class;
      Cash    : Concorde.Money.Money_Type;
      Tag     : String);

   function Limit_Cash
     (Account : Accord.Account.Account_Class)
      return Concorde.Money.Money_Type;

   function New_Account
     (Starting_Balance : Concorde.Money.Money_Type;
      Guarantor        : Accord.Account.Account_Class :=
        Accord.Account.Empty_Handle)
      return Accord.Account.Account_Handle;

   function Describe
     (Agent : Accord.Agent.Agent_Class)
      return String;

   procedure Log_Agent
     (Agent   : Accord.Agent.Agent_Class;
      Context : String;
      Message : String);

end Concorde.Agents;
