with Concorde.Money;
with Concorde.Quantities;

with Accord.Db;

package Concorde.Employment is

   procedure Create_Employment_Contract
     (Employer : Accord.Agent.Agent_Class;
      Employee : Accord.Agent.Agent_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Salary   : Concorde.Money.Price_Type);

end Concorde.Employment;
